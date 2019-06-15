open Printf
open Unix

open Util


module Env = struct
  type t =
    { vars : (string, string array) Hashtbl.t
    ; outer : t option
    }

  let create () =
    { vars = Hashtbl.create 64
    ; outer = None
    }

  let new_env t =
    let new_env = create () in
    { new_env with outer = Some t }

  let delete_env t =
    match t.outer with
    | None -> failwith "no outer environment"
    | Some env -> env

  let rec find t key =
    match Hashtbl.find_opt t.vars key with
    | Some v -> Some v
    | None ->
        match t.outer with
        | None -> None
        | Some env -> find env key

  let set t key v =
    match find t key with
    | None -> Hashtbl.add t.vars key v
    | Some _ -> Hashtbl.replace t.vars key v
end


module Ctx = struct
  type t =
    { mutable status : int
    ; mutable stack : string Stack.t
    ; mutable redir : redir
    ; vars : vars
    }

  and redir =
    { mutable input : file_descr option
    ; mutable output : file_descr option
    }

  and vars =
    { builtin : Env.t
    ; global : Env.t
    ; mutable local : Env.t
    }

  let show t =
    let buf = Buffer.create 64 in
    Buffer.add_string buf "|";
    Stack.iter
      ( fun s ->
          Buffer.add_string buf " ";
          Buffer.add_string buf s;
          Buffer.add_string buf " |"
      )
      t.stack;
    Buffer.contents buf

  let create () =
    { status = 0
    ; stack = Stack.create ()
    ; redir =
        { input = None
        ; output = None
        }
    ; vars =
        { builtin = Env.create ()
        ; global = Env.create ()
        ; local = Env.create ()
        }
    }

  (* TODO: ctx.status should be implemented as shell variable. *)

  let get_status t = t.status

  let set_status t status = t.status <- status

  (* redirection *)

  let dup2_close fd1 fd2 =
    dup2 fd1 fd2;
    close fd1

  let get_input t = t.redir.input

  let get_output t = t.redir.output

  let safe_close = function
    | None -> ()
    | Some fd -> try close fd with Unix_error(_, _, _) -> ()

  let set_stdin t input =
    get_input t |> safe_close;
    t.redir.input <- Some input

  let set_stdout t output =
    get_output t |> safe_close;
    t.redir.output <- Some output

  let do_redirection t =
    begin match get_input t with
    | None -> ()
    | Some fd -> dup2_close fd stdin
    end;
    begin match get_output t with
    | None -> ()
    | Some fd -> dup2_close fd stdout
    end

  let safe_redirection t =
    let stdin' = dup stdin in
    let stdout' = dup stdout in
    let redir = t.redir in
    do_redirection t;
    let thunk () =
      dup2_close stdin' stdin;
      dup2_close stdout' stdout;
      t.redir <- redir
    in
    thunk

  let reset_redir t =
    get_input t |> safe_close;
    get_output t |> safe_close;
    t.redir.input <- None;
    t.redir.output <- None

  (* stack operation *)

  let push t s = Stack.push s t.stack

  let pop t = Stack.pop t.stack

  let pop_all t =
    let rec loop acc =
      if Stack.is_empty t.stack then acc
      else
        let elem = Stack.pop t.stack in
        elem :: acc |> loop
    in
    loop [] |> Array.of_list

  (* handling variables *)

  let new_env t =
    let local = t.vars.local in
    t.vars.local <- Env.new_env local

  let delete_env t =
    let local = t.vars.local in
    t.vars.local <- Env.delete_env local

  let find t key =
    match Env.find t.vars.local key with
    | Some v -> Some v
    | None ->
        match Env.find t.vars.global key with
        | Some v -> Some v
        | None ->
            match Env.find t.vars.builtin key with
            | Some v -> Some v
            | None -> None

  let set_local t = Env.set t.vars.local
end


let wait_child ctx =
  let status_string = function
    | WEXITED n -> !% "exited %d" n
    | WSIGNALED n -> !% "signaled %d" n
    | WSTOPPED n -> !% "stopped %d" n
  in
  let status_num = function
    | WEXITED n -> n
    | WSIGNALED n | WSTOPPED n -> n + 128
  in
  let (pid, status) = wait () in
  let res = !% "pid: %d, status: %s" pid (status_string status) in
  eprintf "%s\n%!" (res |> Deco.colorize `Green);
  status_num status |> Ctx.set_status ctx


let init ctx =
  Ctx.new_env ctx

let exec_builtin ctx args = function
  | "cd" ->
      let argc = Array.length args in
      let dst =
        if argc = 0 then getenv "HOME"
        else if argc = 1 then args.(0)
        else failwith "cd: too many arguments"
      in
      chdir dst
  
  | "echo" ->
      let s = args |> Array.to_list |> String.concat " " in
      printf "%s\n%!" s

  | "false" ->
      Ctx.set_status ctx 1

  | "set" ->
      let argc = Array.length args in
      if argc = 0 then ()
      else
        let v = Array.sub args 1 (argc - 1) in
        Ctx.set_local ctx args.(0) v

  | "true" ->
      Ctx.set_status ctx 0

  | com -> failwith (!% "invalid builtin command %S" com)

let execute ctx code =
  let rec fetch ctx pc =
    try
      let inst = code.(pc) in
      let s = !% "fetched: [%02d] %s" pc (Inst.show inst) in
      eprintf "%s\n%!" (s |> Deco.colorize `Gray);
      let s = Ctx.show ctx in
      eprintf "%s\n%!" (s |> Deco.colorize `Gray);
      exec ctx pc inst
    with Invalid_argument _ ->
      failwith "invalild address"
  
  and exec ctx pc = function
    | Inst.And ->
        let status = Ctx.get_status ctx in
        if status = 0 then fetch ctx & pc + 2
        else fetch ctx & pc + 1

    | Inst.Block ->
        let return = Ctx.safe_redirection ctx in
        Ctx.reset_redir ctx;
        Ctx.new_env ctx;
        fetch ctx & pc + 2;
        Ctx.delete_env ctx;
        return ();
        fetch ctx & pc + 1

    | Inst.Builtin com ->
        let argv = Ctx.pop_all ctx in
        let return = Ctx.safe_redirection ctx in
        exec_builtin ctx argv com;
        return ();
        fetch ctx & pc + 1

    | Inst.Exec ->
        let argv = Ctx.pop_all ctx in
        begin match fork () with
        | 0 ->
            Ctx.do_redirection ctx;
            execvp argv.(0) argv
        | _ ->
            wait_child ctx;
            fetch ctx & pc + 1
        end

    | Inst.Exit ->
        exit 0

    | Inst.Jump dst ->
        fetch ctx dst

    | Inst.Leave ->
        Ctx.reset_redir ctx

    | Inst.Nop ->
        fetch ctx & pc + 1

    | Inst.Or ->
        let status = Ctx.get_status ctx in
        if status = 0 then fetch ctx & pc + 1
        else fetch ctx & pc + 2

    | Inst.Pipe ->
        let (read, write) = pipe () in
        begin match fork () with
        | 0 ->
            close read;
            Ctx.set_stdout ctx write;
            fetch ctx & pc + 2
        | _ ->
            close write;
            Ctx.set_stdin ctx read;
            fetch ctx & pc + 1
        end

    | Inst.Push s ->
        Ctx.push ctx s;
        fetch ctx & pc + 1

    | Inst.Stdout ->
        let path = Ctx.pop ctx in
        let dst = openfile path [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        Ctx.set_stdout ctx dst;
        fetch ctx & pc + 1

    | Inst.Var ->
        let name = Ctx.pop ctx in
        begin match Ctx.find ctx name with
        | None -> Ctx.push ctx ""
        | Some arr -> Array.iter (Ctx.push ctx) arr
        end;
        fetch ctx & pc + 1

    | Inst.Wait ->
        wait_child ctx;
        fetch ctx & pc + 1

    | Inst.While ->
        let return = Ctx.safe_redirection ctx in
        Ctx.new_env ctx;
        fetch ctx & pc + 2;
        Ctx.delete_env ctx;
        return ();
        fetch ctx & pc + 1
  in

  fetch ctx 0