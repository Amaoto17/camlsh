open Printf
open Unix

open Util


module Env = struct
  type t =
    { vars : (string, string) Hashtbl.t
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
    ; mutable redir_chain : (unit -> unit)
    ; mutable stack : string Stack.t
    ; vars : vars
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
    ; redir_chain = (fun () -> ())
    ; stack = Stack.create ()
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

  let add_redir t thunk =
    let rest = t.redir_chain in
    t.redir_chain <- (fun () -> rest (); thunk ())

  let reset_redir t =
    t.redir_chain <- (fun () -> ())

  let redirect t =
    t.redir_chain ()

  let safe_redirect t =
    let saved_stdout = dup stdout in
    redirect t;
    fun () ->
      dup2 saved_stdout stdout;
      close saved_stdout

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
  Ctx.new_env ctx;
  Ctx.set_local ctx "var" "abc"


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
      Ctx.set_local ctx args.(0) args.(1)

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
        begin match fork () with
        | 0 ->
            Ctx.redirect ctx;
            Ctx.reset_redir ctx;
            fetch ctx & pc + 2
        | _ ->
            fetch ctx & pc + 1
        end

    | Inst.Builtin com ->
        let args = Ctx.pop_all ctx in
        let clean = Ctx.safe_redirect ctx in
        exec_builtin ctx args com;
        clean ();
        fetch ctx & pc + 1

    | Inst.End ->
        fetch ctx & pc + 1

    | Inst.Exec ->
        let args = Ctx.pop_all ctx in
        begin match fork () with
        | 0 ->
            Ctx.redirect ctx;
            execvp args.(0) args
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
            let thunk = fun () ->
              dup2 write stdout;
              close write
            in
            Ctx.add_redir ctx thunk;
            fetch ctx & pc + 2
        | _ ->
            close write;
            let thunk = fun () ->
              dup2 read stdin;
              close read
            in
            Ctx.add_redir ctx thunk;
            fetch ctx & pc + 1
        end

    | Inst.Push s ->
        Ctx.push ctx s;
        fetch ctx & pc + 1

    | Inst.Stdout ->
        let path = Ctx.pop ctx in
        let thunk = fun () ->
          let dst = openfile path [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
          dup2 dst stdout;
          close dst
        in
        Ctx.add_redir ctx thunk;
        fetch ctx & pc + 1

    | Inst.Var ->
        let name = Ctx.pop ctx in
        let res =
          match Ctx.find ctx name with
          | None -> ""
          | Some s -> s
        in
        Ctx.push ctx res;
        fetch ctx & pc + 1

    | Inst.Wait ->
        wait_child ctx;
        fetch ctx & pc + 1
  in

  fetch ctx 0