open Printf
open Unix

open Util


module Ctx = struct
  type t =
    { mutable status : int
    ; mutable redir_chain : (unit -> unit)
    ; mutable redir_len : int
    ; mutable stack : string Stack.t
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
    let s = Buffer.contents buf in
    !% "redir: %d\n%s" t.redir_len s

  let create () =
    { status = 0
    ; redir_chain = (fun () -> ())
    ; redir_len = 0
    ; stack = Stack.create ()
    }

  (* TODO: ctx.status should be implemented as shell variable. *)

  let get_status t = t.status

  let set_status t status = t.status <- status

  (* redirection *)

  let add_redir t thunk =
    let rest = t.redir_chain in
    t.redir_chain <- (fun () -> rest (); thunk ());
    let len = t.redir_len in
    t.redir_len <- len + 1

  let reset_redir t =
    t.redir_chain <- (fun () -> ());
    t.redir_len <- 0

  let redirect t =
    t.redir_chain ();
    t.redir_len <- 0

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

  | "true" ->
      Ctx.set_status ctx 0

  | com -> failwith (!% "invalid builtin command %S" com)

let execute code =
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
        | 0 -> fetch ctx & pc + 2
        | _ -> fetch ctx & pc + 1
        end

    | Inst.Builtin com ->
        let args = Ctx.pop_all ctx in
        let clean = Ctx.safe_redirect ctx in
        exec_builtin ctx args com;
        clean ();
        fetch ctx & pc + 1

    | Inst.End ->
        wait_child ctx;
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

    | Inst.Wait ->
        wait_child ctx;
        fetch ctx & pc + 1
  in

  let ctx = Ctx.create () in
  fetch ctx 0


(* module Context = struct
  type t =
    { mutable stdin : file_descr option
    ; mutable stdout : file_descr option
    ; mutable status : int
    }

  let create () =
    { stdin = None
    ; stdout = None
    ; status = 0
    }
end

open Context


let status_string = function
  | WEXITED n -> !% "exited %d" n
  | WSIGNALED n -> !% "signaled %d" n
  | WSTOPPED n -> !% "stopped %d" n


let set_stdin (ctx : Context.t) src =
  begin match ctx.stdin with
  | None -> ()
  | Some src' -> close src'
  end;
  ctx.stdin <- Some src

let set_stdout (ctx : Context.t) dst =
  begin match ctx.stdout with
  | None -> ()
  | Some dst' -> close dst'
  end;
  ctx.stdout <- Some dst


let revert (ctx : Context.t) =
  begin match ctx.stdin with
  | None -> ()
  | Some src -> close src
  end;

  begin match ctx.stdout with
  | None -> ()
  | Some dst -> close dst
  end

let redirect (ctx : Context.t) =
  begin match ctx.stdin with
  | None -> ()
  | Some src -> dup2 src stdin
  end;

  begin match ctx.stdout with
  | None -> ()
  | Some dst -> dup2 dst stdout
  end;

  revert ctx

let redir_bt (ctx : Context.t) =
  begin match ctx.stdout with
  | None ->
      fun () -> ()
  | Some dst ->
      let saved_stdout = dup stdout in
      dup2 dst stdout;
      close dst;
      fun () ->
        dup2 saved_stdout stdout;
        close saved_stdout;
  end


let pop_all stack =
  let rec loop acc =
    if Stack.is_empty stack then acc
    else
      let elem = Stack.pop stack in
      loop (elem :: acc)
  in
  loop [] |> Array.of_list


let dump_stack stack =
  eprintf "%s" ("  Stack:" |> Deco.colorize `Gray);
  Stack.iter (fun s -> s |> !% " (%s)" |> Deco.colorize `Gray |> eprintf "%s") stack;
  eprintf "\n%!"


let exec_builtin ctx args = function
  | "cd" ->
      let len = Array.length args in
      let dst =
        if len = 0 then getenv "HOME"
        else if len = 1 then args.(0)
        else failwith "cd: too many arguments"
      in
      chdir dst

  | "echo" ->
      let s = args |> Array.to_list |> String.concat " " in
      printf "%s\n%!" s

  | "false" ->
      ctx.status <- 1

  | "true" ->
      ctx.status <- 0

  | _ -> failwith "invalid builtin command"


let execute code =
  let stack = Stack.create () in

  let rec fetch ctx pc =
    try
      let s = !% "fetch [%02d]" pc in
      eprintf "%s\n%!" (s |> Deco.colorize `Gray);
      dump_stack stack;
      let inst = code.(pc) in
      exec ctx pc inst

    with Invalid_argument _ ->
      failwith "invalid address"


  and exec ctx pc = function
    | Inst.And ->
        let status = ctx.status in
        if status = 0 then fetch ctx & pc + 2
        else fetch ctx & pc + 1

    | Inst.Block ->
        begin match fork () with
        | 0 -> fetch ctx & pc + 2
        | _ -> fetch ctx & pc + 1
        end

    | Inst.Builtin op ->
        let args = pop_all stack in
        let closer = redir_bt ctx in
        exec_builtin ctx args op;
        closer ();
        fetch ctx & pc + 1

    | Inst.Exec ->
        let args = pop_all stack in
        begin match fork () with
        | 0 ->
            redirect ctx;
            execvp args.(0) args
        | _ ->
            revert ctx;
            let (pid, status) = wait () in
            let res = !% "(exec) pid: %d, status: %s" pid (status_string status) in
            eprintf "%s\n%!" (res |> Deco.colorize `Green);
            let n =
              match status with
              | WEXITED n -> n
              | WSIGNALED n -> n + 128
              | WSTOPPED n -> n + 128
            in
            ctx.status <- n;
            fetch ctx & pc + 1
        end

    | Inst.End ->
        let (pid, status) = wait () in
        let res = !% "(end) pid: %d, status: %s" pid (status_string status) in
        eprintf "%s\n%!" (res |> Deco.colorize `Green);
        fetch ctx & pc + 1

    | Inst.Exit ->
        exit 0

    | Inst.Jump dst ->
        fetch ctx dst

    | Inst.Leave ->
        ()

    | Inst.Nop ->
        fetch ctx & pc + 1

    | Inst.Stdout ->
        let path = Stack.pop stack in
        let dst = openfile path [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        set_stdout ctx dst;
        fetch ctx & pc + 1

    | Inst.Or ->
        let status = ctx.status in
        if status = 0 then fetch ctx & pc + 1
        else fetch ctx & pc + 2

    | Inst.Pipe ->
        let (read, write) = pipe () in
        begin match fork () with
        | 0 ->
            set_stdout ctx write;
            close read;
            fetch ctx & pc + 2
        | _ ->
            set_stdin ctx read;
            close write;
            fetch ctx & pc + 1
        end

    | Inst.Push s ->
        Stack.push s stack;
        fetch ctx & pc + 1

    | Inst.Wait ->
        let (pid, status) = wait () in
        let res = !% "(wait) pid: %d, status: %s" pid (status_string status) in
        eprintf "%s\n%!" (res |> Deco.colorize `Green);
        fetch ctx & pc + 1
  in
  
  let ctx = Context.create () in
  fetch ctx 0 *)