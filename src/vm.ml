open Printf
open Unix

open Util


module Context = struct
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

    | Inst.Builtin op ->
        let args = pop_all stack in
        let closer = redir_bt ctx in
        exec_builtin ctx args op;
        closer ();
        fetch ctx & pc + 1

    | Inst.End ->
        ()

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

    | Inst.Exit ->
        exit 0

    | Inst.Jump dst ->
        fetch ctx dst

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
  fetch ctx 0