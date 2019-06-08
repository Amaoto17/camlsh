open Printf
open Unix

open Util


module Context = struct
  type t =
    { mutable stdin : file_descr option
    ; mutable stdout : file_descr option
    }

  let create () =
    { stdin = None
    ; stdout = None
    }
end


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


let execute code =
  let stack = Stack.create () in

  let rec fetch ctx pc =
    try
      let inst = code.(pc) in
      exec ctx pc inst
    with Invalid_argument _ -> ()

  and exec ctx pc = function
    | Inst.Exec ->
        let args =
          Stack.fold (fun xs x -> x :: xs) [] stack
            |> Array.of_list
        in
        begin match fork () with
        | 0 ->
            redirect ctx;
            execvp args.(0) args
        | _ ->
            revert ctx;
            let (pid, status) = wait () in
            let res = !% "pid: %d, status: %s" pid (status_string status) in
            eprintf "%s\n%!" (res |> Deco.colorize `Green);
            let ctx = Context.create () in
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
        let res = !% "pid: %d, status: %s" pid (status_string status) in
        eprintf "%s\n%!" (res |> Deco.colorize `Green);
        fetch ctx & pc + 1
  in
  
  let ctx = Context.create () in
  fetch ctx 0