open Printf
open Unix

open Util


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

let exec_builtin ctx argv =
  let argc = Array.length argv in
  let status =
    match argv.(0) with
    | "cd" ->
        Builtin.cd argc argv
    | "echo" ->
        Builtin.echo argc argv
    | "false" ->
        1
    | "set" ->
        Builtin.set ctx argc argv
    | "true" ->
        0
    | com ->
        eprintf "unknown builtin %S" com;
        1
  in
  Ctx.set_status ctx status

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
        exec_builtin ctx argv;
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