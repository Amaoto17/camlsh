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
    | Inst.Add_string s ->
        Ctx.add_string ctx s;
        fetch ctx & pc + 1

    | Inst.Begin ->
        Ctx.new_env ctx;
        fetch ctx & pc + 1

    | Inst.Builtin ->
        let argv = Ctx.pop_all ctx in
        Ctx.safe_redirection ctx;
        exec_builtin ctx argv;
        fetch ctx & pc + 1

    | Inst.Brace ->
        Ctx.push_buf ctx;
        fetch ctx & pc + 1

    | Inst.Brace_end ->
        let ss = Ctx.concat_string ctx in
        Ctx.pop_buf ctx;
        Ctx.add_string_list ctx ss;
        fetch ctx & pc + 1

    | Inst.Break ->
        begin match Ctx.loop_end ctx with
        | None ->
            eprintf "'break' while not inside of loop\n%!";
            fetch ctx & pc + 1
        | Some ed ->
            fetch ctx ed
        end

    | Inst.Continue ->
        begin match Ctx.loop_start ctx with
        | None ->
            eprintf "'continue' while not inside of loop\n%!";
            fetch ctx & pc + 1
        | Some st ->
            fetch ctx st
        end

    | Inst.Emit_string ->
        let ss = Ctx.emit_string ctx in
        List.iter (Ctx.push ctx) ss;
        fetch ctx & pc + 1

    | Inst.End ->
        Ctx.delete_env ctx;
        fetch ctx & pc + 1

    | Inst.Exec ->
        let argv = Ctx.pop_all ctx in
        begin match fork () with
        | 0 ->
            Sys.set_signal Sys.sigint Signal_default;
            begin
              try
                Ctx.do_redirection ctx;
                execvp argv.(0) argv
              with
                | Unix_error(Unix.ENOENT, _, name) ->
                    eprintf "camlsh: unknown command %s\n%!" name;
                    exit 127
                | _ ->
                    exit 127
            end
        | _ ->
            wait_child ctx;
            fetch ctx & pc + 1
        end

    | Inst.Exit ->
        exit 0

    | Inst.If ->
        let status = Ctx.get_status ctx in
        if status = "0" then fetch ctx & pc + 2
        else fetch ctx & pc + 1

    | Inst.Jump dst ->
        fetch ctx dst

    | Inst.Leave ->
        Ctx.reset_redir ctx
    
    | Inst.Loop_end ->
        Ctx.exit_loop ctx;
        fetch ctx & pc + 1

    | Inst.Nop ->
        fetch ctx & pc + 1

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

    | Inst.Return ->
        Ctx.return ctx;
        fetch ctx & pc + 1

    | Inst.Stderr ->
        let path = Ctx.pop ctx in
        let dst = openfile path [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        Ctx.set_stderr ctx dst;
        fetch ctx & pc + 1

    | Inst.Stderr_append ->
        let path = Ctx.pop ctx in
        let dst = openfile path [O_WRONLY; O_APPEND] 0o644 in
        Ctx.set_stderr ctx dst;
        fetch ctx & pc + 1

    | Inst.Stdin ->
        let path = Ctx.pop ctx in
        let src = openfile path [O_RDONLY] 0 in
        Ctx.set_stdin ctx src;
        fetch ctx & pc + 1

    | Inst.Stdout ->
        let path = Ctx.pop ctx in
        let dst = openfile path [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        Ctx.set_stdout ctx dst;
        fetch ctx & pc + 1

    | Inst.Stdout_append ->
        let path = Ctx.pop ctx in
        let dst = openfile path [O_WRONLY; O_APPEND] 0o644 in
        Ctx.set_stdout ctx dst;
        fetch ctx & pc + 1

    | Inst.Subst ->
        let (read, write) = pipe () in
        begin match fork () with
        | 0 ->
            close read;
            Ctx.set_stdout ctx write;
            Ctx.clear_stack ctx;
            Ctx.clear_buf ctx;
            fetch ctx & pc + 2
        | _ ->
            close write;
            wait_child ctx;
            let in_ch = in_channel_of_descr read in
            let rec loop acc =
              try (input_line in_ch) :: acc |> loop
              with End_of_file -> List.rev acc
            in
            Ctx.add_string_list ctx (loop []);
            close_in in_ch;
            fetch ctx & pc + 1
        end

    | Inst.Unless ->
        let status = Ctx.get_status ctx in
        if status = "0" then fetch ctx & pc + 1
        else fetch ctx & pc + 2

    | Inst.Var ->
        let name = Ctx.pop ctx in
        begin match Ctx.find ctx name with
        | None -> Ctx.add_empty ctx
        | Some arr -> arr |> Array.to_list |> Ctx.add_string_list ctx
        end;
        fetch ctx & pc + 1

    | Inst.Wait ->
        wait_child ctx;
        fetch ctx & pc + 1

    | Inst.While (st, ed) ->
        Ctx.begin_loop ctx st ed;
        fetch ctx & pc + 1
  in

  fetch ctx 0