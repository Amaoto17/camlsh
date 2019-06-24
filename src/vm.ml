open Printf
open Unix
open Re.Glob

open Util

exception Interruption
exception Exec_failure

let interrupt _ = raise Interruption

let signal_init () =
  Sys.set_signal Sys.sigint & Signal_handle interrupt;
  Sys.set_signal Sys.sigquit Signal_ignore;
  Sys.set_signal Sys.sigtstp Signal_ignore


let status_string = function
  | WEXITED n -> !% "exited %d" n
  | WSIGNALED n -> !% "signaled %d" n
  | WSTOPPED n -> !% "stopped %d" n

let status_num = function
  | WEXITED n -> n
  | WSIGNALED n | WSTOPPED n -> (abs n) + 128

let wait_child ctx pid =
  let (pid, status) = waitpid [] pid in
  let res = !% "pid: %d, status: %s" pid (status_string status) in
  eprintf "%s\n%!" (res |> Deco.colorize Green);
  status_num status |> Ctx.set_status ctx


(* wildcard expansion *)

let can_expand s =
  let is_wildcard = function
    | '*' | '?' | '[' -> true
    | _ -> false
  in
  let len = String.length s in
  let rec loop i =
    if i >= len then false
    else if s.[i] = '\\' then loop (i + 2)
    else if is_wildcard s.[i] then true
    else loop (i + 1)
  in
  loop 0

let split_path path =
  let len = String.length path in
  let sep =
    match String.rindex_opt path '/' with
    | None -> -1
    | Some n -> n
  in
  let dirname =
    if sep = 0 then "/"
    else if sep = -1 then ""
    else String.sub path 0 sep
  in
  let basename = String.sub path (sep + 1) (len - sep - 1) in
  (dirname, basename)

let concat_path dirname basename =
  let dir_len = String.length dirname in
  let base_len = String.length dirname in
  let buf = Buffer.create (dir_len + base_len + 1) in
  if dirname <> "" then begin
    if dirname = "/" then
      Buffer.add_char buf '/'
    else begin
      Buffer.add_string buf dirname;
      Buffer.add_char buf '/'
    end
  end;
  Buffer.add_string buf basename;
  Buffer.contents buf

let glob_dir target dir_only dirname =
  let re = glob target |> Re.compile in
  try
    let dir_handler =
      opendir (if dirname = "" then getcwd () else dirname)
    in
    let rec loop acc =
        try
        let filename = readdir dir_handler in
        let fullpath = concat_path dirname filename in
        if filename = "." || filename = ".." then loop acc
        else if filename.[0] = '.' && target.[0] != '.' then loop acc
        else if dir_only && Sys.file_exists fullpath && not (Sys.is_directory fullpath) then loop acc
        else if Re.execp re filename then loop (fullpath :: acc)
        else loop acc
        with End_of_file -> acc
    in
    let res = loop [] in
    closedir dir_handler;
    res
  with
    | Unix_error (_, _, _) -> []

let rec expand_glob dir_only path =
  let (dirname, basename) = split_path path in
  let dirs =
    if can_expand dirname then
      if path <> dirname then expand_glob true dirname
      else glob_dir "" true dirname
    else [dirname]
  in
  if basename = "" then dirs |> List.map (fun dir -> concat_path dir "/")
  else
    dirs
      |> List.map (fun dir -> glob_dir basename dir_only dir)
      |> List.concat
      |> List.fast_sort String.compare

let exec_external ctx argv =
  Ctx.do_redirection ctx;
  Sys.set_signal Sys.sigint Signal_default;
  Sys.set_signal Sys.sigquit Signal_default;
  try
    execvp argv.(0) argv
  with
    | Unix_error(Unix.ENOENT, _, name) ->
        eprintf "camlsh: unknown command %s\n%!" name;
        exit 127
    | _ ->
        exit 127

let open_file path flags perm =
  try
    openfile path flags perm
  with _ ->
    eprintf "%s: No such file or directory\n%!" path;
    raise Exec_failure

let execute ctx code =
  let rec fetch ctx pc =
    try
      let inst = code.(pc) in
      let s = !% "fetched: [%02d] %s" pc (Inst.show inst) in
      eprintf "%s\n%!" (s |> Deco.colorize Gray);
      let s = Ctx.show ctx in
      eprintf "%s\n%!" (s |> Deco.colorize Gray);
      exec ctx pc inst
    with Invalid_argument _ ->
      failwith "invalild address"
  
  and exec ctx pc = function
    | Inst.Add_string s ->
        Ctx.add_string ctx s;
        fetch ctx & pc + 1

    | Inst.Array_ref (st, ed) ->
        let arr = Ctx.take_string ctx |> Array.of_list in
        let len = Array.length arr in
        (* translate negative number *)
        let st = if st < 0 then len + st else st - 1 in
        let ed = if ed < 0 then len + ed else ed - 1 in
        (* swap *)
        let (st, ed, swapped) = if ed < st then (ed, st, true) else (st, ed, false) in
        (* result is empty when index is out of range *)
        if st >= len || ed < 0 then Ctx.add_empty ctx
        else begin
          (* truncate *)
          let st = max st 0 in
          let ed = min ed (len - 1) in
          (* slice *)
          let sliced = Array.sub arr st (ed - st + 1) |> Array.to_list in
          let sliced = if swapped then List.rev sliced else sliced in
          Ctx.add_string_list ctx sliced
        end;
        fetch ctx & pc + 1

    | Inst.Begin ->
        Ctx.new_env ctx;
        Ctx.safe_redirection ctx;
        fetch ctx & pc + 1

    | Inst.Builtin ->
        let argv = Ctx.pop_all ctx in
        Ctx.safe_redirection ctx;
        Builtin.exec ctx argv;
        Ctx.restore ctx;
        Ctx.reset_redir ctx;
        fetch ctx & pc + 1

    | Inst.Brace ->
        Ctx.push_frame ctx;
        fetch ctx & pc + 1

    | Inst.Brace_end ->
        let ss = Ctx.concat_string ctx in
        Ctx.pop_frame ctx;
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

    | Inst.Concat_array ->
        let ss = Ctx.take_string ctx in
        Ctx.add_string ctx (String.concat " " ss);
        fetch ctx & pc + 1

    | Inst.Continue ->
        begin match Ctx.loop_start ctx with
        | None ->
            eprintf "'continue' while not inside of loop\n%!";
            fetch ctx & pc + 1
        | Some st ->
            Ctx.delete_env_all ctx;
            fetch ctx st
        end

    | Inst.Emit_string ->
        let ss = Ctx.emit_string ctx in
        List.iter (Ctx.push ctx) ss;
        fetch ctx & pc + 1

    | Inst.End ->
        Ctx.restore ctx;
        Ctx.reset_redir ctx;
        Ctx.delete_env ctx;
        fetch ctx & pc + 1

    | Inst.Exec ->
        let argv = Ctx.pop_all ctx in
        Sys.set_signal Sys.sigint Signal_ignore;
        begin match fork () with
        | 0 ->
            exec_external ctx argv
        | pid ->
            wait_child ctx pid;
            Sys.set_signal Sys.sigint & Signal_handle interrupt;
            Ctx.reset_redir ctx;
            fetch ctx & pc + 1
        end

    | Inst.Exec_nofork ->
        let argv = Ctx.pop_all ctx in
        exec_external ctx argv

    | Inst.Exit ->
        let status = Ctx.get_status ctx in
        exit status

    | Inst.For (st, ed) ->
        let values = Ctx.pop_all ctx |> Array.to_list in
        Ctx.push_frame ~iteration:(st, ed, values) ctx;
        Ctx.safe_redirection ctx;
        fetch ctx & pc + 1

    | Inst.For_iter ->
        let name = Ctx.pop ctx in
        if name = "" then fetch ctx & pc + 1
        else
          let value = Ctx.take_iter_val ctx in
          begin match value with
          | None ->
              fetch ctx & pc + 1
          | Some v ->
              Ctx.set_local ctx name v;
              fetch ctx & pc + 2
          end

    | Inst.Glob ->
        let ss = Ctx.emit_string ctx in
        let res =
          try
            List.map (expand_glob false) ss |> List.concat
          with Parse_error ->
            eprintf "invalid wildcard '%s'\n%!" (String.concat "" ss);
            []
        in
        Ctx.add_string_list ctx res;
        fetch ctx & pc + 1

    | Inst.If ->
        let status = Ctx.get_status ctx in
        if status = 0 then fetch ctx & pc + 2
        else fetch ctx & pc + 1

    | Inst.Jump dst ->
        fetch ctx dst

    | Inst.Leave ->
        Ctx.reset_redir ctx
    
    | Inst.Loop_end ->
        Ctx.restore ctx;
        Ctx.reset_redir ctx;
        Ctx.pop_frame ctx;
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

    | Inst.Pipe_err ->
        let (read, write) = pipe () in
        begin match fork () with
        | 0 ->
            close read;
            Ctx.set_stderr ctx write;
            fetch ctx & pc + 2
        | _ ->
            close write;
            Ctx.set_stdin ctx read;
            fetch ctx & pc + 1
        end

    | Inst.Pipe_open ->
        begin match fork () with
        | 0 ->
            fetch ctx & pc + 2
        | pid ->
            wait_child ctx pid;
            fetch ctx & pc + 1
        end

    | Inst.Push s ->
        Ctx.push ctx s;
        fetch ctx & pc + 1

    | Inst.Stderr ->
        let path = Ctx.pop ctx in
        let dst = open_file path [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        Ctx.set_stderr ctx dst;
        fetch ctx & pc + 1

    | Inst.Stderr_append ->
        let path = Ctx.pop ctx in
        let dst = open_file path [O_WRONLY; O_APPEND] 0o644 in
        Ctx.set_stderr ctx dst;
        fetch ctx & pc + 1

    | Inst.Stdin ->
        let path = Ctx.pop ctx in
        let src = open_file path [O_RDONLY] 0 in
        Ctx.set_stdin ctx src;
        fetch ctx & pc + 1

    | Inst.Stdout ->
        let path = Ctx.pop ctx in
        let dst = open_file path [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        Ctx.set_stdout ctx dst;
        fetch ctx & pc + 1

    | Inst.Stdout_append ->
        let path = Ctx.pop ctx in
        let dst = open_file path [O_WRONLY; O_APPEND] 0o644 in
        Ctx.set_stdout ctx dst;
        fetch ctx & pc + 1

    | Inst.Subst ->
        let (read, write) = pipe () in
        begin match fork () with
        | 0 ->
            close read;
            Ctx.set_stdout ctx write;
            Ctx.push_frame ctx;
            fetch ctx & pc + 2
        | pid ->
            close write;
            wait_child ctx pid;
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
        if status = 0 then fetch ctx & pc + 1
        else fetch ctx & pc + 2

    | Inst.Var ->
        let name = Ctx.pop ctx in
        begin match Ctx.find ctx name with
        | None -> Ctx.add_empty ctx
        | Some arr -> arr |> Array.to_list |> Ctx.add_string_list ctx
        end;
        fetch ctx & pc + 1

    | Inst.While (st, ed) ->
        Ctx.push_frame ~iteration:(st, ed, []) ctx;
        Ctx.safe_redirection ctx;
        fetch ctx & pc + 1
  in

  fetch ctx 0