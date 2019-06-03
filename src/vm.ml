open Printf
open Unix

external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (!%) = Printf.sprintf

let status_string = function
  | WEXITED n -> !% "exited %d" n
  | WSIGNALED n -> !% "signaled %d" n
  | WSTOPPED n -> !% "stopped %d" n


let execute code =
  let stack = Stack.create () in
  let rec fetch i =
    try
      let inst = code.(i) in
      exec i inst
    with Invalid_argument _ -> ()
  and exec i = function
    | Inst.Exec ->
        let args =
          Stack.fold (fun xs x -> x :: xs) [] stack
            |> Array.of_list
        in
        execvp args.(0) args

    | Inst.Exit ->
        exit 0

    | Inst.Fork ->
        ( match fork () with
          | 0 -> fetch (i + 2)
          | _ -> fetch (i + 1)
        )

    | Inst.Jump offset ->
        fetch & i + offset

    | Inst.Stdout ->
        let dst = Stack.pop stack in
        let fd = openfile dst [O_WRONLY; O_CREAT; O_TRUNC] 0o644 in
        dup2 fd stdout;
        close fd;
        fetch (i + 1)

    | Inst.Pipe ->
        let (fd_in, fd_out) = pipe () in
        ( match fork () with
          | 0 ->
              dup2 fd_out stdout;
              close fd_out;
              close fd_in;
              fetch & i + 2
          | _ ->
              dup2 fd_in stdin;
              close fd_in;
              close fd_out;
              fetch & i + 1
        )

    | Inst.Pipeopen ->
        let (fd_in, fd_out) = pipe () in
        ( match fork () with
          | 0 ->
              fetch & i + 2
          | _ ->
              close fd_in;
              close fd_out;
              fetch & i + 1
        )

    | Inst.Push s ->
        Stack.push s stack;
        fetch (i + 1)

    | Inst.Wait ->
        let (pid, status) = wait () in
        let res = !% "pid: %d, status: %s" pid (status_string status) in
        eprintf "\x1b[32m%s\x1b[m\n%!" res;
        fetch (i + 1)
  in
  fetch 0