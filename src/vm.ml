open Printf
open Unix

let (!%) = Printf.sprintf

let status_string = function
  | WEXITED n -> !% "exited %d" n
  | WSIGNALED n -> !% "signaled %d" n
  | WSTOPPED n -> !% "stopped %d" n


let exec_external args =
  match fork () with
  | 0 ->
      execvp args.(0) args
  | pid ->
      let (pid, status) = wait () in
      let res = !% "pid: %d, status: %s" pid (status_string status) in
      printf "\x1b[32m%s\x1b[m\n" res

let execute insts =
  let stack = Stack.create () in
  let exec_inst = function
    | Inst.Exec ->
        let args = Stack.fold (fun xs x -> x :: xs) [] stack in
        exec_external (Array.of_list args)
    | Inst.Push s ->
        Stack.push s stack
  in
  List.iter exec_inst insts