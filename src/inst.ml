open Util


type t =
  | Exec
  | Exit
  | Jump of int
  | Nop
  | Stdout
  | Pipe
  | Push of string
  | Wait

let show = function
  | Exec -> "exec"
  | Exit -> "exit"
  | Jump pos -> !% "jump -> %d" pos
  | Nop -> "nop"
  | Pipe -> "pipe"
  | Stdout -> "stdout"
  | Push s -> !% "push %S" s
  | Wait -> "wait"