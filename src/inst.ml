open Util


type t =
  | And
  | Builtin of string
  | End
  | Exec
  | Exit
  | Jump of int
  | Nop
  | Stdout
  | Or
  | Pipe
  | Push of string
  | Wait

let show = function
  | And -> "and"
  | Builtin op -> !% "builtin %s" op
  | End -> "end"
  | Exec -> "exec"
  | Exit -> "exit"
  | Jump pos -> !% "jump -> %d" pos
  | Nop -> "nop"
  | Or -> "or"
  | Pipe -> "pipe"
  | Stdout -> "stdout"
  | Push s -> !% "push %S" s
  | Wait -> "wait"