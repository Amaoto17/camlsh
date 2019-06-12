open Util


type t =
  | And
  | Block
  | Builtin of string
  | End
  | Exec
  | Exit
  | Jump of int
  | Leave
  | Nop
  | Stdout
  | Or
  | Pipe
  | Push of string
  | Var
  | Wait

let show = function
  | And -> "and"
  | Block -> "block"
  | Builtin op -> !% "builtin %s" op
  | End -> "end"
  | Exec -> "exec"
  | Exit -> "exit"
  | Jump pos -> !% "jump -> %02d" pos
  | Leave -> "leave"
  | Nop -> "nop"
  | Or -> "or"
  | Pipe -> "pipe"
  | Stdout -> "stdout"
  | Push s -> !% "push %S" s
  | Var -> "var"
  | Wait -> "wait"