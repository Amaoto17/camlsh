open Util


type t =
  | And
  | Begin
  | Break
  | Builtin of string
  | Continue
  | End
  | Exec
  | Exit
  | Jump of int
  | Leave
  | Loop_end
  | Nop
  | Stdout
  | Or
  | Pipe
  | Push of string
  | Var
  | Wait
  | While of int * int

let show = function
  | And -> "and"
  | Begin -> "begin"
  | Break -> "break"
  | Builtin op -> !% "builtin %s" op
  | Continue -> "continue"
  | End -> "end"
  | Exec -> "exec"
  | Exit -> "exit"
  | Jump pos -> !% "jump -> %02d" pos
  | Leave -> "leave"
  | Loop_end -> "loop_end"
  | Nop -> "nop"
  | Or -> "or"
  | Pipe -> "pipe"
  | Stdout -> "stdout"
  | Push s -> !% "push %S" s
  | Var -> "var"
  | Wait -> "wait"
  | While (st, ed) -> !% "while %d..%d" st ed