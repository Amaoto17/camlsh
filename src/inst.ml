open Util


type t =
  | Begin
  | Break
  | Builtin of string
  | Continue
  | End
  | Exec
  | Exit
  | If
  | Jump of int
  | Leave
  | Loop_end
  | Nop
  | Pipe
  | Push of string
  | Stdout
  | Unless
  | Var
  | Wait
  | While of int * int

let show = function
  | Begin -> "begin"
  | Break -> "break"
  | Builtin op -> !% "builtin %s" op
  | Continue -> "continue"
  | End -> "end"
  | Exec -> "exec"
  | Exit -> "exit"
  | If -> "if"
  | Jump pos -> !% "jump -> %02d" pos
  | Leave -> "leave"
  | Loop_end -> "loop_end"
  | Nop -> "nop"
  | Pipe -> "pipe"
  | Push s -> !% "push %S" s
  | Stdout -> "stdout"
  | Unless -> "unless"
  | Var -> "var"
  | Wait -> "wait"
  | While (st, ed) -> !% "while %d..%d" st ed