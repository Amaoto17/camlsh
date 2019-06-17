open Util


type t =
  | Add_string of string
  | Begin
  | Break
  | Builtin of string
  | Continue
  | Emit_string
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
  | Stdin
  | Stdout
  | Unless
  | Var
  | Wait
  | While of int * int

let show = function
  | Add_string s -> !% "add_string %S" s
  | Begin -> "begin"
  | Break -> "break"
  | Builtin op -> !% "builtin %s" op
  | Continue -> "continue"
  | Emit_string -> "emit_string"
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
  | Stdin -> "stdin"
  | Stdout -> "stdout"
  | Unless -> "unless"
  | Var -> "var"
  | Wait -> "wait"
  | While (st, ed) -> !% "while %d..%d" st ed