open Util


type t =
  | Add_string of string
  | Begin
  | Brace
  | Brace_end
  | Break
  | Builtin
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
  | Return
  | Stdin
  | Stdout
  | Subst
  | Unless
  | Var
  | Wait
  | While of int * int

let show = function
  | Add_string s -> !% "add_string %S" s
  | Begin -> "begin"
  | Brace -> "brace"
  | Brace_end -> "brace_end"
  | Break -> "break"
  | Builtin -> "builtin"
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
  | Return -> "return"
  | Stdin -> "stdin"
  | Stdout -> "stdout"
  | Subst -> "subst"
  | Unless -> "unless"
  | Var -> "var"
  | Wait -> "wait"
  | While (st, ed) -> !% "while %d..%d" st ed