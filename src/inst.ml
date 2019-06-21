open Util


type t =
  | Add_string of string
  | Array_ref of int * int
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
  | For of int * int
  | For_iter
  | Glob
  | If
  | Jump of int
  | Leave
  | Loop_end
  | Nop
  | Pipe
  | Push of string
  | Return
  | Stderr
  | Stderr_append
  | Stdin
  | Stdout
  | Stdout_append
  | Subst
  | Unless
  | Var
  | Wait
  | While of int * int

let show = function
  | Add_string s -> !% "add_string %S" s
  | Array_ref (st, ed) -> !% "array_ref %d..%d" st ed
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
  | For (st, ed) -> !% "for %d..%d" st ed
  | For_iter -> "for_iter"
  | Glob -> "glob"
  | If -> "if"
  | Jump pos -> !% "jump -> %02d" pos
  | Leave -> "leave"
  | Loop_end -> "loop_end"
  | Nop -> "nop"
  | Pipe -> "pipe"
  | Push s -> !% "push %S" s
  | Return -> "return"
  | Stderr -> "stdout"
  | Stderr_append -> "stdout_append"
  | Stdin -> "stdin"
  | Stdout -> "stdout"
  | Stdout_append -> "stdout_append"
  | Subst -> "subst"
  | Unless -> "unless"
  | Var -> "var"
  | Wait -> "wait"
  | While (st, ed) -> !% "while %d..%d" st ed