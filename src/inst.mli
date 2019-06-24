type t =
  | Add_string of string
  | Array_ref of int * int
  | Begin
  | Brace
  | Brace_end
  | Break
  | Builtin
  | Concat_array
  | Continue
  | Emit_string
  | End
  | Exec
  | Exec_nofork
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
  | Pipe_err
  | Pipe_open
  | Push of string
  | Stderr
  | Stderr_append
  | Stdin
  | Stdout
  | Stdout_append
  | Subst
  | Unless
  | Var
  | While of int * int

val show : t -> string