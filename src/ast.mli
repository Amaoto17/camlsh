type t =
  | And of t
  | Array_ref of t * int * int option
  | Begin of t
  | Brace of t list
  | Break
  | Builtin of string * t list
  | Compound of t list
  | Continue
  | Control of t * t list
  | Elem of t list
  | External of t list
  | For of t * t list * t
  | Glob of string
  | Identifier of string
  | If of t * t
  | Or of t
  | Pipe of t * t
  | Pipe_err of t * t
  | Quoted of t list
  | Quoted_ident of t
  | Stderr of t
  | Stderr_append of t
  | Stdin of t
  | Stdout of t
  | Stdout_append of t
  | Subst of t
  | While of t * t
  | Word of string

val show : t -> string