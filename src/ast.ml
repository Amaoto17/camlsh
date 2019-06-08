open Util


type t =
  | Builtin of string * t list
  | External of t list
  | Pipe of t * t
  | Stdout of string
  | Word of string

let rec show = function
  | Builtin (op, nodes) ->
      !% "<builtin %s (%s)>" op
        & nodes |> List.map show |> String.concat " "
  | External nodes ->
      !% "<external (%s)>"
        & nodes |> List.map show |> String.concat " "
  | Pipe (l, r) ->
      !% "<pipe %s | %s>"
        (show l) (show r)
  | Stdout path ->
      !% "> %s" path
  | Word w ->
      w