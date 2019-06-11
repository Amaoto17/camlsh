open Util


type t =
  | And of t
  | Block of t list
  | Builtin of string * t list
  | External of t list
  | Or of t
  | Pipe of t * t
  | Seq of t list
  | Stdout of string
  | Word of string

let rec show = function
  | And node ->
      !% "(and %s)" (show node)
  | Block nodes ->
      !% "(block %s)"
        & nodes |> List.map show |> String.concat " "
  | Builtin (op, nodes) ->
      !% "(builtin %s %s)" op
        & nodes |> List.map show |> String.concat " "
  | External nodes ->
      !% "(external %s)"
        & nodes |> List.map show |> String.concat " "
  | Or node ->
      !% "(or %s)" (show node)
  | Pipe (l, r) ->
      !% "(pipe (%s %s))"
        (show l) (show r)
  | Seq nodes ->
      !% "%s"
        & nodes |> List.map show |> String.concat "; "
  | Stdout path ->
      !% "(> %s)" path
  | Word w ->
      w