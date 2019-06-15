open Util


type t =
  | And of t
  | Block of t
  | Builtin of string * t list
  | Compound of t list
  | External of t list
  | Identifier of string
  | Or of t
  | Pipe of t * t
  | Stdout of string
  | While of t * t
  | Word of string

let rec show = function
  | And node ->
      !% "(and %s)" (show node)
  | Block node ->
      !% "(block %s)" (show node)
  | Compound nodes ->
      !% "%s"
        & nodes |> List.map show |> String.concat " "
  | Builtin (op, nodes) ->
      !% "(builtin %s %s)" op
        & nodes |> List.map show |> String.concat " "
  | External nodes ->
      !% "(external %s)"
        & nodes |> List.map show |> String.concat " "
  | Identifier name ->
      !% "(identifier %s)" name
  | Or node ->
      !% "(or %s)" (show node)
  | Pipe (left, right) ->
      !% "(pipe %s %s)" (show left) (show right)
  | Stdout path ->
      !% "(> %s)" path
  | While (cond, body) ->
      !% "(while %s %s)" (show cond) (show body)
  | Word s ->
      s