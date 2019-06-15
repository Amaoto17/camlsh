open Util


type t =
  | And of t
  | Begin of t
  | Break
  | Builtin of string * t list
  | Compound of t list
  | Continue
  | External of t list
  | Identifier of string
  | If of t * t
  | Or of t
  | Pipe of t * t
  | Stdout of string
  | While of t * t
  | Word of string

let rec show = function
  | And node ->
      !% "(and %s)" (show node)
  | Begin node ->
      !% "(begin %s)" (show node)
  | Break ->
      "break"
  | Builtin (op, nodes) ->
      !% "(builtin %s %s)" op
        & nodes |> List.map show |> String.concat " "
  | Compound nodes ->
      !% "%s"
        & nodes |> List.map show |> String.concat " "
  | Continue ->
      "continue"
  | External nodes ->
      !% "(external %s)"
        & nodes |> List.map show |> String.concat " "
  | Identifier name ->
      !% "(identifier %s)" name
  | If (cond, body) ->
      !% "(if %s %s)" (show cond) (show body)
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