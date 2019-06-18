open Util


type t =
  | And of t
  | Begin of t
  | Brace of t list
  | Break
  | Builtin of string * t list
  | Compound of t list
  | Continue
  | Control of t * t list
  | Elem of t list
  | External of t list
  | Identifier of string
  | If of t * t
  | Or of t
  | Pipe of t * t
  | Stdin of t
  | Stdout of t
  | Subst of t
  | While of t * t
  | Word of string

let rec show = function
  | And node ->
      !% "(and %s)" (show node)
  | Begin body ->
      !% "(begin %s)" (show body)
  | Brace nodes ->
      !% "(brace %s)"
        & nodes |> List.map show |> String.concat " "
  | Break ->
      "break"
  | Builtin (name, nodes) ->
      !% "(builtin %s %s)" name
        & nodes |> List.map show |> String.concat " "
  | Compound nodes ->
      !% "%s"
        & nodes |> List.map show |> String.concat " "
  | Continue ->
      "continue"
  | Control (node, redir) ->
      !% "(control %s %s)" (show node)
        & redir |> List.map show |> String.concat " "
  | Elem nodes ->
      !% "(elem %s)" & nodes |> List.map show |> String.concat " "
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
  | Stdin path ->
      !% "(< %s)" (show path)
  | Stdout path ->
      !% "(> %s)" (show path)
  | Subst node ->
      !% "(subst %s)" (show node)
  | While (cond, body) ->
      !% "(while %s %s)" (show cond) (show body)
  | Word s ->
      !% "(word %s)" s