open Util


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
  | Null
  | Or of t
  | Pipe of t * t
  | Pipe_err of t * t
  | Quoted_ident of t
  | Stderr of t
  | Stderr_append of t
  | Stdin of t
  | Stdout of t
  | Stdout_append of t
  | Subst of t
  | While of t * t
  | Word of string

let rec show = function
  | And node ->
      !% "(and %s)" (show node)
  | Array_ref (node, st, ed) ->
      begin match ed with
      | None -> !% "(array_ref %s[%d])" (show node) st
      | Some ed -> !% "(array_ref %s[%d..%d])" (show node) st ed
      end
  | Begin body ->
      !% "(begin %s)" (show body)
  | Brace nodes ->
      !% "(brace %s)" (show_list nodes)
  | Break ->
      "break"
  | Builtin (name, nodes) ->
      !% "(builtin %s %s)" name (show_list nodes)
  | Compound nodes ->
      !% "%s" (show_list nodes)
  | Continue ->
      "continue"
  | Control (node, redir) ->
      !% "(control %s %s)" (show node) (show_list redir)
  | Elem nodes ->
      !% "(elem %s)" (show_list nodes)
  | External nodes ->
      !% "(external %s)" (show_list nodes)
  | For (ident, values, body) ->
      !% "(for %s %s %s)" (show ident) (show_list values) (show body)
  | Glob s ->
      !% "(glob %s)" s
  | Identifier name ->
      !% "(identifier %s)" name
  | If (cond, body) ->
      !% "(if %s %s)" (show cond) (show body)
  | Null ->
      "(null)"
  | Or node ->
      !% "(or %s)" (show node)
  | Pipe (left, right) ->
      !% "(pipe %s %s)" (show left) (show right)
  | Pipe_err (left, right) ->
      !% "(pipe_err %s %s)" (show left) (show right)
  | Quoted_ident node ->
      !% "(quoted_ident %s)" (show node)
  | Stderr path ->
      !% "(^ %s)" (show path)
  | Stderr_append path ->
      !% "(^^ %s)" (show path)
  | Stdin path ->
      !% "(< %s)" (show path)
  | Stdout path ->
      !% "(> %s)" (show path)
  | Stdout_append path ->
      !% "(>> %s)" (show path)
  | Subst node ->
      !% "(subst %s)" (show node)
  | While (cond, body) ->
      !% "(while %s %s)" (show cond) (show body)
  | Word s ->
      !% "(word %s)" s

and show_list xs = xs |> List.map show |> String.concat " "