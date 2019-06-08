open Util


type t =
  | External of t list
  | Pipe of t * t
  | Stdout of string
  | Word of string

let rec show = function
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