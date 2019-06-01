let (!%) = Printf.sprintf
external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"

type t =
  | External of t list
  | Stdout of string
  | Word of string

let rec show = function
  | External nodes ->
      !% "<external (%s)>"
        & nodes |> List.map show |> String.concat " "
  | Stdout path ->
      !% "> %s" path
  | Word w ->
      w