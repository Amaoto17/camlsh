let (!%) = Printf.sprintf


type t =
  | External of string list

let show = function
  | External args ->
      !% "<external (%s)>" (String.concat " " args)