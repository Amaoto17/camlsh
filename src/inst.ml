let (!%) = Printf.sprintf


type t =
  | Exec
  | Push of string

let show = function
  | Exec -> "exec"
  | Push s -> !% "push %S" s