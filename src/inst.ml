let (!%) = Printf.sprintf


type t =
  | Exec
  | Fork
  | Jump of int
  | Stdout
  | Push of string
  | Wait

let show = function
  | Exec -> "exec"
  | Fork -> "fork"
  | Jump pos -> !% "jump [%d]" pos
  | Stdout -> "stdout"
  | Push s -> !% "push %S" s
  | Wait -> "wait"