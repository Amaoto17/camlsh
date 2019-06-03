let (!%) = Printf.sprintf


type t =
  | Exec
  | Exit
  | Fork
  | Jump of int
  | Stdout
  | Pipe
  | Pipeopen
  | Pipewait
  | Push of string
  | Wait

let show = function
  | Exec -> "exec"
  | Exit -> "exit"
  | Fork -> "fork"
  | Jump pos -> !% "jump (%+d)" pos
  | Pipe -> "pipe"
  | Pipeopen -> "pipeopen"
  | Pipewait -> "pipewait"
  | Stdout -> "stdout"
  | Push s -> !% "push %S" s
  | Wait -> "wait"