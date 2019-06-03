(*

simple:
  % ls
  fork
  jump 0:

  push "ls"
  exec

  label 0:
  wait

redirection:
  % ls > file.txt
  fork
  jump 0:

  push "file.txt"
  open
  stdin
  push "ls"
  exec

  label 0:
  wait

pipeline:
  % ls | head
  pipe
  jump 0:

  pipe
  jump 1:

  fork
  push "ls"
  exec
  wait

  fork
  push "head"
  exec
  wait

  label 0:
  pipewait

pipeline:
  % ls | sort | head
    pipeopen
    jump 0:
    pipe
    jump 1:
    fork
    jump 2:
    push "ls"
    exec
    wait
    exit
  2:
    fork
    jump
    push "sort"
    exec
    wait
    exit
  1:
    fork
    jump
    push "head"
    exec
    wait
    exit
  0:
    wait


*)

external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"

module Code = struct
  type t =
    { mutable seq : Inst.t list
    ; mutable len : int
    }

  let create () = { seq = []; len = 0 }

  let emit t inst =
    t.seq <- inst :: t.seq;
    t.len <- t.len + 1

  let get t =
    List.rev t.seq |> Array.of_list

  let append t1 t2 =
    t1.seq <- t2.seq @ t1.seq;
    t1.len <- t1.len + t2.len
end


let rec walk_pipe t = function
  | Ast.Pipe (left, right) ->
      Code.emit t Inst.Pipe;
      let sub = Code.create () in
      walk_pipe sub left;
      let offset = sub.len + 1 in
      Code.emit t & Inst.Jump offset;
      Code.append t sub;
      walk_pipe t right
  | node ->
      walk t node;
      Code.emit t Inst.Exit


and walk t = function
  | Ast.External nodes ->
      Code.emit t Inst.Fork;
      let sub = Code.create () in
      List.iter (walk sub) nodes;
      let offset = sub.len + 2 in
      Code.emit t & Inst.Jump offset;
      Code.append t sub;
      Code.emit t Inst.Exec;
      Code.emit t Inst.Wait

  | Ast.Pipe _ as pipe ->
      Code.emit t Inst.Pipeopen;
      let sub = Code.create () in
      walk_pipe sub pipe;
      let offset = sub.len + 1 in
      Code.emit t & Inst.Jump offset;
      Code.append t sub;
      Code.emit t Inst.Wait

  | Ast.Stdout path ->
      Code.emit t & Inst.Push path;
      Code.emit t Inst.Stdout

  | Ast.Word w ->
      Code.emit t & Inst.Push w


let compile ast =
  let t = Code.create () in
  walk t ast;
  Code.get t