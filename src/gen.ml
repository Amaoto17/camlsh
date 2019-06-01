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
  % ls | sort | head
  pipe
  jump 0:

  stdin
  pipe
  jump 1:

  stdin
  pipe
  jump 2:

  stdin
  push "ls"
  exec
  exit

  label 2:
  stdout
  push "sort"
  exec
  exit

  label 1:
  stdout
  push "head"
  exec
  exit

  label 0:
  pipewait

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


let rec walk t = function
  | Ast.External nodes ->
      Code.emit t Inst.Fork;
      let sub = Code.create () in
      List.iter (walk sub) nodes;
      let pos = t.len + sub.len + 2 in
      Code.emit t & Inst.Jump pos;
      Code.append t sub;
      Code.emit t Inst.Exec;
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