(*

simple:
  % ls
  push "ls"
  exec

redirection:
  % ls > file.txt
  push "ls"
  push "file.txt"
  stdout
  exec
    
pipeline:
  % ls | head
    pipe
    jump 0: ;jump to parent
;child
    push "ls"
    exec
    exit
;parent
  0:
    push "head"
    exec
    wait

pipeline3:
  % ls | sort | head
    pipe
    jump 0: ;jump to parent
;child
    pipe
    jump 1: ;jump to parent
;parent
    push "ls"
    exec
    exit
;child
    push "sort"
    exec
    wait
    exit
;parent
  0:
    push "head"
    exec
    wait

*)

open Util


module Code = struct
  type t = Inst.t Arraybuffer.t

  let create n = Arraybuffer.create n Inst.Nop

  let length = Arraybuffer.length

  let to_array = Arraybuffer.contents

  let emit = Arraybuffer.add

  let set = Arraybuffer.set
end


let reserve t =
  let pos = Code.length t in
  Code.emit t & Inst.Nop;
  pos

let insert_jump t pos =
  let dst = Code.length t in
  Code.set t pos & Inst.Jump dst


let rec walk t = function
  | Ast.Builtin (op, nodes) ->
      List.iter (walk t) nodes;
      Code.emit t & Inst.Builtin op

  | Ast.External nodes ->
      List.iter (walk t) nodes;
      Code.emit t & Inst.Exec

  | Ast.Pipe (left, right) ->
      Code.emit t & Inst.Pipe;
      let parent = reserve t in
      walk t left;
      Code.emit t & Inst.Exit;
      insert_jump t parent;
      walk t right;
      Code.emit t & Inst.Wait

  | Ast.Stdout path ->
      Code.emit t & Inst.Push path;
      Code.emit t & Inst.Stdout

  | Ast.Word s ->
      Code.emit t & Inst.Push s


let compile ast =
  let t = Code.create 64 in
  walk t ast;
  Code.to_array t