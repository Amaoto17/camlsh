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
  | Ast.And node ->
      Code.emit t & Inst.If;
      let _end = reserve t in
      walk t node;
      insert_jump t _end

  | Ast.Begin body ->
      Code.emit t & Inst.Begin;
      walk t body;
      Code.emit t & Inst.End
    
  | Ast.Break ->
      Code.emit t & Inst.Break

  | Ast.Builtin (name, nodes) ->
      Code.emit t & Inst.Push name;
      List.iter (walk t) nodes;
      Code.emit t & Inst.Builtin;
      Code.emit t & Inst.Return

  | Ast.Compound nodes ->
      List.iter (walk t) nodes

  | Ast.Continue ->
      Code.emit t & Inst.Continue

  | Ast.Control (node, redir) ->
      List.iter (walk t) redir;
      walk t node

  | Ast.Elem nodes ->
      List.iter (walk t) nodes;
      Code.emit t & Inst.Emit_string

  | Ast.External nodes ->
      List.iter (walk t) nodes;
      Code.emit t & Inst.Exec

  | Ast.Identifier name ->
      Code.emit t & Inst.Push name;
      Code.emit t & Inst.Var

  | Ast.If (cond, body) ->
      walk t cond;
      Code.emit t & Inst.If;
      let _end = reserve t in
      Code.emit t & Inst.Begin;
      walk t body;
      Code.emit t & Inst.End;
      insert_jump t _end

  | Ast.Or node ->
      Code.emit t & Inst.Unless;
      let _end = reserve t in
      walk t node;
      insert_jump t _end

  | Ast.Pipe (left, right) ->
      Code.emit t & Inst.Pipe;
      let parent = reserve t in
      walk t left;
      Code.emit t & Inst.Exit;
      insert_jump t parent;
      walk t right;
      Code.emit t & Inst.Wait

  | Ast.Stdin path ->
      walk t path;
      Code.emit t & Inst.Stdin

  | Ast.Stdout path ->
      walk t path;
      Code.emit t & Inst.Stdout

  | Ast.Subst node ->
      Code.emit t & Inst.Subst;
      let _end = reserve t in
      walk t node;
      Code.emit t & Inst.Exit;
      insert_jump t _end

  | Ast.While (cond, body) ->
      let _while = reserve t in
      let _start = Code.length t in
      walk t cond;
      Code.emit t & Inst.If;
      let _cond = reserve t in
      walk t body;
      Code.emit t & Inst.Jump _start;
      let _end = Code.length t in
      Code.emit t & Inst.Loop_end;
      Code.set t _cond & Inst.Jump _end;
      Code.set t _while & Inst.While (_start, _end)

  | Ast.Word s ->
      Code.emit t & Inst.Add_string s


let compile ast =
  let t = Code.create 64 in
  walk t ast;
  Code.emit t & Inst.Leave;
  Code.to_array t