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

  | Ast.Array_ref (node, st, ed) ->
      walk t node;
      let ed =
        match ed with
        | None -> st
        | Some n -> n
      in
      Code.emit t & Array_ref (st, ed)

  | Ast.Begin body ->
      Code.emit t & Inst.Begin;
      walk t body;
      Code.emit t & Inst.End

  | Ast.Brace nodes ->
      Code.emit t & Inst.Brace;
      List.iter (walk t) nodes;
      Code.emit t & Inst.Brace_end
    
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

  | Ast.For (ident, values, body) ->
      List.iter (walk t) values;
      let _for = reserve t in
      let _start = Code.length t in
      walk t ident;
      Code.emit t & Inst.For_iter;
      let _iter = reserve t in
      walk t body;
      Code.emit t & Inst.Jump _start;
      let _end = Code.length t in
      Code.emit t & Inst.Loop_end;
      Code.set t _iter & Inst.Jump _end;
      Code.set t _for & Inst.For (_start, _end)

  | Ast.Glob pat ->
      Code.emit t & Inst.Add_string pat;
      Code.emit t & Inst.Glob

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

  | Ast.Pipe _ as pipe ->
      Code.emit t & Inst.Pipe_open;
      let _parent = reserve t in
      walk_pipe t pipe;
      Code.emit t & Inst.Exit;
      insert_jump t _parent

  | Ast.Stderr path ->
      walk t path;
      Code.emit t & Inst.Stderr

  | Ast.Stderr_append path ->
      walk t path;
      Code.emit t & Inst.Stderr_append

  | Ast.Stdin path ->
      walk t path;
      Code.emit t & Inst.Stdin

  | Ast.Stdout path ->
      walk t path;
      Code.emit t & Inst.Stdout

  | Ast.Stdout_append path ->
      walk t path;
      Code.emit t & Inst.Stdout_append

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


and walk_pipe t = function
  | Ast.Pipe (left, right) ->
      Code.emit t & Inst.Pipe;
      let parent = reserve t in
      walk_pipe t left;
      insert_jump t parent;
      walk_pipe t right
  
  | node ->
      walk t node;
      Code.emit t & Inst.Exit


let compile ast =
  let t = Code.create 64 in
  walk t ast;
  Code.emit t & Inst.Leave;
  Code.to_array t