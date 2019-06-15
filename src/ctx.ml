open Util
open Printf
open Unix


type t =
  { mutable status : int
  ; mutable stack : string Stack.t
  ; mutable loop_stack : (int * int) Stack.t
  ; mutable redir : redir
  ; vars : vars
  }

and redir =
  { mutable input : file_descr option
  ; mutable output : file_descr option
  }

and vars =
  { builtin : Env.t
  ; global : Env.t
  ; mutable local : Env.t
  }

let show t =
  let buf = Buffer.create 64 in
  Buffer.add_string buf "|";
  Stack.iter
    ( fun s ->
        Buffer.add_string buf " ";
        Buffer.add_string buf s;
        Buffer.add_string buf " |"
    )
    t.stack;
  Buffer.contents buf

let create () =
  { status = 0
  ; stack = Stack.create ()
  ; loop_stack = Stack.create ()
  ; redir =
      { input = None
      ; output = None
      }
  ; vars =
      { builtin = Env.create ()
      ; global = Env.create ()
      ; local = Env.create ()
      }
  }

(* TODO: ctx.status should be implemented as shell variable. *)

let get_status t = t.status

let set_status t status = t.status <- status

(* handling variables *)

let new_env t =
  let local = t.vars.local in
  t.vars.local <- Env.new_env local

let delete_env t =
  let local = t.vars.local in
  t.vars.local <- Env.delete_env local

let put_env t =
  let local = t.vars.local in
  t.vars.local <- Env.put_env local

let pop_env t =
  let local = t.vars.local in
  t.vars.local <- Env.pop_env local

let find t key =
  match Env.find t.vars.local key with
  | Some v -> Some v
  | None ->
      match Env.find t.vars.global key with
      | Some v -> Some v
      | None ->
          match Env.find t.vars.builtin key with
          | Some v -> Some v
          | None -> None

let set_local t = Env.set t.vars.local

(* redirection *)

let dup2_close fd1 fd2 =
  dup2 fd1 fd2;
  close fd1

let get_input t = t.redir.input

let get_output t = t.redir.output

let safe_close = function
  | None -> ()
  | Some fd -> try close fd with Unix_error(_, _, _) -> ()

let reset_redir t =
  get_input t |> safe_close;
  get_output t |> safe_close;
  t.redir.input <- None;
  t.redir.output <- None

let set_stdin t input =
  get_input t |> safe_close;
  t.redir.input <- Some input

let set_stdout t output =
  get_output t |> safe_close;
  t.redir.output <- Some output

let do_redirection t =
  begin match get_input t with
  | None -> ()
  | Some fd -> dup2_close fd stdin
  end;
  begin match get_output t with
  | None -> ()
  | Some fd -> dup2_close fd stdout
  end

let safe_redirection t =
  let stdin' = dup stdin in
  let stdout' = dup stdout in
  begin match get_input t with
  | None -> ()
  | Some fd -> dup2 fd stdin
  end;
  begin match get_output t with
  | None -> ()
  | Some fd -> dup2 fd stdout
  end;
  let return () =
    dup2_close stdin' stdin;
    dup2_close stdout' stdout
  in
  return


(* stack operation *)

let push t s = Stack.push s t.stack

let pop t = Stack.pop t.stack

let pop_all t =
  let rec loop acc =
    if Stack.is_empty t.stack then acc
    else
      let elem = Stack.pop t.stack in
      elem :: acc |> loop
  in
  loop [] |> Array.of_list


(* handling loop *)

let begin_loop t st ed =
  put_env t;
  Stack.push (st, ed) t.loop_stack

let exit_loop t =
  pop_env t;
  Stack.pop t.loop_stack |> ignore

let loop_start t =
  if Stack.is_empty t.loop_stack then None
  else let (st, _) = Stack.top t.loop_stack in Some st

let loop_end t =
  if Stack.is_empty t.loop_stack then None
  else let (_, ed) = Stack.top t.loop_stack in Some ed