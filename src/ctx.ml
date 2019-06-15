open Util
open Printf
open Unix


type t =
  { mutable status : int
  ; mutable stack : string Stack.t
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

(* redirection *)

let dup2_close fd1 fd2 =
  dup2 fd1 fd2;
  close fd1

let get_input t = t.redir.input

let get_output t = t.redir.output

let safe_close = function
  | None -> ()
  | Some fd -> try close fd with Unix_error(_, _, _) -> ()

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
  let redir = t.redir in
  do_redirection t;
  let thunk () =
    dup2_close stdin' stdin;
    dup2_close stdout' stdout;
    t.redir <- redir
  in
  thunk

let reset_redir t =
  get_input t |> safe_close;
  get_output t |> safe_close;
  t.redir.input <- None;
  t.redir.output <- None

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

(* handling variables *)

let new_env t =
  let local = t.vars.local in
  t.vars.local <- Env.new_env local

let delete_env t =
  let local = t.vars.local in
  t.vars.local <- Env.delete_env local

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