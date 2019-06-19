open Util
open Printf
open Unix

let nop () = ()

type t =
  { stack : string Stack.t
  ; exp_buf : string list Stack.t Stack.t
  ; loop_stack : (int * int) Stack.t
  ; mutable redir : redir
  ; mutable return : (unit -> unit)
  ; vars : vars
  }

and redir =
  { mutable input : file_descr option
  ; mutable output : file_descr option
  ; mutable error : file_descr option
  }

and vars =
  { builtin : string array Env.t
  ; global : string array Env.t
  ; mutable local : string array Env.t
  }

let show t =
  let buf = Buffer.create 64 in
  Buffer.add_string buf "|";
  Stack.iter
    ( fun s ->
        Buffer.add_char buf ' ';
        Buffer.add_string buf s;
        Buffer.add_string buf " |"
    )
    t.stack;
  Buffer.add_string buf "\n|";
  Stack.iter
    ( fun ss ->
        Buffer.add_string buf " [";
        Buffer.add_string buf (String.concat "; " ss);
        Buffer.add_string buf "] |"
    )
    (Stack.top t.exp_buf);
  Buffer.contents buf

let create () =
  { stack = Stack.create ()
  ; exp_buf = Stack.create ()
  ; loop_stack = Stack.create ()
  ; redir =
      { input = None
      ; output = None
      ; error = None
      }
  ; return = nop
  ; vars =
      { builtin = Env.create ()
      ; global = Env.create ()
      ; local = Env.create ()
      }
  }

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


let set_builtin t = Env.set t.vars.builtin

let get_status t =
  match Env.find t.vars.builtin "status" with
  | None -> failwith "'status' was not found"
  | Some v -> v.(0)

let set_status t v =
  Env.set t.vars.builtin "status" [|string_of_int v|]

let set_local t key v =
  match Env.find t.vars.builtin key with
  | None -> Env.set t.vars.local key v
  | Some _ -> failwith (!% "'%s' is read-only variable" key)

(* redirection *)

let dup2_close fd1 fd2 =
  dup2 fd1 fd2;
  close fd1

let get_input t = t.redir.input

let get_output t = t.redir.output

let get_error t = t.redir.error

let safe_close = function
  | None -> ()
  | Some fd -> try close fd with Unix_error(_, _, _) -> ()

let reset_redir t =
  get_input t |> safe_close;
  get_output t |> safe_close;
  get_error t |> safe_close;
  t.redir.input <- None;
  t.redir.output <- None;
  t.redir.error <- None

let set_stdin t input =
  get_input t |> safe_close;
  t.redir.input <- Some input

let set_stdout t output =
  get_output t |> safe_close;
  t.redir.output <- Some output

let set_stderr t error =
  get_error t |> safe_close;
  t.redir.error <- Some error

let do_redirection t =
  begin match get_input t with
  | None -> ()
  | Some fd -> dup2_close fd stdin
  end;
  begin match get_output t with
  | None -> ()
  | Some fd -> dup2_close fd stdout
  end;
  begin match get_error t with
  | None -> ()
  | Some fd -> dup2_close fd stderr
  end

let set_return t thunk =
  t.return <- thunk

let return t =
  t.return ();
  t.return <- nop

let safe_redirection t =
  let stdin' = dup stdin in
  let stdout' = dup stdout in
  let stderr' = dup stderr in
  begin match get_input t with
  | None -> ()
  | Some fd -> dup2 fd stdin
  end;
  begin match get_output t with
  | None -> ()
  | Some fd -> dup2 fd stdout
  end;
  begin match get_error t with
  | None -> ()
  | Some fd -> dup2 fd stderr
  end;
  let thunk () =
    dup2_close stdin' stdin;
    dup2_close stdout' stdout;
    dup2_close stderr' stderr
  in
  set_return t thunk


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

let clear_stack t = Stack.clear t.stack


(* handling expansion *)

let cartesian acc xs =
  List.map (fun x -> List.map (fun xs -> x :: xs) acc) xs |> List.concat

let push_buf t =
  Stack.push (Stack.create ()) t.exp_buf

let pop_buf t =
  Stack.pop t.exp_buf |> ignore

let buf_top t =
  Stack.top t.exp_buf

let add_empty t =
  Stack.push [] (buf_top t)

let add_string t s =
  Stack.push [s] (buf_top t)

let add_string_list t ss =
  Stack.push ss (buf_top t)

let clear_buf t =
  Stack.clear (buf_top t)

let concat_string t =
  let res = Stack.fold (fun xs x -> x :: xs) [] (buf_top t) in
  clear_buf t;
  res |> List.concat

let take_string t =
  Stack.pop (buf_top t)

let emit_string t =
  let res = Stack.fold cartesian [[]] (buf_top t) in
  clear_buf t;
  List.map (String.concat "") res


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


(* initialization *)

let init t =
  set_builtin t "status" [|"0"|];
  push_buf t;
  new_env t

let reset_all t =
  return t;
  reset_redir t;
  Stack.clear t.stack;
  Stack.clear t.exp_buf;
  push_buf t;
  while not (Stack.is_empty t.loop_stack) do
    exit_loop t
  done;
  try while true do delete_env t done
  with _ -> ()