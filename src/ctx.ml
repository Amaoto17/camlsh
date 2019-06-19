open Util
open Printf
open Unix

let nop () = ()

type t = 
  { mutable control_stack : frame * frame Stack.t
  ; redir : redir
  ; mutable return : (unit -> unit)
  ; vars : vars
  }

and frame =
  { stack : string Stack.t
  ; exp_buf : string list Stack.t
  ; loop_range : (int * int) option
  ; mutable local : string array Env.t
  }

and redir =
  { mutable input : file_descr option
  ; mutable output : file_descr option
  ; mutable error : file_descr option
  }

and vars =
  { builtin : string array Env.t
  ; global : string array Env.t
  }


(* operation for control_stack *)

let new_frame loop_range =
  { stack = Stack.create ()
  ; exp_buf = Stack.create ()
  ; loop_range = loop_range
  ; local = Env.create ()
  }

let current_frame t =
  fst t.control_stack

let push_frame ?loop_range t =
  let (current, stack) = t.control_stack in
  Stack.push current stack;
  let fr = new_frame loop_range in
  fr.local <- Env.new_env current.local;
  t.control_stack <- (fr, stack)

let pop_frame t =
  let (_, stack) = t.control_stack in
  let current = Stack.pop stack in
  t.control_stack <- (current, stack)

let rec pop_frame_all t =
  let (_, stack) = t.control_stack in
  if not (Stack.is_empty stack) then begin
    pop_frame t;
    pop_frame_all t
  end


(* handling variables *)

let new_env t =
  let fr = current_frame t in
  fr.local <- Env.new_env fr.local

let delete_env t =
  let fr = current_frame t in
  match Env.delete_env fr.local with
  | None -> ()
  | Some env -> fr.local <- env

let delete_env_all t =
  let fr = current_frame t in
  let rec loop env =
    match Env.delete_env env with
    | None -> fr.local <- env
    | Some env' -> loop env'
  in
  loop fr.local

let find t key =
  let fr = current_frame t in
  match Env.find fr.local key with
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
  let fr = current_frame t in
  match Env.find t.vars.builtin key with
  | None -> Env.set fr.local key v
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

let push t s =
  let fr = current_frame t in
  Stack.push s fr.stack

let pop t =
  let fr = current_frame t in
  Stack.pop fr.stack

let pop_all t =
  let fr = current_frame t in
  let res = Stack.fold (fun xs x -> x :: xs) [] fr.stack in
  Stack.clear fr.stack;
  res |> Array.of_list


(* handling expansion *)

let cartesian acc xs =
  List.map (fun x -> List.map (fun xs -> x :: xs) acc) xs |> List.concat

let add_empty t =
  let fr = current_frame t in
  Stack.push [] fr.exp_buf

let add_string t s =
  let fr = current_frame t in
  Stack.push [s] fr.exp_buf

let add_string_list t ss =
  let fr = current_frame t in
  Stack.push ss fr.exp_buf

let take_string t =
  let fr = current_frame t in
  Stack.pop fr.exp_buf

let concat_string t =
  let fr = current_frame t in
  let res = Stack.fold (fun xs x -> x :: xs) [] fr.exp_buf in
  Stack.clear fr.exp_buf;
  res |> List.concat

let emit_string t =
  let fr = current_frame t in
  let res = Stack.fold cartesian [[]] fr.exp_buf in
  Stack.clear fr.exp_buf;
  List.map (String.concat "") res


(* handling loop *)

let loop_start t =
  let fr = current_frame t in
  match fr.loop_range with
  | None -> None
  | Some (st, _) -> Some st

let loop_end t =
  let fr = current_frame t in
  match fr.loop_range with
  | None -> None
  | Some (_, ed) -> Some ed


(* initialization and utilities *)

let create () =
  { control_stack = (new_frame None, Stack.create ())
  ; redir =
      { input = None
      ; output = None
      ; error = None
      }
  ; return = nop
  ; vars =
      { builtin = Env.create ()
      ; global = Env.create ()
      }
  }

let show t =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "|";
  let fr = current_frame t in
  Stack.iter (fun s -> Buffer.add_string buf (!% " %s |" s)) fr.stack;
  Buffer.add_string buf "\n|";
  Stack.iter
    (fun ss ->
      Buffer.add_string buf (!% " [%s] |" (String.concat "; " ss))
    )
    fr.exp_buf;
  Buffer.contents buf

let init t =
  set_builtin t "status" [|"0"|]

let reset_all t =
  return t;
  reset_redir t;
  pop_frame_all t;
  delete_env_all t