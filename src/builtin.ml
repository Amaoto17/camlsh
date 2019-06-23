open Util
open Printf
open Unix


let parse_argv spec argc argv =
  let rec loop i =
    if i >= argc then [||]
    else
      let arg = argv.(i) in
      match spec arg with
      | None -> Array.sub argv i (argc - i)
      | Some f -> f (); loop (i + 1)
  in
  loop 1

let return = Ctx.set_status


let cd ctx argc argv =
  let return = return ctx in
  match argc with
  | 1 ->
      chdir (getenv "HOME");
      return 0
  | 2 ->
      chdir argv.(1);
      return 0
  | _ ->
      eprintf "cd: too many arguments\n%!";
      return 1


let echo ctx argc argv =
  let return = return ctx in
  let print_nl = ref true in
  let spec = function
    | "-n" -> Some (fun () -> print_nl := false)
    | _ -> None
  in
  let res = parse_argv spec argc argv in
  let output = res |> Array.to_list |> String.concat " " in
  printf "%s" output;
  if !print_nl then print_newline ();
  printf "%!";
  return 0


let false_ ctx argc argv =
  let return = return ctx in
  match argc with
  | 1 -> return 1
  | _ ->
      eprintf "false: too many arguments\n%!";
      return 2


let true_ ctx argc argv =
  let return = return ctx in
  match argc with
  | 1 -> return 0
  | _ ->
      eprintf "true: too many arguments\n%!";
      return 2


let set ctx argc argv =
  let return = return ctx in
  let sc_global = ref false in
  let spec = function
    | "-g" -> Some (fun () -> sc_global := true)
    | _ -> None
  in
  let res = parse_argv spec argc argv in
  let len = Array.length res in
  if len < 1 then return 1
  else
    let v = Array.sub res 1 (len - 1) in
    try
      if !sc_global then Ctx.set_global ctx res.(0) v
      else Ctx.set_local ctx res.(0) v;
      return 0
    with Failure s ->
      eprintf "%s\n%!" s;
      return 1


let exec ctx argv =
  let argc = Array.length argv in
  let return = return ctx in
  match argv.(0) with
  | "cd" -> cd ctx argc argv
  | "echo" -> echo ctx argc argv
  | "false" -> false_ ctx argc argv
  | "set" -> set ctx argc argv
  | "true" -> true_ ctx argc argv
  | com ->
      eprintf "unknown builtin '%s'\n%!" com;
      return 1