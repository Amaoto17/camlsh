open Util
open Printf
open Unix


let cd argc argv =
  if argc = 1 then begin
    chdir (getenv "HOME");
    0
  end else if argc = 2 then begin
    chdir argv.(1);
    0
  end else begin
    eprintf "cd: too many arguments\n%!";
    1
  end


let echo argc argv =
  let print_nl = ref true in

  let strs = Stack.create () in

  let spec_list =
    [ ("-n", Arg.Clear print_nl, "do not output a newline")
    ]
  in
  let usage = "usage: echo [-n] [string ...]" in
  let anon s = Stack.push s strs in

  Arg.current := 0;
  try
    Arg.parse_argv argv spec_list anon usage;
    let res =
      Stack.fold (fun xs x -> x :: xs) [] strs
        |> String.concat " "
    in
    printf "%s" res;
    if !print_nl then print_newline ();
    flush Pervasives.stdout;
    0
  with Arg.Bad s | Arg.Help s ->
    eprintf "%s\n%!" s;
    1


let set ctx argc argv =
  if argc = 1 then
    1
  else
    try
      let v = Array.sub argv 2 (argc - 2) in
      Ctx.set_local ctx argv.(1) v;
      0
    with Failure s ->
      eprintf "%s\n%!" s;
      1