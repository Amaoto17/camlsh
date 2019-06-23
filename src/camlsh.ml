open Printf
open Unix

open Util


let dump_code =
  Array.iteri
    ( fun i inst ->
        let s =
          !% "[%02d] %s" i (Inst.show inst)
            |> Deco.colorize `Magenta
        in
        eprintf "%s\n%!" s
    )

let rec main_loop ctx =
  try
    while true do
      printf "[%s]\n" (getcwd () |> Deco.colorize `Yellow);
      printf "%% ";
      let input = read_line () in
        match Parse.parse input with
        | Ok ast ->
            eprintf "%s\n%!" (Ast.show ast |> Deco.colorize `Cyan);
            let code = Gen.compile ast in
            dump_code code;
            Vm.execute ctx code;
            ()
        | Error msg ->
            eprintf "%s\n%!" (msg |> Deco.colorize `Red)
    done
  with
    | End_of_file -> exit 0
    | Vm.Interruption ->
        Ctx.reset_all ctx;
        eprintf "interrupted.\n%!";
        main_loop ctx


let main =
  Vm.signal_init ();
  let ctx = Ctx.create () in
  Ctx.init ctx;
  main_loop ctx