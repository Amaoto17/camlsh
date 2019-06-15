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


let main =
  let ctx = Ctx.create () in
  Ctx.init ctx;
  while true do
    printf "[%s]\n" (getcwd () |> Deco.colorize `Yellow);
    printf "%% ";
    let input = read_line () in
    try
      match Parse.parse input with
      | Ok ast ->
          eprintf "%s\n%!" (Ast.show ast |> Deco.colorize `Cyan);
          let code = Gen.compile ast in
          dump_code code;
          Vm.execute ctx code
      | Error msg ->
          eprintf "%s\n%!" (msg |> Deco.colorize `Red)
    with End_of_file -> exit 0
  done