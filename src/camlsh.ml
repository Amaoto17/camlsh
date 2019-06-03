open Printf
open Unix

let (!%) = Printf.sprintf


module Deco = struct
  let colorize color s =
    match color with
    | `Red     -> !% "\x1b[31m%s\x1b[m" s
    | `Green   -> !% "\x1b[32m%s\x1b[m" s
    | `Yellow  -> !% "\x1b[33m%s\x1b[m" s
    | `Magenta -> !% "\x1b[35m%s\x1b[m" s
    | `Cyan    -> !% "\x1b[36m%s\x1b[m" s
end

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
          Vm.execute code
      | Error msg ->
          eprintf "%s\n%!" (msg |> Deco.colorize `Red)
    with End_of_file -> exit 0
  done