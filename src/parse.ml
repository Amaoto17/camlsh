module Str_char = Oparse.Stream.Char
module Base = Oparse.Char.Make(Str_char)
module Parser = struct
  include Base
  include Oparse.Combinator.Make(Base)
end

open Parser
open Oparse.Util

let (!%) = Printf.sprintf


let word =
  one_of
    [ succeed identity
        |= many1 (not_in_class " |<>\n\r\t")
        |> concat
    ; expect "word"
    ]
    |. spaces

let simple =
  succeed (fun ws -> Ast.External ws)
    |= many1 word
    |. spaces

let command =
  simple

let program =
  succeed identity
    |. spaces
    |= command
    |. eof


let parse s =
  let st = Str_char.of_string s in
  match run program st with
  | Ok v -> Ok v
  | Error e -> Error (Error.show e)