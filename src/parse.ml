module Str_char = Oparse.Stream.Char
module Base = Oparse.Char.Make(Str_char)
module Parser = struct
  include Base
  include Oparse.Combinator.Make(Base)
  include Oparse.Expr.Make(Base)
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

let elem =
  succeed (fun w -> Ast.Word w)
    |= word

let redirection =
  succeed (fun path -> Ast.Stdout path)
    |. char '>'
    |. spaces
    |= word

let simple =
  succeed (fun elems -> Ast.External elems)
    |= many1 (elem <|> redirection)
    |. spaces

let command =
  simple

let pipeline =
  let op =
    succeed (fun l r -> Ast.Pipe (l, r))
      |. char '|'
      |. spaces
  in
  chainl command op

let program =
  succeed identity
    |. spaces
    |= pipeline
    |. eof


let parse s =
  let st = Str_char.of_string s in
  match run program st with
  | Ok v -> Ok v
  | Error e -> Error (Error.show e)