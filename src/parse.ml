module Str_char = Oparse.Stream.Char
module Base = Oparse.Char.Make(Str_char)
module Parser = struct
  include Base
  include Oparse.Combinator.Make(Base)
  include Oparse.Expr.Make(Base)
end

open Parser
open Oparse.Util
open Util


let word =
  one_of
    [ succeed identity
        |= many1 (not_in_class " |<>;\n\r\t")
        |> concat
    ; expect "word"
    ]
    |. spaces

let keyword s =
  one_of
    [ backtrack &
        succeed identity
          |= string s
          |. not_followed_by (alpha_num <|> char '_')
    ; expect (!% "keyword %S" s)
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

let builtin =
  succeed (fun op elems -> Ast.Builtin (op, elems))
    |= one_of
        [ keyword "cd"
        ; keyword "echo"
        ; keyword "false"
        ; keyword "true"
        ]
    |= many (elem <|> redirection)
    |. spaces

let command =
  builtin <|> simple

let control =
  one_of
    [ succeed (fun com -> Ast.And com)
        |. keyword "and"
        |= command
    ; succeed (fun com -> Ast.Or com)
        |. keyword "or"
        |= command
    ; command
    ]

let pipeline =
  let op =
    succeed (fun l r -> Ast.Pipe (l, r))
      |. char '|'
      |. spaces
  in
  chainl control op

let sequence =
  succeed (fun coms -> Ast.Seq coms)
    |= sep_end_by1
        ( char ';'
            |. spaces
        )
        pipeline

let program =
  succeed identity
    |. spaces
    |= sequence
    |. eof


let parse s =
  let st = Str_char.of_string s in
  match run program st with
  | Ok v -> Ok v
  | Error e -> Error (Error.show e)