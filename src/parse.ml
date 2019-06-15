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
        |= many1 (not_in_class " |<>;$\n\r\t")
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

let identifier =
  one_of
    [ succeed List.cons
        |= (letter <|> char '_')
        |= many (alpha_num <|> char '_')
        |> concat
    ; expect "identifier"
    ]
    |. spaces

let elem =
  one_of
    [ succeed (fun s -> Ast.Identifier s)
        |. char '$'
        |. spaces
        |= identifier
    ; succeed (fun s -> Ast.Word s)
        |= word
    ]

let redirection =
  one_of
    [ succeed (fun path -> Ast.Stdin path)
        |. char '<'
        |. spaces
        |= word
    ; succeed (fun path -> Ast.Stdout path)
        |. char '>'
        |. spaces
        |= word
    ]

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
        ; keyword "set"
        ; keyword "true"
        ]
    |= many (elem <|> redirection)
    |. spaces

let command =
  builtin <|> simple

let rec block = fun st -> (|>) st &
  let rec loop acc =
    one_of
      [ succeed acc
          |. look_ahead (keyword "end" <|> keyword "do" <|> keyword "then")
          |> map List.rev
          |> map (fun coms -> Ast.Compound coms)
      ; pipeline
          |. char ';'
          |. spaces
          |> and_then (fun x -> x :: acc |> loop)
      ; expect "end of block"
      ]
  in
  loop []

and control = fun st -> (|>) st &
  one_of
    [ succeed (fun com -> Ast.And com)
        |. keyword "and"
        |= command
    ; succeed (fun com -> Ast.Or com)
        |. keyword "or"
        |= command
    ; succeed (fun body -> Ast.Begin body)
        |. keyword "begin"
        |= block
        |. keyword "end"
    ; succeed (fun cond body -> Ast.If (cond, body))
        |. keyword "if"
        |= block
        |. keyword "then"
        |= block
        |. keyword "end"
    ; succeed (fun cond body -> Ast.While (cond, body))
        |. keyword "while"
        |= block
        |. keyword "do"
        |= block
        |. keyword "end"
    ; succeed Ast.Break
        |. keyword "break"
    ; succeed Ast.Continue
        |. keyword "continue"
    ; command
    ]

and pipeline = fun st -> (|>) st &
  let op =
    succeed (fun left right -> Ast.Pipe (left, right))
      |. char '|'
      |. spaces
  in
  chainl control op

and compound = fun st -> (|>) st &
  let delimiter =
    char ';'
      |. spaces
  in
  succeed (fun coms -> Ast.Compound coms)
    |= sep_end_by1
        delimiter
        pipeline

let program =
  succeed identity
    |. spaces
    |= compound
    |. eof


let parse s =
  let st = Str_char.of_string s in
  match run program st with
  | Ok v -> Ok v
  | Error e -> Error (Error.show e)