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


let reserved =
  [ "end"
  ; "then"
  ; "do"
  ]

let is_reserved s = List.mem s reserved

let symbol c =
  char c
    |. spaces

let meta_chars = " |;,'^$<>(){}\\"

let control_char =
  in_class "abefnrtv"
    |> map
        ( function
            | 'a' -> '\x07'
            | 'b' -> '\x08'
            | 'e' -> '\x1b'
            | 'f' -> '\x0c'
            | 'n' -> '\x0a'
            | 'r' -> '\x0d'
            | 't' -> '\x09'
            | 'v' -> '\x0b'
            | _ -> failwith "unreachable pattern: illegal control character"
        )

let escaped_char chars =
  one_of
    [ succeed identity
        |. char '\\'
        |= one_of
            [ in_class chars
            ; control_char
            ; unexpect "illegal backslash escape"
            ]
    ; not_in_class meta_chars
    ]

let word =
  one_of
    [ succeed identity
        |= many1 (escaped_char meta_chars)
        |> concat
    ; expect "word"
    ]

let keyword s =
  one_of
    [ backtrack &
        succeed identity
          |= string s
          |. not_followed_by (not_in_class meta_chars)
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

let single_quoted =
  succeed (fun s -> Ast.Word s)
    |. char '\''
    |= one_of
        [ succeed identity
            |= many1 (escaped_char "\'\\")
            |> concat
        ; expect "word"
        ]
    |. char '\''

let word_elem =
  succeed (fun s -> Ast.Word s)
    |= word

let rec brace = fun st -> (|>) st &
  one_of
    [ succeed (fun nodes -> Ast.Brace nodes)
        |. symbol '{'
        |= sep_by1 (char ',' |. spaces) subst
        |. char '}'
    ; word_elem
    ]

and ident = fun st -> (|>) st &
  one_of
    [ succeed (fun name -> Ast.Identifier name)
        |. char '$'
        |= identifier
    ; brace
    ]

and subst = fun st -> (|>) st &
  one_of
    [ succeed (fun comp -> Ast.Subst comp)
        |. symbol '('
        |= compound
        |. char ')'
    ; ident
    ]

and elem = fun st -> (|>) st &
  succeed (fun nodes -> Ast.Elem nodes)
    |= many1
        ( one_of
            [ single_quoted
            ; subst
            ]
        )
    |. spaces


and redirection = fun st -> (|>) st &
  one_of
    [ succeed (fun path -> Ast.Stdin path)
        |. char '<'
    ; succeed (fun path -> Ast.Stdout_append path)
        |. string ">>"
    ; succeed (fun path -> Ast.Stdout path)
        |. char '>'
    ; succeed (fun path -> Ast.Stderr_append path)
        |. string "^^"
    ; succeed (fun path -> Ast.Stderr path)
        |. char '^'
    ]
    |. spaces
    |= elem
    |. spaces

and simple = fun st -> (|>) st &
  succeed (fun elems -> Ast.External elems)
    |= many1 (elem <|> redirection)
    |. spaces

and builtin = fun st -> (|>) st &
  succeed (fun name elems -> Ast.Builtin (name, elems))
    |= one_of
        [ keyword "cd"
        ; keyword "echo"
        ; keyword "false"
        ; keyword "set"
        ; keyword "true"
        ]
    |= many (elem <|> redirection)
    |. spaces

and command = fun st -> (|>) st &
  one_of
    [ succeed Ast.Break
        |. keyword "break"
    ; succeed Ast.Continue
        |. keyword "continue"
    ; builtin
    ; simple
    ]

and control = fun st -> (|>) st &
  one_of
    [ succeed (fun node redir -> Ast.Control (node, redir))
        |= one_of
            [ succeed (fun body -> Ast.Begin body)
                |. keyword "begin"
                |= compound
            ; succeed (fun cond body -> Ast.If (cond, body))
                |. keyword "if"
                |= compound
                |. keyword "then"
                |= compound
            ; succeed (fun cond body -> Ast.While (cond, body))
                |. keyword "while"
                |= compound
                |. keyword "do"
                |= compound
            ]
        |. keyword "end"
        |= many redirection
    ; command
    ]

and conditional = fun st -> (|>) st &
  one_of
    [ succeed (fun com -> Ast.And com)
        |. keyword "and"
        |= control
    ; succeed (fun com -> Ast.Or com)
        |. keyword "or"
        |= control
    ; control
    ]

and pipeline = fun st -> (|>) st &
  let op =
    succeed (fun left right -> Ast.Pipe (left, right))
      |. symbol '|'
  in
  chainl conditional op

and compound = fun st -> (|>) st &
  let aux =
    look_ahead word
      |> and_then
          ( fun s ->
              if is_reserved s then unexpect "reserved"
              else pipeline
          )
  in
  succeed (fun coms -> Ast.Compound coms)
    |= sep_end_by1 (symbol ';') aux

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