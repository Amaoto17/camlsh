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


let reserved = ["do"; "end"; "in"; "then"]

let is_reserved s = List.mem s reserved

let meta_single_quote = "\\'"
let meta_double_quote = "\\\"$"
let meta_glob = "\\\"'<>^|$,;(){} "
let meta_word = meta_glob ^ "*?[]"

let symbol c =
  char c
    |. spaces

let control_char =
  let escape = function
    | 'a' -> '\x07'
    | 'b' -> '\x08'
    | 'e' -> '\x1b'
    | 'f' -> '\x0c'
    | 'n' -> '\x0a'
    | 'r' -> '\x0d'
    | 't' -> '\x09'
    | 'v' -> '\x0b'
    | _ -> failwith "illegal control character"
  in
  in_class "abefnrtv" |> map escape

let escaped_char chars =
  one_of
    [ succeed identity
        |. char '\\'
        |= one_of
            [ in_class chars
            ; control_char
            ; unexpect "illegal backslash escape"
            ]
    ; not_in_class chars
    ]

let quoted_char chars =
  one_of
    [ succeed identity
        |. char '\\'
        |= one_of
            [ in_class chars
            ; succeed '\\'
            ]
    ; not_in_class chars
    ]

let word =
  one_of
    [ many1 (escaped_char meta_word)
        |> concat
    ; expect "word"
    ]

let keyword s =
  one_of
    [ backtrack &
        string s
          |. not_followed_by (not_in_class meta_word)
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

let array_ref node =
  one_of
    [ succeed (fun st ed -> Ast.Array_ref (node, st, ed))
        |. symbol '['
        |= int
        |= option
            ( succeed identity
                |. spaces
                |. string ".."
                |. spaces
                |= int
            )
        |. spaces
        |. char ']'
    ; succeed node
    ]

let single_quoted =
  succeed identity
    |. char '\''
    |= one_of
        [ many1 (quoted_char meta_single_quote)
            |> concat
        ; expect "quoted word"
        ]
    |. char '\''

let double_quoted =
  succeed (fun nodes -> Ast.Compound nodes)
    |. char '\"'
    |= many1
        ( one_of
            [ many1 (escaped_char meta_double_quote)
                |> concat
                |> map (fun s -> Ast.Word s)
            ; succeed (fun name -> Ast.Identifier name)
                |. char '$'
                |= identifier
                |> and_then array_ref
                |> map (fun node -> Ast.Quoted_ident node)
            ]
        )
    |. char '\"'

let word_elem =
  succeed (fun s -> Ast.Word s)
    |= one_of
        [ single_quoted
        ; word
        ]

let glob_pattern =
  many1 (escaped_char meta_glob)
    |> concat

let glob =
  one_of
    [ word_elem
    ; succeed (fun pat -> Ast.Glob pat)
        |= glob_pattern
    ]

let rec brace = fun st -> (|>) st &
  one_of
    [ succeed (fun nodes -> Ast.Brace nodes)
        |. symbol '{'
        |= sep_by1 (char ',' |. spaces) subst
        |. char '}'
    ; glob
    ]

and variable = fun st -> (|>) st &
  one_of
    [ succeed (fun name -> Ast.Identifier name)
        |. char '$'
        |= identifier
        |> and_then array_ref
    ; brace
    ]

and subst = fun st -> (|>) st &
  one_of
    [ succeed (fun comp -> Ast.Subst comp)
        |. symbol '('
        |= compound
        |. char ')'
        |> and_then array_ref
    ; variable
    ]

and elem = fun st -> (|>) st &
  succeed (fun nodes -> Ast.Elem nodes)
    |= many1
        ( one_of
            [ double_quoted
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
        |. backtrack
            ( char '^'
                |. not_followed_by (char '|')
            )
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
            ; succeed (fun ident values body -> Ast.For (ident, values, body))
                |. keyword "for"
                |= elem
                |. keyword "in"
                |= many elem
                |. symbol ';'
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
    one_of
      [ succeed (fun left right -> Ast.Pipe_err (left, right))
          |. string "^|"
      ; succeed (fun left right -> Ast.Pipe (left, right))
          |. char '|'
      ]
      |. spaces
  in
  chainl conditional op

and compound = fun st -> (|>) st &
  let check_reserved = function
    | Ast.Word s ->
        if is_reserved s then unexpect "reserved"
        else pipeline
    | _ ->
        expect "word"
  in
  let aux =
    one_of
      [ look_ahead word_elem
          |> and_then check_reserved
      ; succeed Ast.Null
      ]
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