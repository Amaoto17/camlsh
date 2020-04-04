open Internal
open Util

module Make (Parser : Char.S) = struct
  module Combinator = Combinator.Make(Parser)
  open Parser
  open Combinator

  let digit_float c =
    Stdlib.Char.code c - 48 |> float_of_int

  let digits =
    many1 digit |> concat

  let decimal =
    map int_of_string digits

  let natural =
        decimal
    <|> expect "natural"

  let signed f p =
    let sign =
      one_of
        [ succeed f
            |. char '-'
        ; succeed identity
            |. char '+'
        ; succeed identity
        ]
    in
    succeed (&)
      |= sign
      |= p

  let int =
        signed (~-) natural
    <|> expect "integer"

  let nat_float =
    map float_of_string digits

  let fraction =
    succeed identity
      |. char '.'
      |= many digit
      |> map
          ( fun cs ->
              let f d n = (n +. digit_float d) /. 10.0 in
              List.fold_right f cs 0.0
          )

  let exponent =
    succeed identity
      |. insensitive_char 'e'
      |= signed (~-.) nat_float
      |> map (fun n -> 10.0 ** n)

  let fract_exp n =
    one_of
      [ succeed (fun fract exp -> (n +. fract) *. exp)
          |= fraction
          |= option_with 1.0 exponent
      ; succeed (fun exp -> n *. exp)
          |= exponent
      ; expect "fraction or exponent"
      ]

  let float =
        signed (~-.) (nat_float |> and_then fract_exp)
    <|> expect "float"

  let chainl p op =
    let rec loop acc =
      one_of
        [ succeed (fun f x -> f acc x)
            |= op
            |= p
            |> and_then loop
        ; succeed acc
        ]
    in
    p |> and_then loop

  let chainr p op =
    let rec loop acc =
      one_of
        [ succeed (fun f x -> f acc x)
            |= op
            |= (p |> and_then loop)
        ; succeed acc
        ]
    in
    p |> and_then loop
end
