open Internal
open Util

module Make(Parser : Prim.S) = struct
  open Parser

  let between open_p close_p p =
    succeed identity
      |. open_p
      |= p
      |. close_p

  let look_ahead p =
    get
      |> and_then
          ( fun st ->
              p
                |. put st
          )

  let not_followed_by p =
    backtrack &
      one_of
        [ p |> and_then (fun c -> unexpect (Str.Elem.show c))
        ; succeed ()
        ]

  let eof =
        not_followed_by any
    <|> expect "end of input"

  let option p =
    one_of
      [ succeed (fun x -> Some x)
          |= p
      ; succeed None
      ]

  let option_with default p =
    one_of
      [ p
      ; succeed default
      ]

  let option_skip p =
    one_of
      [ succeed ()
          |. p
      ; succeed ()
      ]

  let many1 p =
    succeed List.cons
      |= p
      |= many p

  let skip_many1 p =
    succeed ()
      |. p
      |. skip_many p

  let sep_by1 sep_p p =
    succeed List.cons
      |= p
      |= many
          ( succeed identity
              |. sep_p
              |= p
          )

  let sep_by sep_p p =
    one_of
      [ sep_by1 sep_p p
      ; succeed []
      ]

  let end_by1 end_p p =
    many1
      ( p
          |. end_p
      )

  let end_by end_p p =
    one_of
      [ end_by1 end_p p
      ; succeed []
      ]

  let sep_end_by1 sep_p p =
    let rec loop acc =
      one_of
        [ succeed identity
            |. sep_p
            |= one_of
                [ p |> and_then (fun x -> loop (x :: acc))
                ; succeed (List.rev acc)
                ]
        ; succeed (List.rev acc)
        ]
    in
    succeed List.cons
      |= p
      |= loop []

  let sep_end_by sep_p p =
    one_of
      [ sep_end_by1 sep_p p
      ; succeed []
      ]
end
