open Internal

module type S = sig
  module Str : Stream.S
  module Error : Error.S
  type error = Error.t
  type 'a t = Str.t -> ('a * Str.t, error) result

  val succeed : 'a -> 'a t
  val and_then : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val (|=) : ('a -> 'b) t -> 'a t -> 'b t
  val (|.) : 'a t -> 'b t -> 'a t
  val get : Str.t t
  val put : Str.t -> unit t
  val get_position : Str.Pos.t t
  val unexpect : string -> 'a t
  val expect : string -> 'a t
  val problem : string -> 'a t
  val (<|>) : 'a t -> 'a t -> 'a t
  val one_of : 'a t list -> 'a t
  val backtrack : 'a t -> 'a t
  val sequence : 'a t list -> 'a list t
  val count : int -> 'a t -> 'a list t
  val many : 'a t -> 'a list t
  val skip_many : 'a t -> unit t
  val token : (Str.Elem.t -> 'a option) -> 'a t
  val satisfy : (Str.Elem.t -> bool) -> Str.Elem.t t
  val any : Str.Elem.t t
  val run : 'a t -> Str.t -> ('a, error) result
end

module Make(Str : Stream.S) = struct
  module Str = Str
  module Error = Error.Make(Str.Pos)
  type error = Error.t
  type 'a t = Str.t -> ('a * Str.t, error) result

  let succeed x = fun st -> Ok (x, st)

  let and_then f p =
    fun st ->
      match p st with
      | Ok (a, st') -> (f a) st'
      | Error e -> Error e

  let map f p =
    p |> and_then (fun x -> succeed (f x))

  let (|=) p p' =
    p |> and_then (fun f -> map f p')

  let (|.) p p' =
    p |> and_then (fun x -> map (fun _ -> x) p')

  let get = fun st -> Ok (st, st)

  let put st = fun _ -> Ok ((), st)

  let get_position =
    get |> map Str.get_pos

  let fail e = fun _st -> Error e

  let failure reason =
    fun st ->
      let pos = Str.get_pos st in
      Error (Error.create reason pos)

  let unexpect s = failure (Unexpect s)

  let expect s = failure (Expect s)

  let problem s = failure (Problem s)

  let on_failure f p =
    fun st ->
      match p st with
      | Ok v -> Ok v
      | Error e -> (f e) st

  let (<|>) p p' =
    let aux pos e =
      if Error.get_pos e = pos then p' else fail e
    in
    get_position
      |> and_then
          ( fun pos ->
              p |> on_failure (aux pos)
          )

  let one_of ps =
    List.fold_left (<|>) (problem "empty one_of") ps

  let backtrack p =
    let aux pos e =
      fail (Error.set_pos e pos)
    in
    get_position
      |> and_then
          ( fun pos ->
              p |> on_failure (aux pos)
          )

  let sequence ps =
    let rec loop acc = function
      | [] -> succeed (List.rev acc)
      | p :: ps' ->
          succeed (fun x -> x :: acc)
            |= p
            |> and_then (fun acc' -> loop acc' ps')
    in
    loop [] ps

  let count n p =
    if n < 0 then succeed []
    else sequence (List.init n (fun _ -> p))

  let many_accum f init p =
    let rec loop acc =
      one_of
        [ succeed (fun x -> f x acc)
            |= p
            |> and_then loop
        ; succeed acc
        ]
    in
    loop init

  let many p =
    many_accum List.cons [] p
      |> map List.rev

  let skip_many p =
    many_accum (fun _ _ -> ()) () p

  let token test =
    let aux st =
      match Str.uncons st with
      | None -> unexpect "end of input"
      | Some (elem, st') ->
          match test elem with
          | None ->
              unexpect (!% "%s" (Str.Elem.show elem))
          | Some x ->
              succeed x
                |. put st'
    in
    get |> and_then aux

  let satisfy f =
    token (fun x -> if f x then Some x else None)

  let any = satisfy (fun _ -> true)

  let run p st =
    match p st with
    | Ok (res, _) -> Ok res
    | Error e -> Error e
end
