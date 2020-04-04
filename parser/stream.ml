module type S = sig
  module Elem : Elem.S
  module Pos : Pos.S
  type t
  val uncons : t -> (Elem.t * t) option
  val get_pos : t -> Pos.t
end

module Char = struct
  open Lazy_stream

  module Elem = Elem.Char
  module Pos = Pos.Char

  type t = Elem.t Lazy_stream.t * Pos.t

  let uncons (str, pos) =
    match uncons str with
    | None -> None
    | Some (elem, str') -> Some (elem, (str', (Pos.update pos elem)))

  let get_pos (_str, pos) = pos

  let of_string s = (of_string s, Pos.init)
end
