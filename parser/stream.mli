module type S = sig
  module Elem : Elem.S
  module Pos : Pos.S
  type t
  val uncons : t -> (Elem.t * t) option
  val get_pos : t -> Pos.t
end

module Char : sig
  include S with module Elem = Elem.Char
  val of_string : string -> t
end
