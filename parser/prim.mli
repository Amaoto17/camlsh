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

module Make (Str : Stream.S) : S with module Str = Str
