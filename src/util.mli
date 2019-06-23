val (&) : ('a -> 'b) -> 'a -> 'b
val (!%) : ('a, unit, string) format -> 'a

module Deco : sig
  type color = Red | Green | Yellow | Magenta | Cyan | Gray

  val colorize : color -> string -> string
end

module Arraybuffer : sig
  type 'a t

  val create : int -> 'a -> 'a t
  val length : 'a t -> int
  val set : 'a t -> int -> 'a -> unit
  val contents : 'a t -> 'a array
  val add : 'a t -> 'a -> unit
end

module Env : sig
  type 'a t

  val create : unit -> 'a t
  val new_env : 'a t -> 'a t
  val delete_env : 'a t -> 'a t option
  val put_env : 'a t -> 'a t
  val find : 'a t -> string -> 'a option
  val set : 'a t -> string -> 'a -> unit
end