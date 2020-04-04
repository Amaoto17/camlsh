module type S = sig
  type t
  val show : t -> string
  val init : t
end

module Char : sig
  include S
  val update : t -> char -> t
end