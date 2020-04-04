module type S = sig
  type t
  val show : t -> string
end

module Char : S with type t = char