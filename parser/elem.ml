open Internal

module type S = sig
  type t
  val show : t -> string
end

module Char = struct
  type t = char
  let show = !% "%C"
end
