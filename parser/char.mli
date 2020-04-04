module type S = sig
  include Prim.S

  val char : char -> char t
  val insensitive_char : char -> char t
  val digit : char t
  val letter : char t
  val upper : char t
  val lower : char t
  val alpha_num : char t
  val in_class : string -> char t
  val not_in_class : string -> char t
  val concat : char list t -> string t
  val string : string -> string t
  val space : char t
  val spaces : unit t
end

module Make(Str : Stream.S with type Elem.t = char) : S with module Str = Str
