module Make(Parser : Prim.S) : sig
  open Parser

  val between : 'a t -> 'b t -> 'c t -> 'c t
  val look_ahead : 'a t -> 'a t
  val not_followed_by : Str.Elem.t t -> unit t
  val eof : unit t
  val option : 'a t -> 'a option t
  val option_with : 'a -> 'a t -> 'a t
  val option_skip : 'a t -> unit t
  val many1 : 'a t -> 'a list t
  val skip_many1: 'a t -> unit t
  val sep_by : 'a t -> 'b t -> 'b list t
  val sep_by1 : 'a t -> 'b t -> 'b list t
  val end_by : 'a t -> 'b t -> 'b list t
  val end_by1 : 'a t -> 'b t -> 'b list t
  val sep_end_by : 'a t -> 'b t -> 'b list t
  val sep_end_by1 : 'a t -> 'b t -> 'b list t
end
