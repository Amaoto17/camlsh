module Make(Parser : Char.S) : sig
  open Parser

  val natural : int t
  val int : int t
  val float : float t
  val chainl : 'a t -> ('a -> 'a -> 'a) t -> 'a t
  val chainr : 'a t -> ('a -> 'a -> 'a) t -> 'a t
end
