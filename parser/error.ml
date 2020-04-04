open Internal

module type S = sig
  module Pos : Pos.S

  type reason =
    | Unexpect of string
    | Expect of string
    | Problem of string

  type t

  val show : t -> string
  val create : reason -> Pos.t -> t
  val get_pos : t -> Pos.t
  val set_pos : t -> Pos.t -> t
end

module Make (P : Pos.S) = struct
  module Pos = P

  type reason =
    | Unexpect of string
    | Expect of string
    | Problem of string

  type t =
    { reason : reason
    ; pos : Pos.t
    }

  let reason_string = function
    | Unexpect s -> !% "unexpected %s" s
    | Expect s -> !% "expecting %s" s
    | Problem s -> s

  let show { reason; pos } =
    !% "%s %s" (Pos.show pos) (reason_string reason)

  let create reason pos = { reason = reason; pos = pos }

  let get_pos t = t.pos

  let set_pos t i = { t with pos = i }
end
