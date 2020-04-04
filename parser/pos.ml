open Internal

module type S = sig
  type t
  val show : t -> string
  val init : t
end

module Char = struct
  type t = { row : int; column : int }
  let show t = !% "%d:%d" t.row t.column
  let init = { row = 1; column = 1 }
  let update t = function
    | '\n' -> { row = t.row + 1; column = 1 }
    | _ -> { t with column = t.column + 1 }
end
