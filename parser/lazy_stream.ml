type 'a t = 'a node Lazy.t
and 'a node =
  | Nil
  | Cons of 'a * 'a t

let empty = lazy Nil

let cons x t = lazy (Cons (x, t))

let uncons = function
  | lazy Nil -> None
  | lazy (Cons (elem, str)) -> Some (elem, str)

let of_string s =
  let aux s =
    let len = Bytes.length s in
    let rec loop acc i =
      if i < 0 then acc
      else
        let x = Bytes.get s i in
        loop (cons x acc) (i - 1)
    in
    loop empty (len - 1)
  in
  Bytes.unsafe_of_string s |> aux