open Internal

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

module Make(Str : Stream.S with type Elem.t = char) = struct
  include Prim.Make(Str)

  let char c =
        satisfy ((=) c)
    <|> expect (!% "%C" c)

  let insensitive_char c =
    let lc = Stdlib.Char.lowercase_ascii c in
    let uc = Stdlib.Char.uppercase_ascii c in
        satisfy (fun c -> c = lc || c = uc)
    <|> expect (!% "%C or %C" lc uc)

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

  let digit =
        satisfy is_digit
    <|> expect "digit"

  let is_upper = function
    | 'A' .. 'Z' -> true
    | _ -> false

  let upper =
        satisfy is_upper
    <|> expect "uppercase letter"

  let is_lower = function
    | 'a' .. 'z' -> true
    | _ -> false

  let lower =
        satisfy is_lower
    <|> expect "lowercase letter"

  let is_letter c = is_upper c || is_lower c

  let letter =
        satisfy is_letter
    <|> expect "letter"

  let is_alpha_num =
    fun c -> is_digit c || is_letter c

  let alpha_num =
        satisfy is_alpha_num
    <|> expect "letter or digit"

  let in_class s =
    satisfy (String.contains s)

  let not_in_class s =
    satisfy (fun c -> not (String.contains s c))

  let concat p =
    p
      |> map
          ( fun cs ->
              let buf = Buffer.create (List.length cs) in
              List.iter (Buffer.add_char buf) cs;
              Buffer.contents buf
          )

  let take n =
    count n any |> concat

  let string s =
    backtrack &
      take (String.length s)
        |> and_then
            ( fun s' ->
                if s = s' then succeed s
                else expect (!% "%S" s)
            )

  let is_space = function
    | ' ' | '\n' | '\r' | '\t' -> true
    | _ -> false

  let space =
        satisfy is_space
    <|> expect "space"

  let spaces =
        skip_many space
    <|> expect "white space"
end
