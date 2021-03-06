external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (!%) = Printf.sprintf


module Deco = struct
  type color = Red | Green | Yellow | Magenta | Cyan | Gray

  let colorize color s =
    match color with
    | Red     -> !% "\x1b[31m%s\x1b[m" s
    | Green   -> !% "\x1b[32m%s\x1b[m" s
    | Yellow  -> !% "\x1b[33m%s\x1b[m" s
    | Magenta -> !% "\x1b[35m%s\x1b[m" s
    | Cyan    -> !% "\x1b[36m%s\x1b[m" s
    | Gray    -> !% "\x1b[2;37m%s\x1b[m" s
end


module Arraybuffer = struct
  type 'a t =
    { mutable buffer : 'a array
    ; mutable capacity : int
    ; mutable length : int
    }

  let create n elem =
    let n = if n < 0 then 1 else n in
    { buffer = Array.make n elem
    ; capacity = n
    ; length = 0
    }

  let length t =
    t.length

  let set t n elem =
    t.buffer.(n) <- elem

  let contents t =
    Array.sub t.buffer 0 t.length

  let resize t len =
    let new_cap = ref t.capacity in
    while t.capacity + len > !new_cap do
      new_cap := !new_cap * 2
    done;
    let new_buf = Array.make !new_cap t.buffer.(0) in
    Array.blit new_buf 0 t.buffer 0 t.length;
    t.buffer <- new_buf;
    t.capacity <- !new_cap

  let add t elem =
    if t.length >= t.capacity then resize t 1;
    t.buffer.(t.length) <- elem;
    t.length <- t.length + 1

  let append t1 t2 =
    if t1.length + t2.length > t1.capacity then resize t1 t2.length;
    Array.blit t1.buffer t1.length t2.buffer 0 t2.length
end

module Env = struct
  type 'a t =
    { vars : (string, 'a) Hashtbl.t
    ; parent : 'a t option
    ; outer : 'a t option
    }

  let create () =
    { vars = Hashtbl.create 64
    ; parent = None
    ; outer = None
    }

  let new_env t =
    let env = create () in
    { env with parent = Some t; outer = t.outer }

  let delete_env t = t.parent

  let put_env t =
    let env = create () in
    { env with parent = None; outer = Some t }

  let rec find t key =
    match Hashtbl.find_opt t.vars key with
    | Some v -> Some v
    | None ->
        match t.parent with
        | Some env -> find env key
        | None ->
            match t.outer with
            | Some env -> find env key
            | None -> None

  let set t key v =
    match find t key with
    | None -> Hashtbl.add t.vars key v
    | Some _ -> Hashtbl.replace t.vars key v
end