type t
and frame

val push_frame : ?iteration:int * int * string list -> t -> unit
val pop_frame : t -> unit
val pop_frame_all : t -> unit

val new_env : t -> unit
val delete_env : t -> unit
val delete_env_all : t -> unit
val find : t -> string -> string array option
val get_status : t -> int
val set_status : t -> int -> unit
val set_local : t -> string -> string array -> unit
val set_global : t -> string -> string array -> unit

val reset_redir : t -> unit
val set_stdin : t -> Unix.file_descr -> unit
val set_stdout : t -> Unix.file_descr -> unit
val set_stderr : t -> Unix.file_descr -> unit
val do_redirection : t -> unit
val restore : t -> unit
val safe_redirection : t -> unit

val push : t -> string -> unit
val pop : t -> string
val pop_all : t -> string array

val add_empty : t -> unit
val add_string : t -> string -> unit
val add_string_list : t -> string list -> unit
val take_string : t -> string list
val concat_string : t -> string list
val emit_string : t -> string list

val loop_start : t -> int option
val loop_end : t -> int option
val take_iter_val : t -> string array option

val create : unit -> t
val show : t -> string
val init : t -> unit
val reset_all : t -> unit