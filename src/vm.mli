exception Interruption

val signal_init : unit -> unit

val execute : Ctx.t -> Inst.t array -> unit