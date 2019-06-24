exception Interruption
exception Exec_failure

val signal_init : unit -> unit

val execute : Ctx.t -> Inst.t array -> unit