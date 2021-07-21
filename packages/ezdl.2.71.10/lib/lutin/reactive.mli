

(* abstract reactive program *)
type prg = DoStep of (Value.t list -> Value.t list * prg)
val step : prg -> Value.t list -> (Value.t list * prg)

type ctx = RdbgEvent.t
type e = RdbgEvent.t


type 't prg_ldbg = 
    DoStep_ldbg of (ctx ->  't -> Value.t list ->
                    (ctx -> 't -> 't prg_ldbg -> Value.t list -> e) ->
                    (ctx -> 't -> e) -> (ctx -> 't -> string -> e) -> e)

val step_ldbg : ctx -> 't -> 't prg_ldbg -> Value.t list 
                 -> (ctx -> 't -> 't prg_ldbg -> Value.t list -> e)
                 ->  (ctx -> 't -> e) -> (ctx -> 't -> string -> e) -> e



