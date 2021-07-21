

(* abstract reactive program *)
type prg = DoStep of (Value.t list -> Value.t list * prg)
let step p = match p with DoStep p -> p
  

type ctx = RdbgEvent.t
type e = RdbgEvent.t

type 't prg_ldbg = 
    DoStep_ldbg of (ctx ->  't -> Value.t list ->
                    (ctx -> 't -> 't prg_ldbg -> Value.t list -> e) ->
                    (ctx -> 't -> e) -> (ctx -> 't -> string -> e) -> e)

let (step_ldbg : ctx -> 't -> 't prg_ldbg -> Value.t list 
     -> (ctx -> 't -> 't prg_ldbg -> Value.t list -> e)
     ->  (ctx -> 't -> e) -> (ctx -> 't -> string -> e) -> e) =
  fun ctx t p vl cont fail_cont -> 
    match p with DoStep_ldbg p -> p ctx t vl cont fail_cont
