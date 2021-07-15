(* Time-stamp: <modified the 16/03/2020 (at 11:37) by Erwan Jahier> *)


(* The entry points for lus2licRun (i.e., for ltop/ldbg) *)
val do_step : Soc.tbl -> Soc.t -> SocExecValue.ctx -> SocExecValue.ctx

val do_step_dbg : Soc.tbl -> Soc.t -> RdbgEvent.t -> SocExecValue.ctx ->
  (RdbgEvent.t -> SocExecValue.ctx -> RdbgEvent.t) -> RdbgEvent.t
