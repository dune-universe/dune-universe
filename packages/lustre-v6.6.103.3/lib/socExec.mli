(* Time-stamp: <modified the 19/10/2016 (at 16:29) by Erwan Jahier> *)

exception AssertViolation of Lxm.t

(* The entry point for lus2lic -exec *)
val f : Lv6MainArgs.t -> Soc.tbl -> Soc.key -> unit


(* The entry point for lus2licRun (the ocaml API) *)
val soc_step : Soc.step_method -> 
               Soc.tbl -> Soc.t -> SocExecValue.ctx -> SocExecValue.ctx

