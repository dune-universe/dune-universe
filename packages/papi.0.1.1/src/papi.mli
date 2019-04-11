(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

(** Performance Application Programming Interface (PAPI) bindings.

    This module binds [<papi.h>]. PAPI provides portable access to hardware
    performance counters. For more information, see the
    {{: http://icl.cs.utk.edu/papi/}homepage}.

    {b Note.} All functions in this module except {!shutdown} raise {!Error}
    whenever the underlying PAPI call signals an error.

    {b Note.} All functions except {!init} and {!shutdown} raise
    [Error (ENOINIT, _)] before initialisation.

    For examples of use, consult {{!examples}examples}.

    {e v0.1.1 — {{:https://github.com/pqwy/ocaml-papi }homepage}} *)


(** {1 Errors} *)

type error =
  EINVAL | ENOMEM | ESYS | ECMP | ECLOST | EBUG | ENOEVNT | ECNFLCT | ENOTRUN
| EISRUN | ENOEVST | ENOTPRESET | ENOCNTR | EMISC | EPERM | ENOINIT | ENOCMP
| ENOSUPP | ENOIMPL | EBUF | EINVAL_DOM | EATTR | ECOUNT | ECOMBO
(** PAPI errors.

    See the header file [papi.h] for descriptions of errors. *)

exception Error of (error * string)
(** PAPI errors are signalled by raising [Error ((err, fname))],
    where [fname] is the name of the failing function. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf err] pretty-prints [err] on [ppf]. *)

val pp_exn_error : Format.formatter -> error * string -> unit
(** [pp_exn_error ppf arg] pretty-prints the [Error] argument [arg] on [ppf]. *)


(** {1 Initialisation} *)

external init : unit -> unit = "caml_papi_library_init"
(** Initializes PAPI by calling [PAPI_library_init].
    Idempotent: repeated calls are ignored. *)

external shutdown : unit -> unit = "caml_papi_shutdown"
(** Releases PAPI state by calling [PAPI_shutdown]. Idempotent. *)

external hw_counters : unit -> int = "caml_papi_hw_counters"
(** [hw_counters ()] is the number of available hardware counters. *)


(** {1 Events} *)

type event =
  L1_DCM | L1_ICM | L2_DCM | L2_ICM | L3_DCM | L3_ICM | L1_TCM | L2_TCM
| L3_TCM | CA_SNP | CA_SHR | CA_CLN | CA_INV | CA_ITV | L3_LDM | L3_STM
| BRU_IDL | FXU_IDL | FPU_IDL | LSU_IDL | TLB_DM | TLB_IM | TLB_TL | L1_LDM
| L1_STM | L2_LDM | L2_STM | BTAC_M | PRF_DM | L3_DCH | TLB_SD | CSR_FAL
| CSR_SUC | CSR_TOT | MEM_SCY | MEM_RCY | MEM_WCY | STL_ICY | FUL_ICY
| STL_CCY | FUL_CCY | HW_INT | BR_UCN | BR_CN | BR_TKN | BR_NTK | BR_MSP
| BR_PRC | FMA_INS | TOT_IIS | TOT_INS | INT_INS | FP_INS | LD_INS | SR_INS
| BR_INS | VEC_INS | RES_STL | FP_STAL | TOT_CYC | LST_INS | SYC_INS | L1_DCH
| L2_DCH | L1_DCA | L2_DCA | L3_DCA | L1_DCR | L2_DCR | L3_DCR | L1_DCW
| L2_DCW | L3_DCW | L1_ICH | L2_ICH | L3_ICH | L1_ICA | L2_ICA | L3_ICA
| L1_ICR | L2_ICR | L3_ICR | L1_ICW | L2_ICW | L3_ICW | L1_TCH | L2_TCH
| L3_TCH | L1_TCA | L2_TCA | L3_TCA | L1_TCR | L2_TCR | L3_TCR | L1_TCW
| L2_TCW | L3_TCW | FML_INS | FAD_INS | FDV_INS | FSQ_INS | FNV_INS | FP_OPS
| SP_OPS | DP_OPS | VEC_SP | VEC_DP | REF_CYC
(** PAPI [PRESET] events.

    The header file [papiStdEventDefs.h], installed by PAPI, is the
    authoritative description of events.

    Another way to obtain event descriptions is to call {!description}, or
    pretty-print them with {!pp_event}. *)

external name : event -> string = "caml_papi_event_name"
(** [name e] is a human-readable name for [e].

    It returns [PAPI_event_info_t.name], without the prefix ["PAPI_"]. *)

external description : event -> string = "caml_papi_event_descr"
(** [description e] is a human-readable description of [e].

    It returns [PAPI_event_info_t.long_descr]. *)

external query : event -> bool = "caml_papi_query_event"
(** [query e] is [true] iff the hardware supports the event [e]. *)

val pp_event : Format.formatter -> event -> unit
(** [pp_event ppf e] pretty-prints a human-readable description on [e] on [ppf]. *)

val events : event array
(** [events] contains all defined {{!event}events}. *)

(** {1 Event sets} *)

type eventset
(** Sets of events.

    {b Note.} Eventsets are handles to resources held by PAPI. The handles are
    recycled. Calls to {!destroy} followed by {!create} can therefore return
    handles identical to previously destroyed handles, making destroyed
    eventsets {e live} again.
    *)

external create : unit -> eventset = "caml_papi_create_eventset"
(** [create ()] is a new {!eventset} [es].

    Calls [PAPI_create_eventset]. *)

external cleanup : eventset -> unit = "caml_papi_cleanup_eventset"
(** [cleanup es] removes counters from [es].

    Calls [PAPI_cleanup_eventset]. *)

external destroy : eventset -> unit = "caml_papi_destroy_eventset"
(** [destroy es] releases the resources backing [es].

    Calls [PAPI_destroy_eventset]. *)

external add : eventset -> event -> unit = "caml_papi_add_event"
(** [add es e] adds the event [e] to [es].

    Calls [PAPI_add_event]. *)

external num_events : eventset -> int = "caml_papi_num_events"
(** [num_events es] is the number of {{!event}events} currently attached to
    [es]. *)

external start : eventset -> unit = "caml_papi_start"
(** [start es] starts counting the events in [es].

    Calls [PAPI_start]. *)

external stop : eventset -> unit = "caml_papi_stop"
(** [stop es] stops counting the events in [es].

    Calls [PAPI_stop]. *)

external reset : eventset -> unit = "caml_papi_reset"
(** [reset es] resets the counters of events in [es].

    Calls [PAPI_reset]. *)

external read : eventset -> ?off:int -> float array -> unit = "caml_papi_read"
(** [read es ~off values] [es]' event counters to [values].

    These values are written to [values.(off), ..., values.(off + n - 1)] where
    [n] is [num_events es]. [off] defaults to [0].

    Calls [PAPI_read]. *)

external accum : eventset -> ?off:int -> float array -> unit = "caml_papi_accum"
(** [accum es ~off values] adds [es]' event counters to [values] and resets
    them.

    These values are written to [values.(off), ..., values.(off + n - 1)] where
    [n] is [num_events es]. [off] defaults to [0].

    Calls [PAPI_accum]. *)


(** {1:examples Examples}

    Read the TSC and the actual number of cycles:
{[
open Papi

let _ = init ()
let _ =
  let es = create ()
  and vs = Array.create_float 2 in
  List.iter (add es) [REF_CYC; TOT_CYC];
  start es;
  long_running_fun ();
  read es vs;
  stop es; cleanup es; destroy es;
  Fmt.pr "reference: %f, total: %f\n" vs.(0) vs.(1)
]}

    Create bracket that reads a set of events:

{[
open Papi

let _ = init ()
let count_events ~events f =
  let es = create ()
  and vs = Array.create_float (List.length events) in
  List.iter (add es) events;
  start es;
  let res = f () in
  read es vs;
  stop es; cleanup es; destroy es;
  (res, vs)
]}

*)

(* external description : event -> string = "caml_papi_event_descr" *)
(* external short_description : event -> string = "caml_papi_event_short_descr" *)
