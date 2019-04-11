(* Copyright (c) 2019 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md *)

(* Ordering must match the ordering of `error_table[]'! *)
type error =
  EINVAL | ENOMEM | ESYS | ECMP | ECLOST | EBUG | ENOEVNT | ECNFLCT | ENOTRUN
| EISRUN | ENOEVST | ENOTPRESET | ENOCNTR | EMISC | EPERM | ENOINIT | ENOCMP
| ENOSUPP | ENOIMPL | EBUF | EINVAL_DOM | EATTR | ECOUNT | ECOMBO

exception Error of (error * string)
let () = Callback.register_exception "PAPI_EXCEPTION_CTOR" (Error (EMISC, ""));

(* Ordering must match the ordering of `event_table[]'! *)
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

let events = [|
  L1_DCM; L1_ICM; L2_DCM; L2_ICM; L3_DCM; L3_ICM; L1_TCM; L2_TCM; L3_TCM;
  CA_SNP; CA_SHR; CA_CLN; CA_INV; CA_ITV; L3_LDM; L3_STM; BRU_IDL; FXU_IDL;
  FPU_IDL; LSU_IDL; TLB_DM; TLB_IM; TLB_TL; L1_LDM; L1_STM; L2_LDM; L2_STM;
  BTAC_M; PRF_DM; L3_DCH; TLB_SD; CSR_FAL; CSR_SUC; CSR_TOT; MEM_SCY; MEM_RCY;
  MEM_WCY; STL_ICY; FUL_ICY; STL_CCY; FUL_CCY; HW_INT; BR_UCN; BR_CN; BR_TKN;
  BR_NTK; BR_MSP; BR_PRC; FMA_INS; TOT_IIS; TOT_INS; INT_INS; FP_INS; LD_INS;
  SR_INS; BR_INS; VEC_INS; RES_STL; FP_STAL; TOT_CYC; LST_INS; SYC_INS; L1_DCH;
  L2_DCH; L1_DCA; L2_DCA; L3_DCA; L1_DCR; L2_DCR; L3_DCR; L1_DCW; L2_DCW;
  L3_DCW; L1_ICH; L2_ICH; L3_ICH; L1_ICA; L2_ICA; L3_ICA; L1_ICR; L2_ICR;
  L3_ICR; L1_ICW; L2_ICW; L3_ICW; L1_TCH; L2_TCH; L3_TCH; L1_TCA; L2_TCA;
  L3_TCA; L1_TCR; L2_TCR; L3_TCR; L1_TCW; L2_TCW; L3_TCW; FML_INS; FAD_INS;
  FDV_INS; FSQ_INS; FNV_INS; FP_OPS; SP_OPS; DP_OPS; VEC_SP; VEC_DP; REF_CYC
|]

type eventset = int
external init    : unit -> unit = "caml_papi_library_init"
external shutdown : unit -> unit = "caml_papi_shutdown"
external hw_counters : unit -> int = "caml_papi_hw_counters"
external create  : unit -> eventset = "caml_papi_create_eventset"
external cleanup : eventset -> unit = "caml_papi_cleanup_eventset"
external destroy : eventset -> unit = "caml_papi_destroy_eventset"
external add : eventset -> event -> unit = "caml_papi_add_event"
external query : event -> bool = "caml_papi_query_event"
external num_events : eventset -> int = "caml_papi_num_events"
external start : eventset -> unit = "caml_papi_start"
external stop : eventset -> unit = "caml_papi_stop"
external reset : eventset -> unit = "caml_papi_reset"
external read : eventset -> ?off:int -> float array -> unit = "caml_papi_read"
external accum : eventset -> ?off:int -> float array -> unit = "caml_papi_accum"
external strerror : error -> string = "caml_papi_strerror"
external name  : event -> string = "caml_papi_event_name"
external description : event -> string = "caml_papi_event_descr"

let pf = Format.fprintf
(* let pp_eventset ppf = pf ppf "%d" *)
let pp_event ppf e = pf ppf "@[%s@ (%s)@]" (name e) (description e)
let pp_error ppf err =
  let descr = try strerror err with
    Error (ENOINIT, _) -> "Please call Papi.init" in
  Format.pp_print_string ppf descr
let pp_exn_error ppf (err, f) = pf ppf "@[%s:@ %a@]" f pp_error err
