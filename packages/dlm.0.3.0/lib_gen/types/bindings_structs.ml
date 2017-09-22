(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.md.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Ctypes
module Make(S: Cstubs_structs.TYPE) = struct
  open S
  module Dlm_lksb = struct
    type dlm_lksb
    type t = dlm_lksb structure
    let t : t typ = structure "dlm_lksb"

    let sb_status = field t "sb_status" int
    let sb_lkid = field t "sb_lkid" uint32_t
    let sb_flags = field t "sb_flags" char
    let sb_lvbptr = field t "sb_lvbptr" char
    let () = seal t
  end

  module Mode = struct
    let lkm_nlmode = constant "LKM_NLMODE" uint32_t
    let lkm_crmode = constant "LKM_CRMODE" uint32_t
    let lkm_cwmode = constant "LKM_CWMODE" uint32_t
    let lkm_prmode = constant "LKM_PRMODE" uint32_t
    let lkm_pwmode = constant "LKM_PWMODE" uint32_t
    let lkm_exmode = constant "LKM_EXMODE" uint32_t
  end

  module Flags = struct
    let lkf_noqueue = constant "LKF_NOQUEUE" uint32_t
    let lkf_convert = constant "LKF_CONVERT" uint32_t
    let lkf_valblk = constant "LKF_VALBLK" uint32_t
    let lkf_queucvt = constant "LKF_QUECVT" uint32_t
    let lkf_expedite = constant "LKF_EXPEDITE" uint32_t
    let lkf_persistent = constant "LKF_PERSISTENT" uint32_t
    let lkf_convdeadlk = constant "LKF_CONVDEADLK" uint32_t
    let lkf_nodlckwt = constant "LKF_NODLCKWT" uint32_t
    let lkf_nodlckblk = constant "LKF_NODLCKBLK" uint32_t
    let lkf_noqueuebast = constant "LKF_NOQUEUEBAST" uint32_t
    let lkf_headque = constant "LKF_HEADQUE" uint32_t
    let lkf_noorder = constant "LKF_NOORDER" uint32_t
    let lkf_altpr = constant "LKF_ALTPR" uint32_t
    let lkf_altcw = constant "LKF_ALTCW" uint32_t
    let lkf_timeout = constant "LKF_TIMEOUT" uint32_t
    let lkf_wait = constant "LKF_WAIT" uint32_t
  end

  let dlm_lsfl_timewarn = constant "DLM_LSFL_TIMEWARN" uint32_t
end
