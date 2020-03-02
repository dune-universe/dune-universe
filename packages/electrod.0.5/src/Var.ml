(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Provides fresh identifiers for variables (in formulas) at every stage. *)

(** type of an identifier *)
type t =
  { id : int  (** [id] identifies an identifier uniquely *)
  ; name : string
        (** [name] is a base string used to give a 
                                    human-friendly display *)
  ; sep : string
  ; loc : Location.t option }

let fresh =
  let c = ref 0 in
  fun ?(sep = "/") ?loc s ->
    assert (!c < max_int);
    let res = {id = !c; name = s; sep; loc} in
    incr c;
    res


let fresh_copy var = fresh @@ var.name

let fresh_of_raw_ident ?(sep = "/") v =
  fresh ~sep ~loc:(Raw_ident.location v) (Raw_ident.basename v)


let compare id1 id2 = CCInt.compare id1.id id2.id

let equal {id = id1; _} {id = id2; _} = id1 = id2

let style = `Yellow

let pp out {id; name; sep; _} =
  Fmtc.pf
    out
    "%a%a%a"
    Fmtc.(styled style string)
    name
    Fmtc.(styled style string)
    sep
    Fmtc.(styled style int)
    id


module P = Intf.Print.Mixin (struct
  type nonrec t = t

  let pp = pp
end)

include P
