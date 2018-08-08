(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

  open Printtyp

  let make_type ppf f ?(with_pos=false) ty =
    let ty = if with_pos then TypeFix.type_expr ty else ty in
    f ppf ty

  let type_expr ?with_pos ppf = make_type ppf type_expr ?with_pos
  let type_sch ?with_pos ppf = make_type ppf type_sch ?with_pos
  let type_scheme ?with_pos ppf = make_type ppf type_scheme ?with_pos
  let modtype ?(with_pos=false) ppf mty = 
    let mty = if with_pos then TypeFix.module_type mty else mty in
    modtype ppf mty

