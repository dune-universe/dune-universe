(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(************************************************************************)
(* Coq serialization API/Plugin                                         *)
(* Copyright 2016-2019 MINES ParisTech -- Dual License LGPL 2.1 / GPL3+ *)
(* Written by: Emilio J. Gallego Arias                                  *)
(************************************************************************)
(* Status: Very Experimental                                            *)
(************************************************************************)

open Sexplib.Conv

module type ExtS = sig

  include CSig.MapS

  (* module SSet : Ser_cSet.ExtS *)

  include SerType.S1 with type 'a t := 'a t

end

module Make (M : CSig.MapS) (S : SerType.S with type t := M.key) = struct

  include M

  (* module SSet = Ser_cSet.Make(M.Set)(S) *)

  let sexp_of_t f cst =
    sexp_of_list (Sexplib.Conv.sexp_of_pair S.sexp_of_t f) M.(bindings cst)

  let t_of_sexp f sexp =
    List.fold_left (fun e (k,s) -> M.add k s e) M.empty
      (list_of_sexp (pair_of_sexp S.t_of_sexp f) sexp)

end
