(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

module Gen = Gen

module Print = Print

include Code
include Type
include Spec
include Builtin
include Engine

(* Re-package the functor [NewAbstractType] as a function; this is much more
   lightweight for the end user. Label its arguments, and make some of them
   optional. *)

let default_check : type r c . r -> (c -> unit) code =
  fun _ ->
    code
      (lazy (PPrint.string "(fun _ _ -> ())"))
      (fun _ -> ())

let default_var var name =
  match var with
  | Some var ->
      var
  | None ->
      (* Use the first letter of the type's name. *)
      String.sub name 0 1

let declare_abstract_type
  (type t1 t2)
  ?check:(check=default_check)
  ?var
  name
=
  if name = "" then
    invalid_arg "declare_abstract_type: name is empty";
  let var = default_var var name in
  let module T = NewAbstractType(struct
    type nonrec t1 = t1
    type nonrec t2 = t2
    let name = name
    let var = var
    let check = check
  end)
  in T.spec

(* Re-package the functor [NewConcreteType] as a function. *)

let default_print _ =
  PPrint.string "<no printer was supplied>"

let declare_concrete_type (type t)
  ?print:(print=default_print)
  ?equal
  name
=
  let module T = NewConcreteType(struct
    type nonrec t = t
    let name = name
    let print = print
    let equal = equal
  end)
  in T.spec
