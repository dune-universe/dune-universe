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

type 'a code = {

  (* A document that represents the value [value] as an OCaml expression. *)
  ocaml : PPrint.document Lazy.t;

  (* An actual value of type ['a]. *)
  value : 'a;

}

let code ocaml value =
  { value; ocaml }

let ocaml c =
  c.ocaml

let value c =
  c.value

let print c =
  Lazy.force (ocaml c)

let map f { ocaml; value } =
  { ocaml; value = f value }
