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

open PPrint

(* We could represent a piece of OCaml code simply as a string. We could do
   slightly better by representing it as a PPrint document, so as to ensure
   nice line breaks and indentation if the code is long. We could do better
   yet by introducing some form of delay (a function or suspension), so as to
   ensure that the document is actually constructed in memory only if it is
   actually needed. Better still, as icing on the cake, we represent a piece
   of code as a function of a list of documents to a document. Thus, if the
   code represents an OCaml function, and if it appears in the context of an
   OCaml function application, we can give it access to its actual arguments.
   This allows certain low-level optimizations, e.g., using infix syntax when
   a binary operator is applied to two arguments. *)

type appearance =
  document list -> document

type 'a code =
  'a * appearance

let constant c =
  (* Normal application syntax. *)
  let print docs = Print.apply (!^ c) docs in
  print

let document doc =
  (* Normal application syntax. *)
  let print docs = Print.apply doc docs in
  print

let infix op =
  let print docs =
    match docs with
    | [ arg1; arg2 ] ->
        (* Infix application syntax. *)
        Print.raw_apply [ arg1; !^ op; arg2 ]
    | _ ->
        (* Normal application syntax. *)
        Print.apply (parens (!^ op)) docs
  in
  print

let custom print =
  print

let value (v, _print) =
  v

let print (_v, print) =
  print

let apply print =
  print

let string c =
  let doc = print c [] in
  let b = Buffer.create 32 in
  PPrint.ToBuffer.compact b doc;
  Buffer.contents b
