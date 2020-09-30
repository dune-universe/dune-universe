(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 CNRS, Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains a parser combinator library for the OCaml lang-
  uage. It is intended to be used in conjunction with pa_ocaml (an OCaml
  parser and syntax extention mechanism) to provide  a  fully-integrated
  way of building parsers using an extention of OCaml's syntax.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute the software under the terms of the CeCILL-
  B license as circulated by CEA, CNRS and INRIA at the following URL.

      http://www.cecill.info

  As a counterpart to the access to the source code and  rights to copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty  and the software's author, the holder of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

open Earley_core
open Earley

let blank_regexp s =
  let re = Str.regexp s in
  let accept_newline = Str.string_match re "\n" 0 && Str.match_end () = 1 in
  let rec fn str pos =
    let (str, pos) = Input.normalize str pos in
    let l = Input.line str in
    if Str.string_match re l pos then
      let pos = Str.match_end () in
      let len = String.length l in
      if accept_newline && pos = len && not (Input.is_empty str pos) then
        fn str pos
      else (str, pos)
    else (str, pos)
  in fn

let regexp : ?name:string -> string -> ((int -> string) -> 'a) -> 'a grammar =
  fun ?name re  a ->
    let r = Str.regexp re in
    let name =
      match name with
      | None      -> String.escaped re
      | Some name -> name
    in
    let set = Charset.copy Charset.empty in
    let found = ref false in
    for i = 0 to 254 do
      let c = Char.chr i in
      let s = String.make 1 c in
      if Str.string_partial_match r s 0 && Str.match_end () > 0 then
        begin
          found := true;
          Charset.addq set c
        end
    done;
    if not !found then failwith "regexp: illegal empty regexp";
    let fn buf pos =
      let l = Input.line buf in
      if pos >= String.length l then give_up ();
      if Str.string_match r l pos then
        let f n = Str.matched_group n l in
        let pos' = Str.match_end () in
        if pos' = pos then give_up ();
        let res = a f in
        (res, buf, pos')
      else give_up ()
    in
    if Str.string_match r "" 0 then
      let f n = Str.matched_group n "" in
      option (a f) (black_box fn set false name)
    else
      black_box fn set false name
