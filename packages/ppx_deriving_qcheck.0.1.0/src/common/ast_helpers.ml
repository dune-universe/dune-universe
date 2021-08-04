(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Ppxlib

(* Module providing default values in order to build Ppxlib.Ast *)
module Default = struct
  let loc_stack = function Some x -> x | None -> []

  let attributes = function Some x -> x | None -> []

  let loc = function Some x -> x | None -> Location.none

  let default_pat =
    {
      ppat_desc = Ppat_any;
      ppat_loc = loc None;
      ppat_loc_stack = loc_stack None;
      ppat_attributes = attributes None;
    }

  let pat = function Some x -> x | None -> default_pat

  let flag = function Some x -> x | None -> Nonrecursive

  let bindings = function Some x -> x | None -> []
end

module Expression = struct
  let expression ?loc ?loc_stack ?attributes desc =
    let loc_stack = Default.loc_stack loc_stack in
    let attributes = Default.attributes attributes in
    let loc = Default.loc loc in
    {
      pexp_desc = desc;
      pexp_loc = loc;
      pexp_loc_stack = loc_stack;
      pexp_attributes = attributes;
    }

  let unit ?loc () =
    let loc = Default.loc loc in
    [%expr ()]

  let pexp_let ?loc ?loc_stack ?attributes ?flag ?bindings exp =
    let flag = Default.flag flag in
    let bindings = Default.bindings bindings in
    expression ?loc ?loc_stack ?attributes @@ Pexp_let (flag, bindings, exp)

  let pexp_string ?loc ?loc_stack ?attributes str =
    let default_loc = Default.loc loc in
    expression
      ?loc
      ?loc_stack
      ?attributes
      (Pexp_constant (Pconst_string (str, default_loc, None)))

  let pexp_list ?loc xs =
    let loc = Default.loc loc in
    let rec aux loc = function
      | [] -> [%expr []]
      | x :: xs -> [%expr [%e x] :: [%e aux loc xs]]
    in
    aux loc xs

  let pexp_apply ?loc ?loc_stack ?attributes ~f ~args () =
    expression ?loc ?loc_stack ?attributes @@ Pexp_apply (f, args)

  let pexp_tuple ?loc ?loc_stack ?attributes = function
    | [ x ] -> x
    | xs -> expression ?loc ?loc_stack ?attributes @@ Pexp_tuple xs

  let pexp_construct ?loc ?loc_stack ?attributes ~kname ~kargs () =
    expression ?loc ?loc_stack ?attributes @@ Pexp_construct (kname, kargs)

  let pexp_ident ?loc ?loc_stack ?attributes x =
    expression ?loc ?loc_stack ?attributes @@ Pexp_ident x

  let pexp_lident ?loc ?loc_stack ?attributes x =
    let loc' = Default.loc loc in
    expression ?loc ?loc_stack ?attributes
    @@ Pexp_ident { txt = Lident x; loc = loc' }

  let pexp_record ?loc ?loc_stack ?attributes ~fields x =
    expression ?loc ?loc_stack ?attributes @@ Pexp_record (fields, x)

  let pexp_variant ?loc ?loc_stack ?attributes ~label x =
    expression ?loc ?loc_stack ?attributes @@ Pexp_variant (label, x)

  let pexp_constraint ?loc ?loc_stack ?attributes e ct =
    expression ?loc ?loc_stack ?attributes @@ Pexp_constraint (e, ct)
end

module Type = struct
  let core_type ?loc ?loc_stack ?attributes desc =
    let loc_stack = Default.loc_stack loc_stack in
    let attributes = Default.attributes attributes in
    let loc = Default.loc loc in

    {
      ptyp_desc = desc;
      ptyp_loc = loc;
      ptyp_loc_stack = loc_stack;
      ptyp_attributes = attributes;
    }

  let constr_one ?loc ?loc_stack ?attributes x y =
    let loc' = Default.loc loc in
    let x_lgloc = { loc = loc'; txt = x } in
    let y_lgloc = { loc = loc'; txt = y } in
    let y =
      core_type ?loc ?loc_stack ?attributes @@ Ptyp_constr (y_lgloc, [])
    in
    core_type ?loc ?loc_stack ?attributes @@ Ptyp_constr (x_lgloc, [ y ])
end

module Pattern = struct
  let pattern ?loc ?loc_stack ?attributes desc =
    let loc_stack = Default.loc_stack loc_stack in
    let attributes = Default.attributes attributes in
    let loc = Default.loc loc in

    {
      ppat_desc = desc;
      ppat_loc = loc;
      ppat_loc_stack = loc_stack;
      ppat_attributes = attributes;
    }

  let ppat_var ?loc ?loc_stack ?attributes s =
    let default_loc = Default.loc loc in
    pattern ?loc ?loc_stack ?attributes
    @@ Ppat_var { txt = s; loc = default_loc }

  let ppat_any ?loc () = pattern ?loc @@ Ppat_any

  let extract_pat_name_opt pat =
    match pat.ppat_desc with Ppat_var { txt = s; _ } -> Some s | _ -> None

  let extract_pat_name_exn ~loc pat =
    match extract_pat_name_opt pat with
    | Some s -> s
    | None ->
        Error.case_unsupported ~loc ~case:"Could not extract pattern name" ()
end

module Structure = struct
  let value_binding ?loc ?pat ?attributes expr =
    let pat = Default.pat pat in
    let attributes = Default.attributes attributes in
    let loc = Default.loc loc in
    {
      pvb_pat = pat;
      pvb_expr = expr;
      pvb_attributes = attributes;
      pvb_loc = loc;
    }

  let structure_item ?loc x =
    let loc = Default.loc loc in
    { pstr_desc = x; pstr_loc = loc }

  let str_include ?loc xs =
    let loc' = Default.loc loc in
    let include_infos x =
      { pincl_mod = x; pincl_loc = loc'; pincl_attributes = [] }
    in

    let x =
      { pmod_desc = Pmod_structure xs; pmod_loc = loc'; pmod_attributes = [] }
    in
    structure_item ?loc @@ Pstr_include (include_infos x)
end
