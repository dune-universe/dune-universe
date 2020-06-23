(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Spotlib.Spot
open Untyped
open Parsetree
open Typedtree
open Tools

(* attributes *)

let parse_options_in_payload ~loc name payload =match payload with
  | PStr str ->
      let parse { pstr_desc; pstr_loc=loc } =
        match pstr_desc with
        | Pstr_eval (_, _::_ ) ->
            errorf_attribute ~loc "%s attribute expression cannot take attributes" name
        | Pstr_eval (e, []) ->
            let rec parse { pexp_desc; pexp_loc=loc } = match pexp_desc with
              | Pexp_sequence (e1, e2) -> parse e1 @ parse e2
              | Pexp_apply ({ pexp_desc= Pexp_ident {txt=Longident.Lident "="} },
                            [ Nolabel, e1; Nolabel, e2 ]) ->
                  let key = match e1.pexp_desc with
                    | Pexp_ident lloc -> lloc
                    | _ -> errorf_attribute ~loc: e1.pexp_loc "%s attribute item key must be an identifier" name
                  in
                  let value = match e2.pexp_desc with
                    | Pexp_constant c -> `Constant c
                    | Pexp_construct ({txt=Longident.Lident "true"}, None) -> `Bool true
                    | Pexp_construct ({txt=Longident.Lident "false"}, None) -> `Bool false
                    | _ -> errorf_attribute ~loc: e2.pexp_loc "%s attribute item value must be a constant or a bool" name
                  in
                  [(key, value)]
              | _ -> 
                  errorf_attribute ~loc "%s attribute item must have a form key=value" name
            in
            parse e
        | _ -> errorf_attribute ~loc "%s attribute must have a form of key=value; ..; key_value" name
      in
      List.concat (List.map parse str)
  | PSig _ | PTyp _ | PPat _ -> errorf_attribute ~loc "%s attribute requires a structure" name

let get_scaml_toplevel_attributes str =
  let structure_item (st, nomore) { str_desc; _ } =
    match str_desc with
      | Tstr_attribute { attr_name= {txt=s; loc}; attr_payload= payload }
        when match String.lowercase_ascii s with "scaml" | "scam" -> true | _ -> false ->
          (* We use OCaml 4.09.1 but Ppxlib uses 4.08.1 *)
          let payload = Migrate.copy_payload payload in
          begin match s with
            | "Scaml" -> errorf_attribute ~loc "SCaml, not Scaml."
            | "scaml" -> errorf_attribute ~loc "SCaml, not scaml."
            | "scam" ->  errorf_attribute ~loc "SCaml: it's not a scam."
            | _ -> ()
          end;
          if nomore then errorf_attribute ~loc "SCaml attributes must appear at the head of the source file.";
          (loc, parse_options_in_payload ~loc "SCaml" payload) :: st, false
      | _ -> st, true (* we cannot have SCaml attributes later *)
  in
  let structure { str_items= sitems } =
    List.rev & fst & List.fold_left (fun st sitem -> structure_item st sitem) ([],false) sitems
  in
  structure str
