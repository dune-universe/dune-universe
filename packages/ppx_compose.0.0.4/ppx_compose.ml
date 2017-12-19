(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Migrate_parsetree
open Ast_404
let ocaml_version = Versions.ocaml_404

open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

(* Is there an existing function? *)
let fresh_var_for e =
  Printf.sprintf "_ppx_compose_%d" e.pexp_loc.Location.loc_start.Lexing.pos_cnum

let apply ~loc f xs =
  (match f.pexp_desc with
   | Pexp_apply (f', xs') ->
      Exp.apply ~loc f' (List.append xs' xs)
   | _ ->
      Exp.apply ~loc f xs)

let rec reduce_compose h x =
  (match h.pexp_desc with
   | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "%"; _}; _},
                 [(Nolabel, g); (Nolabel, f)])
   | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "%>"; _}; _},
                 [(Nolabel, f); (Nolabel, g)]) ->
      let fx = reduce_compose f x in
      reduce_compose g fx
   | _ ->
      (apply ~loc:h.pexp_loc h [Nolabel, x]))

let classify e =
  (match e.pexp_desc with
   | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "%"; _}; _},
                 [(Nolabel, _); (Nolabel, _)]) -> `Compose
   | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "%>"; _}; _},
                 [(Nolabel, _); (Nolabel, _)]) -> `Compose_fw
   | _ -> `Other)

let eta_expand_composition ~is_fw mapper e =
  let name = fresh_var_for e in
  let var_loc =
    if is_fw then {e.pexp_loc with loc_end = e.pexp_loc.loc_start}
             else {e.pexp_loc with loc_start = e.pexp_loc.loc_end} in
  let pat = Pat.var ~loc:var_loc {txt = name; loc = var_loc} in
  let arg = Exp.ident ~loc:var_loc {txt = Lident name; loc = var_loc} in
  let body = mapper.expr mapper (reduce_compose e arg) in
  Exp.fun_ ~loc:e.pexp_loc Nolabel None pat body

let rewrite_expr mapper e =
  (match e.pexp_desc with
   | Pexp_apply (h, ((Nolabel, x) :: xs)) when classify h <> `Other ->
      mapper.expr mapper (apply ~loc:e.pexp_loc (reduce_compose h x) xs)
   | _ ->
      (match classify e with
       | `Compose -> eta_expand_composition ~is_fw:false mapper e
       | `Compose_fw -> eta_expand_composition ~is_fw:true mapper e
       | `Other -> default_mapper.expr mapper e))

let compose_mapper _config _cookies = {default_mapper with expr = rewrite_expr}

let () = Driver.register ~name:"ppx_compose" ocaml_version compose_mapper
