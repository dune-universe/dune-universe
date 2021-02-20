(**************************************************************************
 *  Copyright (C) 2012-2020
 *  Dmitri Boulytchev (dboulytchev@math.spbu.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

open Pcaml
open GTCommon

let hdtl loc xs = (List.hd xs, List.tl xs)
let trait_proto_t typ trait = Printf.sprintf "%s_proto_%s" trait typ


EXTEND
  GLOBAL: sig_item str_item class_expr class_sig_item expr ctyp type_decl
  class_expr_simple extended_longident;

  class_sig_item: [[
     "inherit"; cs = class_signature -> <:class_sig_item< inherit $cs$ >>
  ]];

  class_signature:
    [ [ ci = class_type_longident ->
          let last, pfx = Camlp5Helpers.sep_last ci in
          assert (not (Camlp5Helpers.capitalized last)) ;
          match pfx with
            [] -> <:class_type< $lid:last$ >>
          | h::t ->
            let rec lirec li = function
                h::t -> lirec <:extended_longident< $longid:li$ . $uid:h$ >> t
              | [] -> li in
            let li = lirec <:extended_longident< $uid:h$ >> t in
            <:class_type< $longid:li$ . $lid:last$ >>
      ] ]
  ;

  class_type_longident: [[
    "@"; ci=qname; t=OPT trait ->
      let n, q = hdtl loc (List.rev ci) in
      let classname =
        match t with
        | None   -> Naming.class_name_for_typ n
        | Some t -> Naming.trait_class_name_for_typ ~trait:t n
      in
      List.rev (classname::q)

  | "+"; ci=qname; t=trait ->
      let n, q = hdtl loc (List.rev ci) in
      List.rev ((trait_proto_t t n) :: q)
  ]]
  ;

  class_expr_simple: BEFORE "simple" [[
    "["; ct = ctyp; ","; ctcl = LIST1 ctyp SEP ","; "]"; ci = class_longident ->
      <:class_expr< [ $list:(ct :: ctcl)$ ] $lilongid:ci$ >>
  | "["; ct = ctyp; "]"; ci = class_longident ->
      <:class_expr< [ $ct$ ] $lilongid:ci$ >>
  | ci = class_longident -> <:class_expr< $lilongid:ci$ >>
  ]];

  expr: BEFORE "simple" [
   LEFTA [ "new"; i = V class_longident "list" -> <:expr< new $_lilongid:i$ >> ]
  ];

  ctyp: BEFORE "simple" [[
    "#"; id = V class_longident "list" -> <:ctyp< # $_lilongid:id$ >>
  ]];

  class_longident: [[
    "@"; ci=qname; t=OPT trait ->
      let n, q = hdtl loc (List.rev ci) in
      let classname =
        match t with
        | None   -> Naming.class_name_for_typ n
        | Some t -> Naming.trait_class_name_for_typ ~trait:t n
      in
      Asttools.longident_lident_of_string_list loc (List.rev (classname::q))

  | "+"; ci=qname; t=trait ->
      let n, q = hdtl loc (List.rev ci) in
      Asttools.longident_lident_of_string_list loc (List.rev ((trait_proto_t n t) :: q))

  | ci=qname -> Asttools.longident_lident_of_string_list loc ci
  ]];

  qname: [[
    n=LIDENT              -> [n]
  | m=UIDENT; "."; q=SELF -> m :: q
  ]];

  trait: [[ "["; id=LIDENT; "]" -> id ]];

  str_item: LEVEL "top" [[
    "@"; "type"; t=LIST1 t_decl SEP "and" -> Core2.generate_str t loc
  ]];

  sig_item: LEVEL "top" [[
    "@"; "type"; t=LIST1 t_decl SEP "and" -> Core2.generate_sig t loc
  ]];

  t_decl: [[
    "["; t=type_decl; "]" -> t, []
  | t=type_decl; d=OPT deriving ->
    (* t, [tdecl_to_descr loc t, match d with None -> [] | Some d -> d] *)
    t, (match d with None -> [] | Some d -> d)
  ]];

  deriving: [["with"; s=LIST1 LIDENT SEP "," -> s]];

END;
