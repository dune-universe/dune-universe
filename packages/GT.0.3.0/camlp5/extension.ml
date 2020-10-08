(**************************************************************************
 *  Copyright (C) 2012-2014
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

#load "pa_extend.cmo";;
#load "q_MLast.cmo";;

open List
open Pcaml
open Asttools
open GTCommon

let hdtl loc xs = (List.hd xs, List.tl xs)
let trait_proto_t typ trait = Printf.sprintf "%s_proto_%s" trait typ

(* let class_t name = name ^ "_t"
 * let trait_t typ trait = class_t (if trait <> "" then sprintf "%s_%s" trait typ else typ) *)

(* let tdecl_to_descr loc t =
 *   let name = get_val loc (snd (get_val loc t.tdNam)) in
 *   let args =
 *     map (fun (x, _) ->
 *      match get_val loc x with
 * 	   | Some y -> y
 * 	   | None   -> oops loc "wildcard type parameters not supported"
 *     )
 *     (get_val loc t.tdPrm)
 *   in
 *   let convert =
 *     let convert_concrete typ =
 *       let rec inner = function
 *       | <:ctyp< ( $list:typs$ ) >> as typ -> Tuple (typ, map inner typs)
 *       | <:ctyp< ' $a$ >> as typ -> Variable (typ, a)
 *       | <:ctyp< $t$ $a$ >> as typ ->
 *           (match inner t, inner a with
 *            | _, Arbitrary _ -> Arbitrary typ
 *            | Instance (_, targs, tname), a -> Instance (typ, targs@[a], tname)
 *            | _ -> Arbitrary typ
 *           )
 *       | <:ctyp< $q$ . $t$ >> as typ ->
 *           (match inner q, inner t with
 *           | Instance (_, [], q), Instance (_, [], t) -> Instance (typ, [], q@t)
 *           | _ -> Arbitrary typ
 *           )
 *       | (<:ctyp< $uid:n$ >> | <:ctyp< $lid:n$ >>) as typ -> Instance (typ, [], [n])
 *       | t -> Arbitrary t
 *       in
 *       let rec replace = function
 *       | Tuple (t, typs) -> Tuple (t, map replace typs)
 *       | Instance (t, args', qname) as orig when qname = [name] ->
 *          (try
 *            let args' =
 *              map (function
 * 	               | Variable (_, a) -> a
 *                  | _ -> invalid_arg "Not a variable"
 *                  )
 *              args'
 *            in
 *            if args' = args then Self (t, args, qname) else orig
 *          with Invalid_argument "Not a variable" -> orig
 *          )
 *       | x -> x
 *       in
 *       replace (inner typ)
 *     in
 *     function
 *     | <:ctyp< [ $list:const$ ] >> | <:ctyp< $_$ == $priv:_$ [ $list:const$ ] >> ->
 *        let const = map (fun (loc, name, args, d) ->
 * 	       match d with
 * 			   | None -> `Con (get_val loc name, map convert_concrete (get_val loc args))
 * 			   | _    -> oops loc "unsupported constructor declaration"
 *          )
 *          const
 *     	in
 *       `Vari const
 *
 *     | <:ctyp< { $list:fields$ } >> | <:ctyp< $_$ == $priv:_$ { $list:fields$ } >> ->
 *       let fields = map (fun (_, name, mut, typ) -> name, mut, convert_concrete typ) fields in
 *       `Struct fields
 *
 *     | <:ctyp< ( $list:typs$ ) >> -> `Tuple (map convert_concrete typs)
 *
 *     | <:ctyp< [ = $list:variants$ ] >> ->
 *       let wow () = oops loc "unsupported polymorphic variant type constructor declaration" in
 *       let variants =
 *         map (function
 * 	       | <:poly_variant< $typ$ >> ->
 *             (match convert_concrete typ with
 *             | Arbitrary _ -> wow ()
 *             | typ -> `Type typ
 *             )
 * 	       | <:poly_variant< ` $c$ >> -> `Con (c, [])
 * 	       | <:poly_variant< ` $c$ of $list:typs$ >> ->
 * 		   let typs =
 * 		     flatten (
 * 		       map (function
 * 			    | <:ctyp< ( $list:typs$ ) >> -> map convert_concrete typs
 * 			    | typ -> [convert_concrete typ]
 * 			   )
 * 		           typs
 * 		     )
 * 		   in
 * 		   `Con (c, typs)
 * 	       | _ -> wow ()
 * 	      )
 * 	    variants
 * 	in
 *         `Poly variants
 *
 *     | typ ->
 * 	(match convert_concrete typ with
 * 	 | Arbitrary _ -> oops loc "unsupported type"
 * 	 | typ         -> `Vari [match typ with Variable (t, _) -> `Tuple [Tuple (<:ctyp< ($list:[t]$) >>, [typ])] | _ -> `Type typ]
 * 	)
 *   in
 *   (args, name, convert t.tdDef) *)


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
      let n, q = hdtl loc (rev ci) in
      let classname =
        match t with
        | None   -> Naming.class_name_for_typ n
        | Some t -> Naming.trait_class_name_for_typ ~trait:t n
      in
      rev (classname::q)

  | "+"; ci=qname; t=trait ->
      let n, q = hdtl loc (rev ci) in
      rev ((trait_proto_t t n) :: q)
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
      let n, q = hdtl loc (rev ci) in
      let classname =
        match t with
        | None   -> Naming.class_name_for_typ n
        | Some t -> Naming.trait_class_name_for_typ ~trait:t n
      in
      longident_lident_of_string_list loc (rev (classname::q))

  | "+"; ci=qname; t=trait ->
      let n, q = hdtl loc (rev ci) in
      longident_lident_of_string_list loc (rev ((trait_proto_t n t) :: q))

  | ci=qname -> longident_lident_of_string_list loc ci
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
