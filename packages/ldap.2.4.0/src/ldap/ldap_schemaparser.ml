(* A parser for rfc2252 format schema definitionsa

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)


open Ldap_schemalexer;;

module Oid =
  (struct
     type t = string
     let of_string s = s
     let to_string oid = oid
     let compare x y = String.compare (to_string x) (to_string y)
   end
     :
   sig
     type t
     val of_string: string -> t
     val to_string: t -> string
     val compare: t -> t -> int
   end);;

let format_oid id =
  Format.open_box 0;
  Format.print_string ("<oid " ^ Oid.to_string id ^ ">");
  Format.close_box ()

module Lcstring =
  (struct
     type t = string
     let of_string s = String.lowercase s
     let to_string x = x
     let compare x y = String.compare x y
   end
     :
   sig
     type t
     val of_string: string -> t
     val to_string: t -> string
     val compare: t -> t -> int
   end);;

let format_lcstring id =
  Format.open_box 0;
  Format.print_string ("<lcstring " ^ Lcstring.to_string id ^ ">");
  Format.close_box ()

type octype = Abstract | Structural | Auxiliary;;
type objectclass = {oc_name: string list;
                    oc_oid:Oid.t;
                    oc_desc:string;
                    oc_obsolete:bool;
                    oc_sup:Lcstring.t list;
                    oc_must:Lcstring.t list;
                    oc_may:Lcstring.t list;
                    oc_type:octype;
                    oc_xattr:string list}

type attribute = {at_name:string list;
                  at_desc:string;
                  at_oid:Oid.t;
                  at_equality:string;
                  at_ordering:string;
                  at_substr:Oid.t;
                  at_syntax:Oid.t;
                  at_length: Int64.t;
                  at_obsolete:bool;
                  at_single_value:bool;
                  at_collective:bool;
                  at_no_user_modification:bool;
                  at_usage:string;
                  at_sup:Lcstring.t list;
                  at_xattr:string list};;

type schema = {objectclasses: (Lcstring.t, objectclass) Hashtbl.t;
               objectclasses_byoid: (Oid.t, objectclass) Hashtbl.t;
               attributes: (Lcstring.t, attribute) Hashtbl.t;
               attributes_byoid: (Oid.t, attribute) Hashtbl.t};;

type schema_error = Undefined_attr_reference of string
                    | Undefined_oc_reference of string
                    | Cross_linked_oid of string list

let typecheck_schema schema =
  let attribute_exists_p schema attr =
    if Hashtbl.mem schema.attributes attr then true
    else
      Hashtbl.fold
        (fun _ {at_name=names} b ->
           if b then b
           else
             List.exists
               (fun name -> (Lcstring.of_string name) = attr)
               names)
        schema.attributes
        false
  in
    (* check that all musts, and all mays are attributes which
       exist. It would be an error to specify a must or a may which does
       not exist. *)
  let errors =
    Hashtbl.fold
      (fun oc {oc_must=musts;oc_may=mays} errors ->
         let check_error errors attr =
           if not (attribute_exists_p schema attr) then
             (Lcstring.to_string oc,
              Undefined_attr_reference (Lcstring.to_string attr)) :: errors
           else errors
         in
           (List.rev_append
              errors
              (List.rev_append
                 (List.fold_left check_error [] musts)
                 (List.fold_left check_error [] mays))))
      schema.objectclasses
      []
  in
    (* check for cross linked oids *)
  let errors =
    let oids = Hashtbl.create 100 in
    let seen = Hashtbl.create 100 in
      Hashtbl.iter
        (fun oid {at_name=n} -> Hashtbl.add oids oid (List.hd n))
        schema.attributes_byoid;
      Hashtbl.iter
        (fun oid {oc_name=n} -> Hashtbl.add oids oid (List.hd n))
        schema.objectclasses_byoid;
      Hashtbl.fold
        (fun oid name errors ->
           if List.length (Hashtbl.find_all oids oid) > 1 then
             if Hashtbl.mem seen oid then
               errors
             else (
               Hashtbl.add seen oid ();
               (name, Cross_linked_oid (Hashtbl.find_all oids oid)) :: errors
             )
           else
             errors
        )
        oids
        errors
  in
    (* make sure all superior ocs are defined *)
  let errors =
    Hashtbl.fold
      (fun oc {oc_sup=sups} errors ->
          List.rev_append
            errors
            (List.rev_map
               (fun missing -> (missing, Undefined_oc_reference missing))
               (List.filter
                  (fun oc ->
                     not
                       (Hashtbl.mem
                          schema.objectclasses
                          (Lcstring.of_string oc)))
                  (List.rev_map Lcstring.to_string sups))))
      schema.objectclasses
      errors
  in
    errors

let schema_print_depth = ref 10
let format_schema s =
  let indent = 3 in
  let printtbl tbl =
    let i = ref 0 in
      try
        Hashtbl.iter
          (fun aname aval ->
             if !i < !schema_print_depth then begin
               Format.print_string ("<KEY " ^ (Lcstring.to_string aname) ^ ">");
               Format.print_break 1 indent;
               i := !i + 1
             end
             else failwith "depth")
          tbl
      with Failure "depth" -> Format.print_string "..."
  in
    Format.open_box 0;
    Format.print_string "{objectclasses = <HASHTBL ";
    Format.print_break 0 indent;
    printtbl s.objectclasses;
    Format.print_string ">;";
    Format.print_break 0 1;
    Format.print_string "objectclasses_byoid = <HASHTBL ...>;";
    Format.print_break 0 1;
    Format.print_string "attributes = <HASHTBL ";
    Format.print_break 0 indent;
    printtbl s.attributes;
    Format.print_string ">;";
    Format.print_break 0 1;
    Format.print_string "attributes_byoid = <HASHTBL ...>}";
    Format.close_box ()

exception Parse_error_oc of Lexing.lexbuf * objectclass * string;;
exception Parse_error_at of Lexing.lexbuf * attribute * string;;
exception Syntax_error_oc of Lexing.lexbuf * objectclass * string;;
exception Syntax_error_at of Lexing.lexbuf * attribute * string;;

let rec readSchema oclst attrlst =
  let empty_oc = {oc_name=[];oc_oid=Oid.of_string "";oc_desc="";oc_obsolete=false;oc_sup=[];
                  oc_must=[];oc_may=[];oc_type=Abstract;oc_xattr=[]}
  in
  let empty_attr = {at_name=[];at_oid=Oid.of_string "";at_desc="";at_equality="";at_ordering="";
                    at_usage=""; at_substr=Oid.of_string "";at_syntax=Oid.of_string "";
                    at_length=0L;at_obsolete=false;at_single_value=false;
                    at_collective=false;at_no_user_modification=false;at_sup=[];at_xattr=[]}
  in
  let readOc lxbuf oc =
    let rec readOptionalFields lxbuf oc =
      try match (lexoc lxbuf) with
          Name s                -> readOptionalFields lxbuf {oc with oc_name=s}
        | Desc s                -> readOptionalFields lxbuf {oc with oc_desc=s}
        | Obsolete              -> readOptionalFields lxbuf {oc with oc_obsolete=true}
        | Sup s                 -> (readOptionalFields
                                       lxbuf
                                       {oc with oc_sup=(List.rev_map (Lcstring.of_string) s)})
        | Ldap_schemalexer.Abstract   -> readOptionalFields lxbuf {oc with oc_type=Abstract}
        | Ldap_schemalexer.Structural -> readOptionalFields lxbuf {oc with oc_type=Structural}
        | Ldap_schemalexer.Auxiliary  -> readOptionalFields lxbuf {oc with oc_type=Auxiliary}
        | Must s                -> (readOptionalFields
                                       lxbuf
                                       {oc with oc_must=(List.rev_map (Lcstring.of_string) s)})
        | May s                 -> (readOptionalFields
                                       lxbuf
                                       {oc with oc_may=(List.rev_map (Lcstring.of_string) s)})
        | Xstring t             -> (readOptionalFields
                                       lxbuf
                                       {oc with oc_xattr=(t :: oc.oc_xattr)})
        | Rparen                -> oc
        | _                     -> raise (Parse_error_oc (lxbuf, oc, "unexpected token"))
      with Failure(_) -> raise (Parse_error_oc (lxbuf, oc, "Expected right parenthesis"))
    in
    let readOid lxbuf oc =
      try match (lexoc lxbuf) with
          Numericoid(s) -> readOptionalFields lxbuf {oc with oc_oid=Oid.of_string s}
        | _ -> raise (Parse_error_oc (lxbuf, oc, "missing required field, numericoid"))
      with Failure(_) -> raise (Syntax_error_oc (lxbuf, oc, "Syntax error"))
    in
    let readLparen lxbuf oc =
      try match (lexoc lxbuf) with
          Lparen -> readOid lxbuf oc
        | _ -> raise (Parse_error_oc (lxbuf, oc, "Expected left paren"))
      with Failure(_) -> raise (Syntax_error_oc (lxbuf, oc, "Syntax error"))
    in
      readLparen lxbuf oc
  in
  let rec readOcs oclst schema =
    match oclst with
        a :: l -> let oc = readOc (Lexing.from_string a) empty_oc in
          List.iter (fun n -> Hashtbl.add schema.objectclasses (Lcstring.of_string n) oc) oc.oc_name;
          Hashtbl.add schema.objectclasses_byoid oc.oc_oid oc;readOcs l schema
      | [] -> ()
  in
  let rec readAttr lxbuf attr =
    let rec readOptionalFields lxbuf attr =
      try match (lexattr lxbuf) with
          Name s              -> readOptionalFields lxbuf {attr with at_name=s}
        | Desc s              -> readOptionalFields lxbuf {attr with at_desc=s}
        | Obsolete            -> readOptionalFields lxbuf {attr with at_obsolete=true}
        | Sup s               ->
            readOptionalFields lxbuf {attr with at_sup=(List.rev_map (Lcstring.of_string) s)}
        | Equality s          -> readOptionalFields lxbuf {attr with at_equality=s}
        | Substr s            -> readOptionalFields lxbuf {attr with at_substr=Oid.of_string s}
        | Ordering s          -> readOptionalFields lxbuf {attr with at_ordering=s}
        | Syntax (s, l)       ->
            readOptionalFields lxbuf {attr with at_syntax=Oid.of_string s;at_length=l}
        | Single_value         -> readOptionalFields lxbuf {attr with at_single_value=true}
        | Collective           -> readOptionalFields lxbuf {attr with at_collective=true}
        | No_user_modification -> readOptionalFields lxbuf {attr with at_no_user_modification=true}
        | Usage s              -> readOptionalFields lxbuf {attr with at_usage=s}
        | Rparen               -> attr
        | Xstring t            -> (readOptionalFields
                                     lxbuf
                                     {attr with at_xattr=(t :: attr.at_xattr)})
        | _                    -> raise (Parse_error_at (lxbuf, attr, "unexpected token"))
      with Failure(f) -> raise (Parse_error_at (lxbuf, attr, f))
    in
    let readOid lxbuf attr =
      try match (lexoc lxbuf) with
          Numericoid(s) -> readOptionalFields lxbuf {attr with at_oid=Oid.of_string s}
        | _ -> raise (Parse_error_at (lxbuf, attr, "missing required field, numericoid"))
      with Failure(_) -> raise (Syntax_error_at (lxbuf, attr, "Syntax error"))
    in
    let readLparen lxbuf attr =
      try match (lexoc lxbuf) with
          Lparen -> readOid lxbuf attr
        | _ -> raise (Parse_error_at (lxbuf, attr, "Expected left paren"))
      with Failure(_) -> raise (Syntax_error_at (lxbuf, attr, "Syntax error"))
    in
      readLparen lxbuf attr
  in
  let rec readAttrs attrlst schema =
    match attrlst with
        a :: l -> let attr = readAttr (Lexing.from_string a) empty_attr in
          List.iter (fun n -> Hashtbl.add schema.attributes (Lcstring.of_string n) attr) attr.at_name;
          Hashtbl.add schema.attributes_byoid attr.at_oid attr;readAttrs l schema
      | [] -> ()
  in
  let schema = {objectclasses=Hashtbl.create 500;
                objectclasses_byoid=Hashtbl.create 500;
                attributes=Hashtbl.create 5000;
                attributes_byoid=Hashtbl.create 5000} in
    readAttrs attrlst schema;
    readOcs oclst schema;
    schema;;
