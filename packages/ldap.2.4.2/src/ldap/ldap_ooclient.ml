(* An object oriented interface to ldap

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


open Ldap_types
open Ldap_funclient
open Ldap_schemaparser

(* types used throughout the library *)
(* add types *)
type op = string * string list
type op_lst = op list
type referral_policy = [ `RETURN
                       | `FOLLOW ]

(* change type for ldap entry *)
type changetype = [ `ADD | `DELETE | `MODIFY | `MODDN | `MODRDN ]

class type ldapentry_t =
object
  method add : op_lst -> unit
  method delete : op_lst -> unit
  method replace : op_lst -> unit
  method modify : (modify_optype * string * string list) list -> unit
  method attributes : string list
  method exists : string -> bool
  method get_value : string -> string list
  method diff : ldapentry_t -> (modify_optype * string * string list) list
  method changes : (modify_optype * string * string list) list
  method changetype : changetype
  method set_changetype : changetype -> unit
  method flush_changes : unit
  method dn : string
  method set_dn : string -> unit
  method print : unit
end;;

let format_entry e =
  Format.open_box 0;
  Format.open_box 2;
  Format.print_string ("<ldapentry_t " ^ (String.escaped e#dn));
  Format.force_newline ();
  let length_attrs = List.length e#attributes in
  let j = ref 0 in
    List.iter
      (fun a ->
         let length = List.length (e#get_value a) in
         let i = ref 0 in
           Format.print_string (Printf.sprintf "(\"%s\", " (String.escaped a));
           Format.open_box 0;
           Format.print_string "[";
           List.iter
             (fun v ->
                if !i < length - 1 then
                  (Format.print_string (Printf.sprintf "\"%s\";" (String.escaped v));
                   Format.print_break 1 0)
                else
                  Format.print_string (Printf.sprintf "\"%s\"" (String.escaped v));
                i := !i + 1)
             (e#get_value a);
           Format.print_string "]";
           Format.close_box ();
           (if !j < length_attrs - 1 then
              (Format.print_string ");";
               Format.force_newline ())
            else
              Format.print_string ")");
           j := !j + 1)
      (e#attributes);
    Format.close_box ();
    Format.print_string ">";
    Format.close_box ()

exception Limit

let format_entries lst =
  let length = List.length lst in
  let i = ref 0 in
    Format.open_box 0;
    Format.print_string "[";
    if length > 3 then
      try
        List.iter
          (fun e ->
             if !i > 49 then raise Limit
             else if !i < length - 1 then begin
               Format.print_string ("<ldapentry_t " ^ (String.escaped e#dn) ^ ">; ");
               Format.print_cut ();
               i := !i + 1
             end else
               Format.print_string ("<ldapentry_t " ^ (String.escaped e#dn) ^ ">"))
          lst
      with Limit -> Format.print_string "..."
    else
      List.iter
        (fun e ->
           if !i < length - 1 then begin
             format_entry e;
             Format.print_break 1 0;
             i := !i + 1
           end else
             format_entry e)
        lst;
    Format.print_string "]";
    Format.close_box ()

module CaseInsensitiveString =
  (struct
     type t = string * string
     let of_string s = (s, String.lowercase_ascii s)
     let to_string x = fst x
     let compare x y = String.compare (snd x) (snd y)
   end
     :
   sig
     type t
     val of_string: string -> t
     val to_string: t -> string
     val compare: t -> t -> int
   end);;

module OrdOid =
struct
  type t = Oid.t
  let compare = Oid.compare
end;;

module OrdStr =
struct
  type t = CaseInsensitiveString.t
  let compare = CaseInsensitiveString.compare
end;;

(* types for a set of Oids, and a set of strings *)
module Strset = Set.Make (OrdStr)
module Setstr = Set.Make (OrdOid)

(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
(* ldap entry object *)
class ldapentry =
object (self)
  val mutable dn = ""
  val mutable data = Hashtbl.create 50
  val mutable changes = []
  val mutable changetype = `ADD

  method private push_change (t:modify_optype) lst =
    match changetype with
        `MODIFY -> (match lst with
                        [] -> ()
                      | (attr, values) :: tail ->
                          changes <- (t, attr, values) :: changes; self#push_change t tail)
      | _ -> ()

  method changetype = changetype;
  method set_changetype (typ:changetype) = changetype <- typ
  method flush_changes = changes <- []
  method changes = changes

  method exists x = Hashtbl.mem data (String.lowercase_ascii x)
  method add (x:op_lst) =
    let rec do_add (x:op_lst) =
      match x with
          [] -> ()
        | (name, value) :: lst ->
            let lcname = String.lowercase_ascii name in
              try
                Ulist.addlst (Hashtbl.find data lcname) value; do_add lst
              with Not_found ->
                let current = Ulist.create 5 in
                  Hashtbl.add data lcname current; Ulist.addlst current value; do_add lst
    in
      do_add x; self#push_change `ADD x

  method diff (entry: ldapentry_t) =
    let diff_entries e1 e2 : (modify_optype * string * string list) list =
      let rec setOfList ?(set=Strset.empty) list =
        match list with
            a :: tail -> setOfList ~set:(Strset.add a set) tail
          | []  -> set
      in
      let ciStringlst list =
        List.rev_map
          CaseInsensitiveString.of_string
          list
      in
      let e1attrs = setOfList (ciStringlst e1#attributes) in
      let e2attrs = setOfList (ciStringlst e2#attributes) in
      let add_attrs =
        Strset.fold
          (fun attr mods ->
             let attr = CaseInsensitiveString.to_string attr in
               (`REPLACE, attr, e1#get_value attr) :: mods)
          (Strset.diff e1attrs (Strset.inter e1attrs e2attrs))
          []
      in
      let remove_attrs =
        Strset.fold
          (fun attr mods ->
             let attr = CaseInsensitiveString.to_string attr in
               (`DELETE, attr, []) :: mods)
          (Strset.diff e2attrs (Strset.inter e2attrs e1attrs))
          []
      in
      let sync_attrs =
        Strset.fold
          (fun attr mods ->
             let attr = CaseInsensitiveString.to_string attr in
             let e1vals = setOfList (ciStringlst (e1#get_value attr)) in
             let e2vals = setOfList (ciStringlst (e2#get_value attr)) in
               if (not (Strset.is_empty (Strset.diff e1vals (Strset.inter e1vals e2vals)))) ||
                 (not (Strset.is_empty (Strset.diff e2vals (Strset.inter e1vals e2vals))))
               then
                 (`REPLACE, attr, e1#get_value attr) :: mods
               else
                 mods)
          (Strset.inter e1attrs (Strset.inter e1attrs e2attrs))
          []
      in
        List.rev_append remove_attrs (List.rev_append sync_attrs add_attrs)
    in
      (diff_entries self entry)

  method delete (x:op_lst) =
    let rec do_delete x =
      match x with
          [] -> ()
        | (attr, values) :: lst ->
            let lcname = String.lowercase_ascii attr in
              match values with
                  [] -> Hashtbl.remove data lcname;do_delete lst
                | _  ->
                    (try List.iter (Ulist.remove (Hashtbl.find data lcname)) values
                     with Not_found -> ());
                    (match Ulist.tolst (Hashtbl.find data lcname) with
                         [] -> Hashtbl.remove data lcname
                       | _  -> ());
                    do_delete lst
    in
      do_delete x; self#push_change `DELETE x

  method replace (x:op_lst) =
    let rec do_replace x =
      match x with
          [] -> ()
        | (attr, values) :: lst -> let n = Ulist.create 5 in
            Ulist.addlst n values; Hashtbl.replace data (String.lowercase_ascii attr) n;
            do_replace lst;
    in
      do_replace x; self#push_change `REPLACE x

  method modify (x: (modify_optype * string * string list) list) =
    let rec do_modify x =
      match x with
          [] -> ()
        | (`ADD, attr, values) :: t -> self#add [(attr, values)];do_modify t
        | (`DELETE, attr, values) :: t -> self#delete [(attr, values)];do_modify t
        | (`REPLACE, attr, values) :: t -> self#replace [(attr, values)];do_modify t
    in
      do_modify x

  method attributes =
    let keys hash =
      let cur = ref [] in
      let key k _ = cur := k :: !cur in
        Hashtbl.iter key hash; !cur
    in
      keys data

  method get_value attr = Ulist.tolst (Hashtbl.find data (String.lowercase_ascii attr))
  method set_dn x = dn <- x
  method dn = dn
  method print =
    print_endline "THIS METHOD IS DEPRECATED, use Ldif_oo, or rely on the toplevel printers";
    print_endline ("dn: " ^ self#dn);
    (List.iter
       (fun a ->
          (List.iter
             (fun b -> print_endline (a ^ ": " ^ b))
             (self#get_value a)))
       self#attributes)

end

type changerec =
    [`Modification of string * ((Ldap_types.modify_optype * string * string list) list)
    | `Addition of ldapentry
    | `Delete of string
    | `Modrdn of string * int * string]

(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
let to_entry ent =
  let rec add_attrs attrs entry =
    match attrs with
        {attr_type = name; attr_vals = values} :: tail ->
          entry#add [(name, values)]; add_attrs tail entry
      | [] -> entry#set_changetype `MODIFY; entry
  in
    match ent with
        `Entry {sr_dn = dn; sr_attributes = attrs} ->
          let entry = new ldapentry in
            entry#set_dn dn; add_attrs attrs entry
      | `Referral refs ->
          let entry = new ldapentry in
            entry#set_dn "referral";
            entry#add [("ref", refs)];
            entry#add [("objectclass", ["referral"])];
            entry

let of_entry ldapentry =
  let rec extract_attrs ?(converted=[]) entry attrs =
    match attrs with
        [] -> converted
      | attr :: tail ->
          extract_attrs
            ~converted:({attr_type=attr;
                         attr_vals=(entry#get_value attr)} :: converted)
            entry
            tail
  in
    {sr_dn=(ldapentry#dn);
     sr_attributes=(extract_attrs ldapentry ldapentry#attributes)}

let iter (f: ldapentry -> unit) (res: ?abandon:bool -> unit -> ldapentry) =
  try
    while true
    do
      f (res ());
    done
  with
      LDAP_Failure (`SUCCESS, _, _) -> ()
    | exn -> (try ignore (res ~abandon:true ()) with _ -> ());raise exn

let rev_map (f: ldapentry -> 'a) (res: ?abandon:bool -> unit -> ldapentry) =
  let lst = ref [] in
    (try while true
     do
       lst := (f (res ())) :: !lst
     done
     with
         LDAP_Failure (`SUCCESS, _, _) -> ()
       | exn -> (try ignore (res ~abandon:true ()) with _ -> ());raise exn);
    !lst

let map (f: ldapentry -> 'a) (res: ?abandon:bool -> unit -> ldapentry) =
  List.rev (rev_map f res)

let fold (f:ldapentry -> 'a -> 'a) (v:'a) (res: ?abandon:bool -> unit -> ldapentry) =
  let value = ref v in
    try
      while true
      do
        value := (f (res ()) !value)
      done;
      !value
    with
        LDAP_Failure (`SUCCESS, _, _) -> !value
      | exn -> (try ignore (res ~abandon:true ()) with _ -> ());raise exn

(* a connection to an ldap server *)
class ldapcon ?(connect_timeout=1) ?(referral_policy=`RETURN) ?(version = 3) hosts =
object (self)
  val _referral_policy = referral_policy (* TODO: not used?? *)

  val mutable bdn = ""
  val mutable pwd = ""
  val mutable mth = `SIMPLE
  val mutable bound = true
  val mutable reconnect_successful = true
  val mutable con = init ~connect_timeout:connect_timeout ~version:version hosts
  method private reconnect =
    if bound then unbind con;
    bound <- false;
    reconnect_successful <- false;
    con <- init ~connect_timeout:connect_timeout ~version:version hosts;
    bound <- true;
    bind_s ~who: bdn ~cred: pwd ~auth_method: mth con;
    reconnect_successful <- true;

  method unbind = if bound then (unbind con;bound <- false)

  method update_entry (e:ldapentry) =
    if not (reconnect_successful && bound) then self#reconnect;
    try self#modify e#dn (List.rev e#changes); e#flush_changes
    with LDAP_Failure(`SERVER_DOWN, _, _) -> self#reconnect;self#update_entry e

  method bind ?(cred = "") ?(meth:authmethod = `SIMPLE) dn =
    if not bound then begin
      con <- init ~connect_timeout:connect_timeout ~version: version hosts;
      bound <- true
    end;
    bind_s ~who: dn ~cred: cred ~auth_method: meth con;
    reconnect_successful <- true;
    bdn <- dn; pwd <- cred; mth <- meth

  method add (entry: ldapentry) =
      if not (reconnect_successful && bound) then self#reconnect;
      try add_s con (of_entry entry)
      with LDAP_Failure(`SERVER_DOWN, _, _) ->
        self#reconnect;self#add entry

  method delete dn =
    if not (reconnect_successful && bound) then self#reconnect;
    try delete_s con ~dn
    with LDAP_Failure(`SERVER_DOWN, _, _) ->
      self#reconnect;self#delete dn

  method modify dn mods =
    if not (reconnect_successful && bound) then self#reconnect;
    try modify_s con ~dn ~mods
    with LDAP_Failure(`SERVER_DOWN, _, _) ->
      self#reconnect;self#modify dn mods

  method modrdn dn ?(deleteoldrdn = true) ?(newsup: string option=None) newrdn =
    if not (reconnect_successful && bound) then self#reconnect;
    try modrdn_s con ~dn ~newdn:newrdn ~deleteoldrdn ~newsup
    with LDAP_Failure(`SERVER_DOWN, _, _) ->
      self#reconnect;self#modrdn dn ~deleteoldrdn:deleteoldrdn newrdn

  method search
    ?(scope = `SUBTREE)
    ?(attrs = [])
    ?(attrsonly = false)
    ?(base = "")
    ?(sizelimit = 0l)
    ?(timelimit = 0l)
    filter =
    if not (reconnect_successful && bound) then self#reconnect;
    try
      List.rev_map to_entry
        (search_s
           ~scope ~base ~attrs
           ~attrsonly ~sizelimit
           ~timelimit con filter)
    with LDAP_Failure(`SERVER_DOWN, _, _) ->
      self#reconnect;
      self#search
        ~scope ~attrs ~attrsonly
        ~base ~sizelimit ~timelimit filter

  method search_a
    ?(scope = `SUBTREE)
    ?(attrs = [])
    ?(attrsonly = false)
    ?(base = "")
    ?(sizelimit = 0l)
    ?(timelimit = 0l)
    filter =

    (* a function which is returned by search_a, calling it will give
       the next entry due from the async search. The first_entry
       argument is there to maintain the semantics of ldapcon's
       transparent reconnection system. When search_a is called, we
       fetch the first entry, and pass it in to this function. We do
       this because, we will not know if the server actually recieved
       our search until we read the first entry. *)
    let fetch_result con (msgid:msgid) first_entry ?(abandon=false) () =
      if abandon then
        (Ldap_funclient.abandon con msgid;
         self#reconnect;
         to_entry (`Entry {sr_dn="";sr_attributes=[]}))
      else
        match !first_entry with (* are we on the first entry of the search? *)
            `No -> to_entry (get_search_entry con msgid)
          | `Yes e ->
              first_entry := `No;
              to_entry e
          | `NoResults -> (* this search has no results *)
              raise
                (LDAP_Failure
                   (`SUCCESS, "success",
                    {ext_matched_dn = ""; ext_referral = None}))
    in
      if not (reconnect_successful && bound) then self#reconnect;
      try
        let first_entry = ref `No in
        let msgid =
          search
            ~scope ~base ~attrs ~attrsonly
            ~sizelimit ~timelimit
            con filter
        in
          (* make sure the server is really still there *)
          (try first_entry := `Yes (get_search_entry con msgid)
           with LDAP_Failure (`SUCCESS, _, _) ->
             (* the search is already complete and has no results *)
             first_entry := `NoResults);
          fetch_result con msgid first_entry
      with LDAP_Failure(`SERVER_DOWN, _, _) ->
        self#reconnect;
        self#search_a
          ~scope ~attrs ~attrsonly ~base
          ~sizelimit ~timelimit filter

  method schema =
    if not (reconnect_successful && bound) then self#reconnect;
    try
      if version = 3 then
        let schema_base = (match (self#search
                                    ~base: ""
                                    ~scope: `BASE
                                    ~attrs: ["subschemasubentry"]
                                    "(objectclass=*)")
                           with
                               [e] -> List.hd (e#get_value "subschemasubentry")
                             |  _  -> raise Not_found) in
          (match (self#search
                    ~base: schema_base
                    ~scope: `BASE
                    ~attrs: ["objectClasses";"attributeTypes";
                             "matchingRules";"ldapSyntaxes"]
                    "(objectclass=subschema)") with
               [e] ->
                 readSchema
                   (e#get_value "objectclasses")
                   (e#get_value "attributetypes")
             |  _  -> raise Not_found)
      else
        raise Not_found
    with LDAP_Failure(`SERVER_DOWN, _, _) -> self#reconnect;self#schema

  method rawschema =
    if not (reconnect_successful && bound) then self#reconnect;
    try
      if version = 3 then
        let schema_base = (match (self#search
                                    ~base: ""
                                    ~scope: `BASE
                                    ~attrs: ["subschemasubentry"]
                                    "(objectclass=*)") with
                               [e] -> List.hd (e#get_value "subschemasubentry")
                             |  _  -> raise Not_found) in
          (match (self#search
                    ~base: schema_base
                    ~scope: `BASE
                    ~attrs: ["objectClasses";"attributeTypes";
                             "matchingRules";"ldapSyntaxes"]
                    "(objectclass=*)") with
               [e] -> e
             |  _  -> raise Not_found)
      else
        raise Not_found
    with LDAP_Failure(`SERVER_DOWN, _, _) -> self#reconnect;self#rawschema
end;;

(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
(* A schema checking entry:
   An entry which validates its validity against the server's
   schema *)
(* schema checking flavor *)
type scflavor = Optimistic (* attempt to find objectclasses which make illegal
                              attributes legal, delete them if no objectclass can
                              be found *)
                | Pessimistic (* delete any illegal attributes, do not add
                                 objectclasses to make them legal*)

(* for the schema checker, should never be seen by
   the user *)
exception Invalid_objectclass of string
exception Invalid_attribute of string
exception Single_value of string
exception Objectclass_is_required

let attrToOid schema (attr:Lcstring.t) =
  try (Hashtbl.find schema.attributes attr).at_oid (* try canonical name first *)
  with Not_found ->
    (match (Hashtbl.fold
              (fun _k v matches ->
                 if (List.exists
                       (fun n -> attr = (Lcstring.of_string n))
                       v.at_name)
                 then
                   v.at_oid :: matches
                 else matches)
              schema.attributes [])
     with
         [] -> raise (Invalid_attribute (Lcstring.to_string attr))
       | [oid] -> oid
       | _ -> raise (Invalid_attribute
                       ("this attribute mapps to multiple oids: " ^
                        (Lcstring.to_string attr))));;

let oidToAttr schema (attr:Oid.t) =
  List.hd (Hashtbl.find schema.attributes_byoid attr).at_name;;

let ocToOid schema (oc:Lcstring.t) =
  try (Hashtbl.find schema.objectclasses oc).oc_oid
  with Not_found -> raise (Invalid_objectclass (Lcstring.to_string oc));;

let oidToOc schema (oc:Oid.t) =
  List.hd (Hashtbl.find schema.objectclasses_byoid oc).oc_name

let getOc schema (oc:Lcstring.t) =
  try Hashtbl.find schema.objectclasses oc
  with Not_found -> raise (Invalid_objectclass (Lcstring.to_string oc));;

let getAttr schema (attr:Lcstring.t) =
  try Hashtbl.find schema.attributes attr
  with Not_found -> raise (Invalid_attribute (Lcstring.to_string attr));;

let equateAttrs schema a1 a2 = (attrToOid schema a1) = (attrToOid schema a2)

let rec setOfList ?(set=Setstr.empty) list =
  match list with
      a :: tail -> setOfList ~set:(Setstr.add a set) tail
    | []  -> set

class scldapentry schema =
object (self)
  inherit ldapentry as super
  val schemaAttrs = Hashtbl.create 50
  val schema = schema
  val mutable consistent = false
    (* the set of all attibutes actually present *)
  val mutable present       = Setstr.empty
    (* the set of all musts from all objectclasses on the entry *)
  val mutable must          = Setstr.empty
    (* the set of all mays from all objectclasses on the entry *)
  val mutable may           = Setstr.empty
    (* the set of required objectclasses *)
  val mutable requiredOcs   = Setstr.empty
    (* present objectclasses *)
  val mutable presentOcs    = Setstr.empty

  (* must + may *)
  val mutable all_allowed   = Setstr.empty
    (* must - (present * must) *)
  val mutable missingAttrs  = Setstr.empty
    (* requiredOcs - (presentOcs * requiredOcs) *)
  val mutable missingOcs    = Setstr.empty
    (* any objectclass which depends on a missing objectclass *)
  val mutable illegalOcs    = Setstr.empty
    (* present - (present * all_allowed) *)
  val mutable illegalAttrs  = Setstr.empty

  (* schema checking is best expressed as set manipulations.
     I can ascert this having implimented it in other ways *)
  method private update_condition =
    let generate_present attrs schema =
      setOfList (List.rev_map (attrToOid schema) attrs) in
    let rec generate_mustmay ocs schema set must =
      match ocs with
          oc :: tail ->
            let musts = setOfList
              (List.rev_map
                 (fun attr -> attrToOid schema attr)
                 (if must then (getOc schema oc).oc_must
                  else (getOc schema oc).oc_may))
            in
              generate_mustmay tail schema (Setstr.union musts set) must
        | [] -> set
    in
    let rec lstRequired schema (oc: Lcstring.t) =
      oc :: (List.flatten (List.rev_map
                             (fun sup -> lstRequired schema sup)
                             (getOc schema oc).oc_sup))
    in
    let generate_requiredocs schema ocs =
      setOfList
        (List.rev_map
           (ocToOid schema)
           (List.flatten (List.rev_map (lstRequired schema) ocs)))
    in
    let generate_illegal_oc missing schema ocs =
      let is_illegal_oc missing schema oc =
        let supchain = lstRequired schema oc in
          List.exists
            (fun mis ->
               List.exists ((=) mis)
                 supchain)
            missing
      in
        List.filter (is_illegal_oc missing schema) ocs
    in

      present      <- (generate_present
                         (List.rev_map (Lcstring.of_string) super#attributes)
                         schema);
      must         <- (generate_mustmay
                         (List.rev_map
                            (Lcstring.of_string)
                            (try super#get_value "objectclass"
                             with Not_found -> raise Objectclass_is_required))
                         schema
                         Setstr.empty
                         true);
      may          <- (generate_mustmay
                         (List.rev_map
                            (Lcstring.of_string)
                            (try super#get_value "objectclass"
                             with Not_found -> raise Objectclass_is_required))
                         schema
                         Setstr.empty
                         false);
      all_allowed  <- Setstr.union must may;
      missingAttrs <- Setstr.diff must (Setstr.inter must present);
      illegalAttrs <- Setstr.diff present (Setstr.inter all_allowed present);
      requiredOcs  <- (generate_requiredocs
                         schema
                         (List.rev_map
                            (Lcstring.of_string)
                            (try super#get_value "objectclass"
                             with Not_found -> raise Objectclass_is_required)));
      presentOcs   <- (setOfList
                         (List.rev_map
                            (fun attr -> ocToOid schema (Lcstring.of_string attr))
                            (try super#get_value "objectclass"
                             with Not_found -> raise Objectclass_is_required)));
      missingOcs   <- Setstr.diff requiredOcs (Setstr.inter requiredOcs presentOcs);
      illegalOcs   <- (setOfList
                         (List.rev_map
                            (ocToOid schema)
                            (generate_illegal_oc
                               (List.rev_map
                                  (fun x -> Lcstring.of_string (oidToOc schema x))
                                  (Setstr.elements missingOcs))
                               schema
                               (List.rev_map
                                  (Lcstring.of_string)
                                  (try super#get_value "objectclass"
                                   with Not_found -> raise Objectclass_is_required)))));
      if Setstr.is_empty (Setstr.union missingAttrs illegalAttrs) then
        consistent <- true
      else
        consistent <- false

  method private drive_updatecon =
    try self#update_condition
    with
        Invalid_objectclass(s) -> super#delete [("objectclass",[s])];self#drive_updatecon
      | Invalid_attribute(s) -> super#delete [(s,[])];self#drive_updatecon
      | Objectclass_is_required -> super#add [("objectclass", ["top"])]

  method private reconsile_illegal flavor =
    let find_in_oc oc attr = (List.exists
                                ((=) (Lcstring.of_string attr))
                                oc.oc_must) ||
      (List.exists
         ((=) (Lcstring.of_string attr))
         oc.oc_may) in
    let find_oc schema attr =
      let oc = ref (Lcstring.of_string "") in
        Hashtbl.iter
          (fun key valu ->
             if (find_in_oc valu attr) then oc := key)
          schema.objectclasses;
        if !oc = (Lcstring.of_string "") then raise Not_found;
        !oc
    in
      match flavor with
          Optimistic ->
            if not (Setstr.is_empty illegalAttrs) then
              ((List.iter (* add necessary objectclasses *)
                  (fun oc -> super#add [("objectclass",[(Lcstring.to_string oc)])])
                  (List.rev_map
                     (fun attr ->
                        try find_oc schema attr
                        with Not_found -> raise (Invalid_attribute attr))
                     (List.rev_map (oidToAttr schema) (Setstr.elements illegalAttrs))));
               self#drive_updatecon);
            (* add any objectclasses the ones we just added are dependant on *)
            if not (Setstr.is_empty missingOcs) then
              ((List.iter
                  (fun oc -> super#add [("objectclass", [oc])])
                  (List.rev_map (oidToOc schema) (Setstr.elements missingOcs)));
               self#drive_updatecon);
        | Pessimistic ->
            (List.iter
               (fun oc -> super#delete [("objectclass",[oc])])
               (List.rev_map (oidToOc schema) (Setstr.elements illegalOcs)));
            self#drive_updatecon;
            (List.iter (* remove disallowed attributes *)
               (fun attr -> super#delete [(attr, [])])
               (List.rev_map (oidToAttr schema) (Setstr.elements illegalAttrs)));
            self#drive_updatecon

  method private drive_reconsile flavor =
    try self#reconsile_illegal flavor
    with Invalid_attribute(a) -> (* remove attributes for which there is no objectclass *)
      (super#delete [(a, [])];
       self#drive_updatecon;
       self#drive_reconsile flavor)

  (* for debugging *)
  method private getCondition =
    let printLst lst = List.iter print_endline lst in
      print_endline "MAY";
      printLst (List.rev_map (oidToAttr schema) (Setstr.elements may));
      print_endline "PRESENT";
      printLst (List.rev_map (oidToAttr schema) (Setstr.elements present));
      (*      printLst (Setstr.elements present);*)
      print_endline "MUST";
      printLst (List.rev_map (oidToAttr schema) (Setstr.elements must));
      (*      printLst (Setstr.elements must);*)
      print_endline "MISSING";
      printLst (List.rev_map (oidToAttr schema) (Setstr.elements missingAttrs));
      (*      printLst (Setstr.elements missingAttrs);*)
      print_endline "ILLEGAL";
      printLst (List.rev_map (oidToAttr schema) (Setstr.elements illegalAttrs));
      print_endline "REQUIREDOCS";
      (*      printLst (List.rev_map (oidToOc schema) (Setstr.elements requiredOcs));*)
      printLst (List.rev_map Oid.to_string (Setstr.elements requiredOcs));
      print_endline "PRESENTOCS";
      (*      printLst (List.rev_map (oidToOc schema) (Setstr.elements presentOcs));*)
      printLst (List.rev_map Oid.to_string (Setstr.elements presentOcs));
      print_endline "MISSINGOCS";
      (*      printLst (List.rev_map (oidToOc schema) (Setstr.elements missingOcs));*)
      printLst (List.rev_map Oid.to_string (Setstr.elements missingOcs));
      print_endline "ILLEGALOCS";
      (*      printLst (List.rev_map (oidToOc schema) (Setstr.elements illegalOcs))*)
      printLst (List.rev_map Oid.to_string (Setstr.elements illegalOcs));

  (* for debugging *)
  method private getData = (must, may, present, missingOcs)

  method of_entry ?(scflavor=Pessimistic) (e:ldapentry) =
    super#set_dn (e#dn);
    super#set_changetype `ADD;
    (List.iter
       (fun attr ->
          try
            (super#add
               (try
                  self#single_val_check [(attr, (e#get_value attr))] true;
                  [(attr, (e#get_value attr))]
                with (* remove single valued attributes *)
                    Single_value _ -> [(attr, [List.hd (e#get_value attr)])]))
          with (* single_val_check may encounter unknown attributes *)
              Invalid_attribute _ | Invalid_objectclass _ -> ())
       e#attributes);
    self#drive_updatecon;
    self#drive_reconsile scflavor

  (* raise an exception if the user attempts to have more than
     one value in a single valued attribute. *)
  method private single_val_check (x:op_lst) consider_present =
    let check op =
      let attr = getAttr schema (Lcstring.of_string (fst op)) in
        (if attr.at_single_value then
           (match op with
                (_attr, _v1 :: _v2 :: _tail) -> false
              | (attr, _v1 :: _tail) ->
                  (if consider_present && (super#exists attr) then
                     false
                   else true)
              | _ -> true)
         else true)
    in
      match x with
          op :: tail -> (if not (check op) then
                           raise (Single_value (fst op))
                         else self#single_val_check tail consider_present)
        |  [] -> ()

  method! add x =
    self#single_val_check x true;super#add x;
    self#drive_updatecon;self#drive_reconsile Optimistic

  method! delete x =
    super#delete x;self#drive_updatecon;self#drive_reconsile Pessimistic

  method! replace x =
    self#single_val_check x false;super#replace x;
    self#drive_updatecon;self#drive_reconsile Optimistic

  method! modify x =
    let filter_mod x op =
      List.rev_map
        (fun (_, a, v) -> (a, v))
        (List.filter
           (function (the_op, _, _) when the_op = op -> true | _ -> false) x)
    in
      self#single_val_check (filter_mod x `ADD) true;
      self#single_val_check (filter_mod x `REPLACE) false;
      super#modify x;
      self#drive_updatecon;
      self#drive_reconsile Pessimistic

  method! get_value x =
    try super#get_value x with Not_found ->
      if (Setstr.mem (attrToOid schema (Lcstring.of_string x)) missingAttrs) then
        ["required"]
      else
        raise Not_found

  method! attributes =
    List.rev_append
      super#attributes
      (List.rev_map
         (fun a -> oidToAttr schema a)
         (Setstr.elements missingAttrs))

  method list_missing = Setstr.elements missingAttrs
  method list_allowed = Setstr.elements all_allowed
  method list_present = Setstr.elements present
  method is_missing x =
    Setstr.mem (attrToOid schema (Lcstring.of_string x)) missingAttrs
  method is_allowed x =
    Setstr.mem (attrToOid schema (Lcstring.of_string x)) all_allowed
end;;

(********************************************************************************)
(********************************************************************************)
(********************************************************************************)
(* a high level interface for accounts, and services in the directory *)

type generator = {gen_name:string;
                  required:string list;
                  genfun:(ldapentry_t -> string list)};;

type service = {svc_name: string;
                static_attrs: (string * (string list)) list;
                generate_attrs: string list;
                depends: string list};;

type generation_error = Missing_required of string list
                        | Generator_error of string

exception No_generator of string;;
exception Generation_failed of generation_error;;
exception No_service of string;;
exception Service_dep_unsatisfiable of string;;
exception Generator_dep_unsatisfiable of string * string;;
exception Cannot_sort_dependancies of (string list);;

let diff_values _convert_to_oid convert_from_oid attr attrvals svcvals =
    (attr, (List.rev_map
              convert_from_oid
              (Setstr.elements
                 (Setstr.diff
                    svcvals
                    (Setstr.inter svcvals attrvals)))))

(* compute the intersection of values between an attribute and a service,
   you need to pass this function as an argument to apply_set_op_to_values *)
let intersect_values _convert_to_oid convert_from_oid attr attrvals svcvals =
  (attr, (List.rev_map
            convert_from_oid
            (Setstr.elements
               (Setstr.inter svcvals attrvals))))

(* this function allows you to apply a set operation to the values of an attribute, and
   the static values on a service *)
let apply_set_op_to_values schema (attr:string) e svcval opfun =
  let lc = String.lowercase_ascii in
  let convert_to_oid = (match lc ((getAttr schema (Lcstring.of_string attr)).at_equality) with
                            "objectidentifiermatch" ->
                              (fun oc -> ocToOid schema (Lcstring.of_string oc))
                          | "caseexactia5match" -> Oid.of_string
                          | _ -> (fun av -> Oid.of_string (lc av)))
  in
  let convert_from_oid = (match lc ((getAttr schema (Lcstring.of_string attr)).at_equality) with
                              "objectidentifiermatch" -> (fun av -> oidToOc schema av)
                            | "caseexactia5match" -> Oid.to_string
                            | _ -> Oid.to_string)
  in
  let attrvals = setOfList
                   (List.rev_map
                      convert_to_oid
                      (try e#get_value attr with Not_found -> []))
  in
  let svcvals = setOfList (List.rev_map convert_to_oid (snd svcval))
  in
    opfun convert_to_oid convert_from_oid attr attrvals svcvals

class ldapaccount
  schema
  (generators:(string, generator) Hashtbl.t)
  (services:(string, service) Hashtbl.t) =
object (self)
  inherit scldapentry schema as super
  val mutable toGenerate = Setstr.empty
  val mutable neededByGenerators = Setstr.empty
  val services = services
  val generators = generators

(* evaluates the set of missing attributes to see if any of
   them can be generated, if so, it adds them to be generated *)
  method private resolve_missing =
    (* computes the set of generateable attributes *)
    let generate_togenerate generators missing togenerate =
      (* generators have dependancies. Some of the dependancies can
         also be generated. We can generate a dependancy if the following
         conditions are met.
         1. The dependancy is in the generators hash (it has a generation function)
         2. The dependancy is allowed by the schema (it is either a must or may of
         an objectclass currently on the object)
         3. The dependancy is not already present (if it is present already then it
         has already been satisfied, and there is no need to generate it) *)
      let find_generatable_dep generators generator =
        (List.rev_map
           (fun e -> attrToOid schema (Lcstring.of_string e))
           (List.filter
              (fun g ->
                 if ((Hashtbl.mem generators g) &&
                     (not (Setstr.mem
                             (attrToOid schema (Lcstring.of_string g))
                             (setOfList self#list_present)))) then
                   true
                 else false)
              (List.filter (* we can only add it if it is allowed by the schema *)
                 (fun attr -> super#is_allowed attr)
                 (Hashtbl.find generators generator).required)))
      in
        (* collect a flat list of all generatable dependancies *)
      let find_generatable_deps generators genlst =
        (List.flatten
           (List.rev_map
              (find_generatable_dep generators)
              genlst))
      in
        (* the set we are currently generating, union the set of missing attributes which we
           can generate. *)
      let generateing = (List.filter
                           (fun gen ->
                              if (Hashtbl.mem generators (String.lowercase_ascii (oidToAttr schema gen))) then
                                true
                              else false)
                           (List.rev_append
                              missing
                              (Setstr.elements togenerate)))
      in
        (* the total set of generatable at any point in time is. The set
           we are already generating, unioned with any generatable dependancies, unioned
           with the set of missing attributes (required by the schema) which can be generated.
           Note, the last union is done in the generateing expression above. *)
        setOfList
          (List.rev_append generateing (find_generatable_deps
                                          generators
                                          (List.rev_map
                                             (fun e -> String.lowercase_ascii (oidToAttr schema e))
                                             generateing)))
    in
    let generate_missing togen generators =
      setOfList
        (Hashtbl.fold
           (fun key valu requiredlst ->
              if Setstr.mem (attrToOid schema (Lcstring.of_string valu.gen_name)) togen then
                List.rev_append
                  requiredlst
                  (List.rev_map
                     (fun x -> try
                        attrToOid schema (Lcstring.of_string x)
                      with Invalid_attribute a ->
                        raise (Generator_dep_unsatisfiable (key, a)))
                     valu.required)
              else
                requiredlst)
           generators [])
    in
      toGenerate <- generate_togenerate generators super#list_missing toGenerate;
      neededByGenerators <- generate_missing toGenerate generators;

  method! list_missing =
    let allmissing =
      Setstr.union neededByGenerators (setOfList super#list_missing)
    in
      Setstr.elements
        (Setstr.diff
           allmissing
           (Setstr.inter
              allmissing
              (Setstr.union
                 toGenerate
                 (setOfList super#list_present))))

  method! attributes =
    (List.rev_map (oidToAttr schema)
       (Setstr.elements
          (Setstr.union toGenerate
             (setOfList
                (List.rev_map
                   (fun a -> attrToOid schema (Lcstring.of_string a))
                   super#attributes)))))

  method! is_missing x = (not (Setstr.mem
                                (attrToOid schema (Lcstring.of_string x))
                                toGenerate))
                        || (super#is_missing x)

  method generate =
    let sort_genlst generators unsatisfied =
      let satisfied alreadysatisfied present deps =
        List.for_all
          (fun dep ->
             (List.mem dep alreadysatisfied) ||
             (List.mem (attrToOid schema (Lcstring.of_string dep)) (present)))
          deps
      in
      let rec sort present ordtogen unsatisfied =
        match unsatisfied with
            [] -> ordtogen
          | todo ->
              let (aresat, notyet) =
                (List.partition
                   (fun attr ->
                      (satisfied ordtogen present
                         (Hashtbl.find generators attr).required))
                   todo)
              in
                match aresat with
                    [] -> raise (Cannot_sort_dependancies notyet)
                  | _ -> sort present (ordtogen @ aresat) notyet
      in
        sort (self#list_present) [] unsatisfied
    in
      match self#list_missing with
          [] ->
            (List.iter
               (fun attr ->
                  self#add [(attr, (Hashtbl.find generators attr).genfun (self:>ldapentry_t))])
               (sort_genlst generators
                  (List.rev_map
                     (fun elt -> String.lowercase_ascii (oidToAttr schema elt))
                     (Setstr.elements toGenerate))));
            toGenerate <- Setstr.empty
        | a  -> raise (Generation_failed
                         (Missing_required (List.rev_map (oidToAttr schema) a)))

  method! get_value x =
    if (Setstr.mem (attrToOid schema (Lcstring.of_string x)) toGenerate) then
      ["generate"]
    else
      super#get_value x

(* adapt the passed in service to the current state of the entry
   this may result in a service with applies no changes. The entry
   may already have the service. *)
  method adapt_service svc =
      {svc_name=svc.svc_name;
       static_attrs=(List.filter
                          (fun cons ->
                             match cons with
                                 (_attr, []) -> false
                               | _          -> true)
                          (List.rev_map
                             (fun cons -> apply_set_op_to_values schema (fst cons) self cons diff_values)
                             svc.static_attrs));
       generate_attrs=(List.filter
                         (fun attr ->
                            (try (ignore (super#get_value attr));false
                             with Not_found -> true))
                         svc.generate_attrs);
       depends=svc.depends}

(* add a service to the account, if they already satisfy the service
   then do nothing *)
  method add_service svc =
    let service = try Hashtbl.find services (String.lowercase_ascii svc)
    with Not_found -> raise (No_service svc) in
      (try List.iter (self#add_service) service.depends
       with (No_service x) -> raise (Service_dep_unsatisfiable x));
      let adaptedsvc = self#adapt_service service in
        (let do_adds a =
           let singlevalu =
             (List.filter
                (fun attr -> (getAttr schema
                             (Lcstring.of_string (fst attr))).at_single_value) a)
           in
           let multivalued =
             (List.filter
                (fun attr -> not (getAttr schema
                                 (Lcstring.of_string (fst attr))).at_single_value) a)
           in
             self#add multivalued;
             self#replace singlevalu
         in
           do_adds adaptedsvc.static_attrs);
        (match adaptedsvc.generate_attrs with
             [] -> ()
           | a  -> List.iter (self#add_generate) a)

  method delete_service svc =
    let find_deps services service =
      (Hashtbl.fold
         (fun serv svcstruct deplst ->
            if (List.exists ((=) service) svcstruct.depends) then
              serv :: deplst
            else
              deplst)
         services [])
    in
    let service = try Hashtbl.find services (String.lowercase_ascii svc)
    with Not_found -> raise (No_service svc) in
      (List.iter (self#delete_service) (find_deps services svc));
      (List.iter
         (fun e -> match e with
              (_attr, []) -> ()
            | a -> (try (ignore (super#get_value (fst a)));super#delete [a]
                    with Not_found -> ()))
         (List.rev_map
            (fun cons ->
               apply_set_op_to_values schema (fst cons) self cons intersect_values)
            service.static_attrs));
      (List.iter
         (fun attr ->
            (try (match self#get_value attr with
                      ["generate"] -> self#delete_generate attr
                    | _ -> super#delete [(attr, [])])
             with Not_found -> ()))
         service.generate_attrs)

  method service_exists service =
    let service = (try (Hashtbl.find services service)
                   with Not_found -> raise (No_service service))
    in
      match self#adapt_service service with
          {svc_name=_s;
           static_attrs=[];
           generate_attrs=[];
           depends=d} -> (match d with
                              [] -> true
                            | d  -> List.for_all self#service_exists d)
        | _ -> false

  method services_present =
    Hashtbl.fold
      (fun _k v l ->
         if self#service_exists v.svc_name then
           v.svc_name :: l
         else l)
      services []

  method! of_entry ?(scflavor=Pessimistic) e = super#of_entry ~scflavor e;self#resolve_missing

  method add_generate x =
    (if (Hashtbl.mem generators (String.lowercase_ascii x)) then
       toGenerate <- Setstr.add (attrToOid schema (Lcstring.of_string x)) toGenerate
     else raise (No_generator x));
    self#resolve_missing
  method delete_generate x =
    let find_dep attr generators =
      (Hashtbl.fold
         (fun key valu deplst ->
            if (List.exists ((=) attr) valu.required) then
              key :: deplst
            else
              deplst)
         generators [])
    in
      (List.iter (self#delete_generate) (find_dep x generators));
      toGenerate <-
      Setstr.remove
        (attrToOid schema (Lcstring.of_string x)) toGenerate

  method! add x = (* add x, remove all attributes in x from the list of generated attributes *)
    super#add x;
    (List.iter
      (fun a ->
         toGenerate <- (Setstr.remove
                          (attrToOid schema (Lcstring.of_string (fst a)))
                          toGenerate))
       x);
    self#resolve_missing
  method! delete x = super#delete x;self#resolve_missing
  method! replace x = (* replace x, removeing it from the list of generated attrs *)
    super#replace x;
    (List.iter
       (fun a ->
          toGenerate <- (Setstr.remove
                           (attrToOid schema (Lcstring.of_string (fst a)))
                           toGenerate))
       x);
    self#resolve_missing
end;;
