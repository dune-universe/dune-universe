(* PPX related tools *)

open Ast_405

open Utils

open Parsetree
open Asttypes
open Ast_helper
open Location

let ghost l = { l with loc_ghost = true }
    
let at ?loc txt = 
  let loc = match loc with 
    | None -> !Ast_helper.default_loc
    | Some loc -> loc
  in
  { txt; loc }

let (!@) x = at x
  
let lid ?loc s = at ?loc & Longident.parse s

let with_loc = Ast_helper.with_default_loc
let with_gloc l = Ast_helper.with_default_loc (ghost l)

module Name = struct
  let make_unique =
    let cntr = ref 0 in
    fun n ->
      let x = !cntr in
      incr cntr;
      n ^ "_" ^ string_of_int x
end

module Typ = struct
  include Typ
  let new_var =
    let cntr = ref 0 in
    fun prefixo -> 
      incr cntr;
      let prefix = match prefixo with None -> "tvar" | Some n -> n in
      var & prefix ^ "_" ^ string_of_int !cntr
  let ref_ ?loc ?attrs ty = 
    constr ?loc ?attrs (at ?loc & Longident.Lident "ref") [ty]
  let option ?loc ?attrs ty =
    constr ?loc ?attrs (at ?loc & Longident.(Ldot (Lident "*predef*", "option"))) [ty]
end

module Exp = struct
  include Exp
  let var ?loc ?attrs s = ident ?loc ?attrs & at ?loc & Longident.Lident s
  let lident ?loc ?attrs lid = ident ?loc ?attrs & at ?loc lid
  let unit () = construct (at & Longident.Lident "()") None 
  let string ?loc ?attrs s = constant ?loc ?attrs & Pconst_string (s, None)
  let int ?loc ?attrs i = constant ?loc ?attrs & Pconst_integer (string_of_int i, None)
  let bool ?loc ?attrs b = construct ?loc ?attrs (lid ?loc (if b then "true" else "false")) None
  let id ?loc ?attrs s = ident ?loc ?attrs & at ?loc & Longident.parse s
  let option ?loc ?attrs = function
    | None -> construct ?loc ?attrs (lid ?loc "None") None
    | Some e -> construct ?loc ?attrs (lid ?loc "Some") (Some e)
  let parse s =
    try
      Migrate.To405.copy_expression
      & Parser.parse_expression Lexer.token (Lexing.from_string s)
    with
    | _e -> failwith (Printf.sprintf "parse fail: %s" s)

  let object' ?loc ?attrs flds = object_ ?loc ?attrs (Cstr.mk (Pat.any ()) flds)
  let seqs = function
    | [] -> assert false
    | x::xs -> List.fold_right (fun x st -> sequence x st) xs x
  let ignore_ e = apply (id "Pervasives.ignore") [Nolabel, e]
  let assert_false () = assert_ & bool false
  let open_ ?loc ?attrs ?(override=false) =
    open_ ?loc ?attrs (if override then Override else Fresh)

  let list ?loc ?(attrs=[]) xs =
    let null = construct ?loc (lid ?loc "[]") None in
    let cons x xs = construct ?loc (lid ?loc "::") (Some (tuple ?loc [x;xs])) in
    { (List.fold_right cons xs null) with pexp_attributes = attrs }

  let with_desc e desc = { e with pexp_desc = desc }

  let is_simple_ext s e = match e.pexp_desc with
    | Pexp_extension ({ txt }, PStr []) -> txt = s
    | _ -> false
end

module Pat = struct
  include Pat
  let var' ?loc ?attrs s = var ?loc ?attrs (at ?loc s)
  let unit () = construct (at & Longident.Lident "()") None 

  exception Not_supported of expression

  let of_expr e = 
    let rec f e = 
      let loc = e.pexp_loc in
      let attrs = e.pexp_attributes in
      match e.pexp_desc with
      | Pexp_ident {txt=Lident s; loc=loc'} -> 
          Pat.var ~loc ~attrs {txt=s; loc=loc'} 
      | Pexp_constant c -> Pat.constant ~loc ~attrs c
      | Pexp_tuple es -> Pat.tuple ~loc ~attrs & List.map f es
      | Pexp_construct (lid, eopt) ->
          Pat.construct ~loc ~attrs lid & Option.map f eopt
      | Pexp_variant (l, eopt) ->
          Pat.variant ~loc ~attrs l & Option.map f eopt
      | Pexp_record (fields , None) ->
          Pat.record ~loc ~attrs (List.map (fun (lid, e) -> lid, f e) fields) Closed
      | Pexp_array es -> Pat.array ~loc ~attrs & List.map f es
      | Pexp_constraint (e, ty) -> Pat.constraint_ ~loc ~attrs (f e) ty
      | Pexp_lazy e -> Pat.lazy_ ~loc ~attrs & f e
      | Pexp_extension ({txt="p"}, PPat (p, None)) -> p
      | _ -> raise (Not_supported e)
    in
    try
      `Ok (f e)
    with
    | Not_supported e -> `Error e
end

module ExpPat = struct
  let var ?loc ?attrs s = (Exp.var ?loc ?attrs s, Pat.var' ?loc ?attrs s)
end

module Cf = struct
  include Cf

  let method_concrete ?loc ?attrs name ?(priv=false) ?(override=false) e = 
    Cf.method_ ?loc ?attrs name (if priv then Private else Public)
      (Cfk_concrete ((if override then Override else Fresh), e))
  let method_virtual ?loc ?attrs name ?(priv=false) cty = 
    Cf.method_ ?loc ?attrs name (if priv then Private else Public)
      (Cfk_virtual cty)
  let inherit_ ?loc ?attrs ?(override=false) =
    inherit_ ?loc ?attrs (if override then Override else Fresh)
  let concrete ?(override=false) =
    concrete (if override then Override else Fresh)
  (** [override]'s default is [false] *)
end

module Cstr = struct
  include Cstr
  let mk' ?(self= Pat.any ()) fields = Cstr.mk self fields
end

module Mod = struct
  include Ast_helper.Mod
  let ident' ?loc lid = ident ?loc (at ?loc lid)
end

module Attr = struct
  let ocaml ?loc k e = (at ?loc ("ocaml." ^ k), PStr [Str.eval e])
end

module Opn = struct
  open Opn
  let mk ?loc ?attrs ?docs ?(override=false) =
    mk ?loc ?attrs ?docs ~override:(if override then Override else Fresh)
end

module Sig = struct
  include Sig

  let type_ ?loc ?(rec_flag=Recursive) tds = type_ ?loc rec_flag tds
end

module Str = struct
  include Str

  let type_ ?loc ?(rec_flag=Recursive) tds = type_ ?loc rec_flag tds
end
  
module Val = Val
module Te = Te
module Mty = Mty

module Md = Md
module Mtd = Mtd
module Mb = Mb
module Incl = Incl
module Cty = Cty
module Ctf = Ctf
module Cl = Cl
module Ci = Ci
module Csig = Csig
module Type = Type
module Vb = Vb

let ocaml_warning ?loc s = 
  (* [@@ocaml.warning "-39"] *)
  (at ?loc "ocaml.warning",
   PStr [Str.eval ?loc (Exp.string s)])
