(** {1 Coercions} *)

let int_of_expression (e : Ppxlib.expression) : int =
  Ppxlib.Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match
    match e.pexp_desc with
    | Pexp_constant (Pconst_integer (value, _)) ->
        int_of_string_opt value
    | _ ->
        None
  with
  | Some result -> result
  | None ->
      Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
        "Integer value expected"

let destruct_string_constant (constant : Ppxlib.constant) : string option =
  match constant with
  | Pconst_string (s, _) -> Some s
  | _ -> None

let string_of_expression (expression : Ppxlib.expression) : string =
  Ppxlib.Ast_helper.with_default_loc expression.pexp_loc @@ fun () ->
  match
    match expression.pexp_desc with
    | Pexp_constant constant -> destruct_string_constant constant
    | _ -> None
  with
  | Some value -> value
  | _ ->
      Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
        "String value expected"

let string_of_arbitrary_expression (expression : Ppxlib.expression)
    : string =
  match
    match expression.pexp_desc with
    | Pexp_constant constant -> destruct_string_constant constant
    | _ -> None
  with
  | Some value -> value
  | _ ->
      Format.asprintf "%a" Ppxlib.Pprintast.expression expression

let bool_of_expression (e : Ppxlib.expression) : bool =
  Ppxlib.Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "false"; _ }, None) ->
      false
  | Pexp_construct ({ txt = Lident "true"; _ }, None) ->
      true
  | _ ->
      Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
        "Boolean value expected"

let pair_of_expression (e : Ppxlib.expression)
    : Ppxlib.expression * Ppxlib.expression =
  Ppxlib.Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_tuple [a; b] -> (a, b)
  | _ ->
      Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
        "Pair expected"

let rec list_of_expression (e : Ppxlib.expression)
    : Ppxlib.expression list =
  Ppxlib.Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
  | Pexp_construct ({ txt = Lident "::"; _ }, Some pair) ->
      let (hd, tl) = pair_of_expression pair in
      hd :: list_of_expression tl
  | _ ->
      Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
        "List expected"

let list_of_tuple (e : Ppxlib.expression) : Ppxlib.expression list =
  match e.pexp_desc with
  | Pexp_tuple list -> list
  | Pexp_construct ({ txt = Lident "()"; _}, None) -> []
  | _ -> [e]

let structure_of_expression (e : Ppxlib.expression) : Ppxlib.structure =
  [Ppxlib.Ast_helper.Str.eval e]

let lid_of_str (str : Ppxlib.Ast_helper.str) : Ppxlib.Ast_helper.lid =
  Location.mkloc (Longident.Lident str.txt) str.loc

(** {1 Location management} *)

let mkloc (txt : 'a) : 'a Location.loc =
  { txt; loc = !Ppxlib.Ast_helper.default_loc }

let map_loc (f : 'a -> 'b) (l : 'a Location.loc) : 'b Location.loc =
  Ppxlib.Ast_helper.with_default_loc l.loc (fun () -> { l with txt = f l.txt })

let with_loc (f : 'a -> 'b) (l : 'a Location.loc) : 'b =
  Ppxlib.Ast_helper.with_default_loc l.loc (fun () -> f l.txt)

(** {1 Constructing identifiers } *)

let make_ident ?(prefix : Longident.t option) (s : string) : Longident.t =
  match prefix with
  | None -> Lident s
  | Some prefix -> Ldot (prefix, s)

(** {1 Constructing function application} *)

let nolabel arg =
  (Ppxlib.Asttypes.Nolabel, arg)

let nolabels args =
  List.map nolabel args

let apply ?attrs (f : Ppxlib.expression)
    ?(labels : (string * Ppxlib.expression) list = [])
    ?(optional : (string * Ppxlib.expression) list = [])
    (args : Ppxlib.expression list) : Ppxlib.expression =
  Ppxlib.Ast_helper.Exp.apply ?attrs f
    (List.map (fun (l, e) -> (Ppxlib.Asttypes.Labelled l, e)) labels @
      List.map (fun (l, e) -> (Ppxlib.Asttypes.Optional l, e)) optional @
        nolabels args)

(** {1 Generic signature for visitable nodes} *)

type 'a iter = 'a -> unit

type 'a map = 'a -> 'a

module type VisitableS = sig
  type t

  val to_loc : t -> Location.t

  val iter : #Ppxlib.Ast_traverse.iter -> t iter

  val map : #Ppxlib.Ast_traverse.map -> t map
end

(** {1 Generic signature for extensible nodes} *)

type destruct_extension = Ppxlib.extension * Ppxlib.attributes

module type ExtensibleS = sig
  include VisitableS

  val extension : ?attrs:Ppxlib.attributes -> Ppxlib.extension -> t

  val destruct_extension : t -> destruct_extension option
end

module type PayloadS = sig
  type t

  val of_payload : Ppxlib.payload -> t

  val to_payload : t -> Ppxlib.payload
end

module type ItemS = sig
  include ExtensibleS

  include PayloadS with type t := t

  val of_list : t list -> t
end

module Cty : ExtensibleS with type t = Ppxlib.class_type = struct
  type t = Ppxlib.class_type

  let to_loc (cty : Ppxlib.class_type) : Location.t =
    cty.pcty_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#class_type

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#class_type

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Cty.extension ?attrs e

  let destruct_extension (cty : Ppxlib.class_type) :
      destruct_extension option =
    match cty.pcty_desc with
    | Pcty_extension e -> Some (e, cty.pcty_attributes)
    | _ -> None
end

module Ctf : ExtensibleS with type t = Ppxlib.class_type_field = struct
  type t = Ppxlib.class_type_field

  let to_loc (ctf : Ppxlib.class_type_field) : Location.t =
    ctf.pctf_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#class_type_field

  let map (map : #Ppxlib.Ast_traverse.map) : t map =    map#class_type_field

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Ctf.extension ?attrs e

  let destruct_extension (ctf : Ppxlib.class_type_field)
      : destruct_extension option =
    match ctf.pctf_desc with
    | Pctf_extension e -> Some (e, ctf.pctf_attributes)
    | _ -> None
end

module Cl : ExtensibleS with type t = Ppxlib.class_expr = struct
  type t = Ppxlib.class_expr

  let to_loc (cl : Ppxlib.class_expr) : Location.t =
    cl.pcl_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#class_expr

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#class_expr

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Cl.extension ?attrs e

  let destruct_extension (cl : Ppxlib.class_expr)
      : destruct_extension option =
    match cl.pcl_desc with
    | Pcl_extension e -> Some (e, cl.pcl_attributes)
    | _ -> None
end

module Cf : ExtensibleS with type t = Ppxlib.class_field = struct
  type t = Ppxlib.class_field

  let to_loc (cf : Ppxlib.class_field) : Location.t =
    cf.pcf_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#class_field

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#class_field

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Cf.extension ?attrs e

  let destruct_extension (cf : Ppxlib.class_field)
      : destruct_extension option =
    match cf.pcf_desc with
    | Pcf_extension e -> Some (e, cf.pcf_attributes)
    | _ -> None
end

module Mty : ExtensibleS with type t = Ppxlib.module_type = struct
  type t = Ppxlib.module_type

  let to_loc (mty : Ppxlib.module_type) : Location.t =
    mty.pmty_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#module_type

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#module_type

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Mty.extension ?attrs e

  let destruct_extension (mty : Ppxlib.module_type)
      : destruct_extension option =
    match mty.pmty_desc with
    | Pmty_extension e -> Some (e, mty.pmty_attributes)
    | _ -> None
end

module Mod : ExtensibleS with type t = Ppxlib.module_expr = struct
  type t = Ppxlib.module_expr

  let to_loc (m : Ppxlib.module_expr) : Location.t =
    m.pmod_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#module_expr

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#module_expr

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Mod.extension ?attrs e

  let destruct_extension (m : Ppxlib.module_expr)
      : destruct_extension option =
    match m.pmod_desc with
    | Pmod_extension e -> Some (e, m.pmod_attributes)
    | _ -> None
end

let range_loc (first : Location.t) (last : Location.t) : Location.t = {
  loc_start = first.loc_start;
  loc_end = last.loc_end;
  loc_ghost = first.loc_ghost || last.loc_ghost;
}

module Stri = struct
  type t = Ppxlib.structure_item

  let to_loc (s : Ppxlib.structure_item) : Location.t =
    s.pstr_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#structure_item

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#structure_item

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Str.extension ?attrs e

  let destruct_extension (s : Ppxlib.structure_item)
      : destruct_extension option =
    match s.pstr_desc with
    | Pstr_extension (e, attr) -> Some (e, attr)
    | _ -> None

  let of_payload (payload : Ppxlib.payload)
      : Ppxlib.structure_item =
    match payload with
    | PStr [item] -> item
    | _ ->
        Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
          "Single structure item expected"

  let to_payload (item : Ppxlib.structure_item) : Ppxlib.payload =
    PStr [item]

  let of_list (structure : Ppxlib.structure) : Ppxlib.structure_item =
    Ppxlib.Ast_helper.Str.include_ (Ppxlib.Ast_helper.Incl.mk
      (Ppxlib.Ast_helper.Mod.structure structure))
end

let list_to_loc (item_to_loc : 'a -> Location.t) (l : 'a list) : Location.t =
  match l with
  | [] -> !Ppxlib.Ast_helper.default_loc
  | first :: tl ->
      let last = List.fold_left (fun _ last -> last) first tl in
      range_loc (item_to_loc first) (item_to_loc last)

module Str = struct
  type t = Ppxlib.structure

  let to_loc (s : Ppxlib.structure) : Location.t =
    list_to_loc Stri.to_loc s

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#structure

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#structure

  let of_payload (payload : Ppxlib.payload) : Ppxlib.structure =
    match payload with
    | PStr str -> str
    | _ ->
        Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc "Structure expected"

  let to_payload (str : Ppxlib.structure) : Ppxlib.payload =
    PStr str
end

module Sigi = struct
  type t = Ppxlib.signature_item

  let to_loc (s : Ppxlib.signature_item) : Location.t =
    s.psig_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#signature_item

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#signature_item

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Sig.extension ?attrs e

  let destruct_extension (s : Ppxlib.signature_item)
      : destruct_extension option =
    match s.psig_desc with
    | Psig_extension (e, attr) -> Some (e, attr)
    | _ -> None

  let of_payload (payload : Ppxlib.payload) : Ppxlib.signature_item =
    match payload with
    | PSig [item] -> item
    | _ ->
        Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
          "Single signature item expected"

  let to_payload (item : Ppxlib.signature_item) : Ppxlib.payload =
    PSig [item]

  let of_list (signature : Ppxlib.signature) : Ppxlib.signature_item =
    Ppxlib.Ast_helper.Sig.include_ (Ppxlib.Ast_helper.Incl.mk
      (Ppxlib.Ast_helper.Mty.signature signature))
end

module Sig = struct
  type t = Ppxlib.signature

  let to_loc (s : Ppxlib.signature) : Location.t =
    list_to_loc Sigi.to_loc s

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#signature

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#signature

  let of_payload (payload : Ppxlib.payload) : Ppxlib.signature =
    match payload with
    | PSig sgn -> sgn
    | _ ->
        Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc "Signature expected"

  let to_payload (signature : Ppxlib.signature) : Ppxlib.payload =
    PSig signature
end

type value = {
    exp : Ppxlib.expression;
    pat : Ppxlib.pattern;
  }

(** {1 Generic signature for expressions and patterns} *)

module type BaseValueS = sig
  include ExtensibleS

  val var : ?attrs:Ppxlib.attributes -> string -> t

  val of_constant : ?attrs:Ppxlib.attributes -> Ppxlib.constant -> t

  val of_bytes : ?attrs:Ppxlib.attributes -> bytes -> t

  val force_tuple : ?attrs:Ppxlib.attributes -> t list -> t

  val force_construct :
      ?attrs:Ppxlib.attributes -> Ppxlib.Ast_helper.lid -> t option -> t

  val array : ?attrs:Ppxlib.attributes -> t list -> t

  val record : ?attrs:Ppxlib.attributes -> (Longident.t * t) list -> t

  val variant : ?attrs:Ppxlib.attributes -> string -> t option -> t

  val lazy_ :  ?attrs:Ppxlib.attributes -> t -> t

  val choice :
      (unit -> Ppxlib.expression) -> (unit -> Ppxlib.pattern) -> t

  val of_payload : Ppxlib.payload -> t

  val to_payload : t -> Ppxlib.payload
end

module type ValueS = sig
  include BaseValueS

  val of_int : ?attrs:Ppxlib.attributes -> int -> t

  val of_string : ?attrs:Ppxlib.attributes -> string -> t

  val of_char : ?attrs:Ppxlib.attributes -> char -> t

  val of_unit : ?attrs:Ppxlib.attributes -> unit -> t

  val of_bool : ?attrs:Ppxlib.attributes -> bool -> t

  val of_float : ?attrs:Ppxlib.attributes -> float -> t

  val of_int32 : ?attrs:Ppxlib.attributes -> int32 -> t

  val of_int64 : ?attrs:Ppxlib.attributes -> int64 -> t

  val of_nativeint : ?attrs:Ppxlib.attributes -> nativeint -> t

  val none : ?attrs:Ppxlib.attributes -> unit -> t

  val some : ?attrs:Ppxlib.attributes -> t -> t

  val option : ?attrs:Ppxlib.attributes -> t option -> t

  val of_longident : Longident.t -> t

  val construct : ?attrs:Ppxlib.attributes -> Longident.t -> t list -> t

  val tuple : ?attrs:Ppxlib.attributes -> t list -> t

  val nil : ?attrs:Ppxlib.attributes -> ?prefix:Longident.t -> unit -> t

  val cons : ?attrs:Ppxlib.attributes -> ?prefix:Longident.t -> t -> t -> t

  val list :
      ?attrs:Ppxlib.attributes -> ?prefix:Longident.t -> t list -> t
end

let unit_ctor = "()"

let none_ctor = "None"

let some_ctor = "Some"

let nil_ctor = "[]"

let cons_ctor = "::"

let longident = Longident.Lident "Longident"

module ExtendValue (Base : BaseValueS) : ValueS with type t = Base.t = struct
  include Base

  let of_int ?attrs i =
    of_constant ?attrs (Ppxlib.Ast_helper.Const.int i)

  let of_string ?attrs s =
    of_constant ?attrs (Ppxlib.Ast_helper.Const.string s)

  let of_char ?attrs s =
    of_constant ?attrs (Ppxlib.Ast_helper.Const.char s)

  let of_unit ?attrs () =
    force_construct ?attrs (mkloc (Longident.Lident unit_ctor)) None

  let of_float ?attrs f =
    of_constant ?attrs (Ppxlib.Ast_helper.Const.float (string_of_float f))

  let of_int32 ?attrs i =
    of_constant ?attrs (Ppxlib.Ast_helper.Const.int32 i)

  let of_int64 ?attrs i =
    of_constant ?attrs (Ppxlib.Ast_helper.Const.int64 i)

  let of_nativeint ?attrs i =
    of_constant ?attrs (Ppxlib.Ast_helper.Const.nativeint i)

  let tuple ?attrs (args : t list) : t =
    match args with
    | [] -> of_unit ?attrs ()
    | [arg] -> arg
    | _ -> force_tuple ?attrs args

  let construct ?attrs (ident : Longident.t) (args : t list) : t =
    let arg =
      match args with
      | [] -> None
      | [arg] -> Some arg
      | _ -> Some (tuple args) in
    force_construct ?attrs (mkloc ident) arg

  let of_bool ?attrs b =
    construct ?attrs (Lident (string_of_bool b)) []

  let none ?attrs () =
    construct ?attrs (Lident none_ctor) []

  let some ?attrs x =
    construct ?attrs (Lident some_ctor) [x]

  let option ?attrs opt =
    match opt with
    | None -> none ?attrs ()
    | Some x -> some ?attrs x

  let rec of_longident (ident : Longident.t) : t =
    match ident with
    | Ldot (m, v) ->
        construct (Ldot (longident, "Ldot"))
          [of_longident m; of_string v]
    | Lident ident ->
        construct (Ldot (longident, "Lident")) [of_string ident]
    | Lapply (f, x) ->
        construct (Ldot (longident, "Lapply"))
          [of_longident f; of_longident x]

  let nil ?attrs ?prefix () =
    construct ?attrs (make_ident ?prefix nil_ctor) []

  let cons ?attrs ?prefix hd tl =
    construct ?attrs (make_ident ?prefix cons_ctor) [hd; tl]

  let rec list ?attrs ?prefix l =
    match l with
    | [] -> nil ?attrs ?prefix ()
    | hd :: tl -> cons ?attrs ?prefix hd (list ?prefix tl)
end

module Exp = struct
  let ident ?loc ?attrs (ident : Longident.t) : Ppxlib.expression =
    Ppxlib.Ast_helper.Exp.ident ?loc ?attrs (mkloc ident)

  let ident_of_str ?attrs (str : Ppxlib.Ast_helper.str) : Ppxlib.expression =
    ident ?attrs ~loc:str.loc (Lident str.txt)

  include ExtendValue (struct
    type t = Ppxlib.expression

    let to_loc (e : Ppxlib.expression) : Location.t =
      e.pexp_loc

    let var ?attrs x =
      ident ?attrs (Lident x)

    let of_constant ?attrs cst =
      Ppxlib.Ast_helper.Exp.constant ?attrs cst

    let of_bytes ?attrs b =
      apply ?attrs (ident (Ldot (Lident "Bytes", "of_string")))
        [of_constant (Ppxlib.Ast_helper.Const.string (Bytes.to_string b))]

    let force_tuple ?attrs (args : t list) : t =
      Ppxlib.Ast_helper.Exp.tuple ?attrs args

    let force_construct ?attrs (lid : Ppxlib.Ast_helper.lid) (args : t option) : t =
      Ppxlib.Ast_helper.Exp.construct ?attrs lid args

    let array ?attrs (items : t list) : t =
      Ppxlib.Ast_helper.Exp.array ?attrs items

    let record ?attrs (fields : (Longident.t * t) list) : t =
      Ppxlib.Ast_helper.Exp.record ?attrs
        (List.map (fun (field, value) -> (mkloc field, value)) fields)
        None

    let variant ?attrs (ctor : string) (arg : t option) : t =
      Ppxlib.Ast_helper.Exp.variant ?attrs ctor arg

    let lazy_ ?attrs (arg : t) : t =
      Ppxlib.Ast_helper.Exp.lazy_ ?attrs arg

    let choice (e : unit -> Ppxlib.expression)
        (_p : unit ->Ppxlib.pattern) : t =
      e ()

    let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
      iter#expression

    let map (map : #Ppxlib.Ast_traverse.map) : t map =
      map#expression

    let extension ?attrs (e : Ppxlib.extension) =
      Ppxlib.Ast_helper.Exp.extension ?attrs e

    let destruct_extension (e : Ppxlib.expression)
        : destruct_extension option =
      match e.pexp_desc with
      | Pexp_extension extension -> Some (extension, e.pexp_attributes)
      | _ -> None

    let of_payload (payload : Ppxlib.payload) : Ppxlib.expression =
      match payload with
      | PStr [{ pstr_desc = Pstr_eval (expr, []); _ }] ->
          expr
      | _ ->
          Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc
            "Expression expected"

    let to_payload (e : Ppxlib.expression) : Ppxlib.payload =
      PStr (structure_of_expression e)
  end)
end

module Typ = struct
  type t = Ppxlib.core_type

  let to_loc (ty : Ppxlib.core_type) : Location.t =
    ty.ptyp_loc

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#core_type

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#core_type

  let extension ?attrs (e : Ppxlib.extension) : t =
    Ppxlib.Ast_helper.Typ.extension ?attrs e

  let destruct_extension (ty : Ppxlib.core_type)
      : destruct_extension option =
    match ty.ptyp_desc with
    | Ptyp_extension e -> Some (e, ty.ptyp_attributes)
    | _ -> None

  let of_payload (payload : Ppxlib.payload) : Ppxlib.core_type =
    match payload with
    | PTyp typ -> typ
    | _ ->
        Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc "Type expected"

  let to_payload (typ : Ppxlib.core_type) : Ppxlib.payload =
    PTyp typ
end

module Pat = ExtendValue (struct
  type t = Ppxlib.pattern

  let to_loc (p : Ppxlib.pattern) : Location.t =
    p.ppat_loc

  let var ?attrs x =
    Ppxlib.Ast_helper.Pat.var ?attrs (mkloc x)

  let of_constant ?attrs cst =
    Ppxlib.Ast_helper.Pat.constant ?attrs cst

  let of_bytes ?attrs:_ _b =
    failwith "Pat.of_bytes: bytes cannot be turned into patterns"

  let force_tuple ?attrs (args : t list) : t =
    Ppxlib.Ast_helper.Pat.tuple ?attrs args

  let force_construct ?attrs (lid : Ppxlib.Ast_helper.lid) (args : t option) : t =
    Ppxlib.Ast_helper.Pat.construct ?attrs lid args

  let record ?attrs (fields : (Longident.t * t) list) : t =
    Ppxlib.Ast_helper.Pat.record ?attrs
      (List.map (fun (field, value) -> (mkloc field, value)) fields)
      Closed

  let array ?attrs (items : t list) : t =
    Ppxlib.Ast_helper.Pat.array ?attrs items

  let variant ?attrs (ctor : string) (arg : t option) : t =
    Ppxlib.Ast_helper.Pat.variant ?attrs ctor arg

  let lazy_ ?attrs (arg : t) : t =
    Ppxlib.Ast_helper.Pat.lazy_ ?attrs arg

  let choice (_e : unit -> Ppxlib.expression) (p : unit -> Ppxlib.pattern)
      : t =
    p ()

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    iter#pattern

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    map#pattern

  let extension ?attrs (e : Ppxlib.extension) =
    Ppxlib.Ast_helper.Pat.extension ?attrs e

  let destruct_extension (e : Ppxlib.pattern) : destruct_extension option =
    match e.ppat_desc with
    | Ppat_extension extension -> Some (extension, e.ppat_attributes)
    | _ -> None

  let of_payload (payload : Ppxlib.payload) : Ppxlib.pattern =
    match payload with
    | PPat (pat, None) -> pat
    | _ ->
        Location.raise_errorf ~loc:!Ppxlib.Ast_helper.default_loc "Pattern expected"
  let to_payload (pat : Ppxlib.pattern) : Ppxlib.payload =
    PPat (pat, None)
end)

module Value : ValueS with type t = value = ExtendValue (struct
  type t = value

  let rec split (l : value list)
      : Ppxlib.expression list * Ppxlib.pattern list =
    match l with
    | [] -> ([], [])
    | hd :: tl ->
        let (tl_exp, tl_pat) = split tl in
        (hd.exp :: tl_exp, hd.pat :: tl_pat)

  let split_option (o : value option)
      : Ppxlib.expression option * Ppxlib.pattern option =
      match o with
      | None -> (None, None)
      | Some { exp; pat } -> (Some exp, Some pat)

  let rec split_assoc (l : ('a * value) list)
      : ('a * Ppxlib.expression) list * ('a * Ppxlib.pattern) list =
    match l with
    | [] -> ([], [])
    | (key, hd) :: tl ->
        let (tl_exp, tl_pat) = split_assoc tl in
        ((key, hd.exp) :: tl_exp, (key, hd.pat) :: tl_pat)

  let to_loc (v : value) : Location.t =
    v.exp.pexp_loc

  let var ?attrs x =
    { exp = Exp.var ?attrs x; pat = Pat.var ?attrs x }

  let of_constant ?attrs cst =
    { exp = Exp.of_constant ?attrs cst; pat = Pat.of_constant ?attrs cst }

  let of_bytes ?attrs b =
    { exp = Exp.of_bytes ?attrs b; pat = Pat.of_bytes ?attrs b }

  let force_tuple ?attrs (args : t list) : t =
    let args_exp, args_pat = split args in
    { exp = Exp.force_tuple ?attrs args_exp;
      pat = Pat.force_tuple ?attrs args_pat; }

  let force_construct ?attrs (lid : Ppxlib.Ast_helper.lid) (args : t option) : t =
    let args_exp, args_pat = split_option args in
    { exp = Exp.force_construct ?attrs lid args_exp;
      pat = Pat.force_construct ?attrs lid args_pat; }

  let array ?attrs (args : t list) : t =
    let args_exp, args_pat = split args in
    { exp = Exp.array ?attrs args_exp;
      pat = Pat.array ?attrs args_pat; }

  let record ?attrs (fields : (Longident.t * t) list) : t =
    let fields_exp, fields_pat = split_assoc fields in
    { exp = Exp.record ?attrs fields_exp;
      pat = Pat.record ?attrs fields_pat; }

  let variant ?attrs (ctor : string) (arg : t option) : t =
    let arg_exp, arg_pat = split_option arg in
    { exp = Exp.variant ?attrs ctor arg_exp;
      pat = Pat.variant ?attrs ctor arg_pat; }

  let lazy_ ?attrs (arg : t) : t =
    { exp = Exp.lazy_ ?attrs arg.exp;
      pat = Pat.lazy_ ?attrs arg.pat; }

  let choice (e : unit -> Ppxlib.expression) (p : unit -> Ppxlib.pattern)
      : t =
    { exp = e ();
      pat = p (); }

  let iter (iter : #Ppxlib.Ast_traverse.iter) : t iter =
    failwith "no iterator"

  let map (map : #Ppxlib.Ast_traverse.map) : t map =
    failwith "no mapper"

  let extension ?attrs (e : Ppxlib.extension) : t =
    { exp = Exp.extension ?attrs e;
      pat = Pat.extension ?attrs e; }

  let destruct_extension (v : value) : destruct_extension option =
    Exp.destruct_extension v.exp

  let of_payload _ =
    failwith "value cannot be obtained from payload"

  let to_payload (v : value) : Ppxlib.payload =
    Exp.to_payload v.exp
end)

(** {1 Payload extraction} *)

let int_of_payload (payload : Ppxlib.payload) : int =
  int_of_expression (Exp.of_payload payload)

let string_of_payload (payload : Ppxlib.payload) : string =
  string_of_expression (Exp.of_payload payload)

let bool_of_payload (payload : Ppxlib.payload) : bool =
  bool_of_expression (Exp.of_payload payload)

(** {1 Payload construction (ctd) *)

let payload_of_int (i : int) : Ppxlib.payload =
  Exp.to_payload (Exp.of_int i)

(** {1 Coercions (ctd)} *)

let sequence (list : Ppxlib.expression list) : Ppxlib.expression =
  match list with
  | [] -> Exp.of_unit ()
  | [singleton] -> singleton
  | hd :: tl ->
      List.fold_left Ppxlib.Ast_helper.Exp.sequence hd tl

(** {1 General purpose functions} *)

let update f ref =
  let (result, new_contents) = f !ref in
  ref := new_contents;
  result

let mutate f ref =
  ref := f !ref

module Accu = Accu
