module Header = struct
(** {1 Coercions} *)

let int_of_expression (e : Parsetree.expression) : int =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match
    match e.pexp_desc with
    | Pexp_constant (Pconst_integer (value, _)) ->
        int_of_string_opt value
    | _ ->
        None
  with
  | Some result -> result
  | None ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Integer value expected"

let string_of_expression (e : Parsetree.expression) : string =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_constant (Pconst_string (value, _)) ->
      value
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "String value expected"

let string_of_arbitrary_expression (expression : Parsetree.expression)
    : string =
  match expression with
  | { pexp_desc = Pexp_constant (Pconst_string (s, _)); _ } -> s
  | _ ->
      Format.asprintf "%a" Pprintast.expression expression

let bool_of_expression (e : Parsetree.expression) : bool =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "false"; _ }, None) ->
      false
  | Pexp_construct ({ txt = Lident "true"; _ }, None) ->
      true
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Boolean value expected"

let pair_of_expression (e : Parsetree.expression)
    : Parsetree.expression * Parsetree.expression =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_tuple [a; b] -> (a, b)
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "Pair expected"

let rec list_of_expression (e : Parsetree.expression)
    : Parsetree.expression list =
  Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
  | Pexp_construct ({ txt = Lident "::"; _ }, Some pair) ->
      let (hd, tl) = pair_of_expression pair in
      hd :: list_of_expression tl
  | _ ->
      Location.raise_errorf ~loc:!Ast_helper.default_loc
        "List expected"

let list_of_tuple (e : Parsetree.expression) : Parsetree.expression list =
  match e.pexp_desc with
  | Pexp_tuple list -> list
  | Pexp_construct ({ txt = Lident "()"; _}, None) -> []
  | _ -> [e]

let structure_of_expression (e : Parsetree.expression) : Parsetree.structure =
  [Ast_helper.Str.eval e]

let lid_of_str (str : Ast_helper.str) : Ast_helper.lid =
  Location.mkloc (Longident.Lident str.txt) str.loc

(** {1 Location management} *)

let mkloc (txt : 'a) : 'a Location.loc =
  { txt; loc = !Ast_helper.default_loc }

let map_loc (f : 'a -> 'b) (l : 'a Location.loc) : 'b Location.loc =
  Ast_helper.with_default_loc l.loc (fun () -> { l with txt = f l.txt })

let with_loc (f : 'a -> 'b) (l : 'a Location.loc) : 'b =
  Ast_helper.with_default_loc l.loc (fun () -> f l.txt)

(** {1 Constructing identifiers } *)

let make_ident ?(prefix : Longident.t option) (s : string) : Longident.t =
  match prefix with
  | None -> Lident s
  | Some prefix -> Ldot (prefix, s)

let ident ?attrs (ident : Longident.t) : Parsetree.expression =
  Ast_helper.Exp.ident ?attrs (mkloc ident)

(** {1 Constructing function application} *)

let nolabel arg =
  (Asttypes.Nolabel, arg)

let nolabels args =
  List.map nolabel args

let apply ?attrs (f : Parsetree.expression)
    ?(labels : (string * Parsetree.expression) list = [])
    (args : Parsetree.expression list) : Parsetree.expression =
  Ast_helper.Exp.apply ?attrs f
    (List.map (fun (l, e) -> (Asttypes.Labelled l, e)) labels @ nolabels args)

(** {1 Generic signature for visitable nodes} *)

type 'a iterator_item = Ast_iterator.iterator -> 'a -> unit

type 'a mapper_item = Ast_mapper.mapper -> 'a -> 'a

type ('cell, 'contents) accessor = {
    get : 'cell -> 'contents;
    set : 'contents -> 'cell -> 'cell;
  }

module type VisitableS = sig
  type t

  val to_loc : t -> Location.t

  val iterator : (Ast_iterator.iterator, t iterator_item) accessor

  val mapper : (Ast_mapper.mapper, t mapper_item) accessor
end

(** {1 Generic signature for extensible nodes} *)

module type ExtensibleS = sig
  include VisitableS

  val extension : ?attrs:Parsetree.attributes -> Parsetree.extension -> t

  val destruct_extension : t -> Parsetree.extension option
end

module type PayloadS = sig
  type t

  val of_payload : Parsetree.payload -> t

  val to_payload : t -> Parsetree.payload
end

module type ItemS = sig
  include ExtensibleS

  include PayloadS with type t := t

  val of_list : t list -> t
end

module Cty : ExtensibleS with type t = Parsetree.class_type = struct
  type t = Parsetree.class_type

  let to_loc (cty : Parsetree.class_type) : Location.t =
    cty.pcty_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { class_type; _ } -> class_type);
    set = (fun class_type iterator -> { iterator with class_type })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { class_type; _ } -> class_type);
    set = (fun class_type mapper -> { mapper with class_type })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Cty.extension ?attrs e

  let destruct_extension (cty : Parsetree.class_type)
      : Parsetree.extension option =
    match cty.pcty_desc with
    | Pcty_extension e -> Some e
    | _ -> None
end

module Ctf : ExtensibleS with type t = Parsetree.class_type_field = struct
  type t = Parsetree.class_type_field

  let to_loc (ctf : Parsetree.class_type_field) : Location.t =
    ctf.pctf_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { class_type_field; _ } -> class_type_field);
    set = (fun class_type_field iterator -> { iterator with class_type_field })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { class_type_field; _ } -> class_type_field);
    set = (fun class_type_field mapper -> { mapper with class_type_field })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Ctf.extension ?attrs e

  let destruct_extension (ctf : Parsetree.class_type_field)
      : Parsetree.extension option =
    match ctf.pctf_desc with
    | Pctf_extension e -> Some e
    | _ -> None
end

module Cl : ExtensibleS with type t = Parsetree.class_expr = struct
  type t = Parsetree.class_expr

  let to_loc (cl : Parsetree.class_expr) : Location.t =
    cl.pcl_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { class_expr; _ } -> class_expr);
    set = (fun class_expr iterator -> { iterator with class_expr })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { class_expr; _ } -> class_expr);
    set = (fun class_expr mapper -> { mapper with class_expr })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Cl.extension ?attrs e

  let destruct_extension (cl : Parsetree.class_expr)
      : Parsetree.extension option =
    match cl.pcl_desc with
    | Pcl_extension e -> Some e
    | _ -> None
end

module Cf : ExtensibleS with type t = Parsetree.class_field = struct
  type t = Parsetree.class_field

  let to_loc (cf : Parsetree.class_field) : Location.t =
    cf.pcf_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { class_field; _ } -> class_field);
    set = (fun class_field iterator -> { iterator with class_field })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { class_field; _ } -> class_field);
    set = (fun class_field mapper -> { mapper with class_field })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Cf.extension ?attrs e

  let destruct_extension (cf : Parsetree.class_field)
      : Parsetree.extension option =
    match cf.pcf_desc with
    | Pcf_extension e -> Some e
    | _ -> None
end

module Mty : ExtensibleS with type t = Parsetree.module_type = struct
  type t = Parsetree.module_type

  let to_loc (mty : Parsetree.module_type) : Location.t =
    mty.pmty_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { module_type; _ } -> module_type);
    set = (fun module_type iterator -> { iterator with module_type })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { module_type; _ } -> module_type);
    set = (fun module_type mapper -> { mapper with module_type })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Mty.extension ?attrs e

  let destruct_extension (mty : Parsetree.module_type)
      : Parsetree.extension option =
    match mty.pmty_desc with
    | Pmty_extension e -> Some e
    | _ -> None
end

module Mod : ExtensibleS with type t = Parsetree.module_expr = struct
  type t = Parsetree.module_expr

  let to_loc (m : Parsetree.module_expr) : Location.t =
    m.pmod_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { module_expr; _ } -> module_expr);
    set = (fun module_expr iterator -> { iterator with module_expr })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { module_expr; _ } -> module_expr);
    set = (fun module_expr mapper -> { mapper with module_expr })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Mod.extension ?attrs e

  let destruct_extension (m : Parsetree.module_expr)
      : Parsetree.extension option =
    match m.pmod_desc with
    | Pmod_extension e -> Some e
    | _ -> None
end

let range_loc (first : Location.t) (last : Location.t) : Location.t = {
  loc_start = first.loc_start;
  loc_end = last.loc_end;
  loc_ghost = first.loc_ghost || last.loc_ghost;
}

module Stri = struct
  type t = Parsetree.structure_item

  let to_loc (s : Parsetree.structure_item) : Location.t =
    s.pstr_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { structure_item; _ } -> structure_item);
    set = (fun structure_item iterator -> { iterator with structure_item })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { structure_item; _ } -> structure_item);
    set = (fun structure_item mapper -> { mapper with structure_item })
  }
  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Str.extension ?attrs e

  let destruct_extension (s : Parsetree.structure_item)
      : Parsetree.extension option =
    match s.pstr_desc with
    | Pstr_extension (e, _) -> Some e
    | _ -> None

  let of_payload (payload : Parsetree.payload)
      : Parsetree.structure_item =
    match payload with
    | PStr [item] -> item
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc
          "Single structure item expected"

  let to_payload (item : Parsetree.structure_item) : Parsetree.payload =
    PStr [item]

  let of_list (structure : Parsetree.structure) : Parsetree.structure_item =
    Ast_helper.Str.include_ (Ast_helper.Incl.mk
      (Ast_helper.Mod.structure structure))
end

let list_to_loc (item_to_loc : 'a -> Location.t) (l : 'a list) : Location.t =
  match l with
  | [] -> !Ast_helper.default_loc
  | first :: tl ->
      let last = List.fold_left (fun _ last -> last) first tl in
      range_loc (item_to_loc first) (item_to_loc last)

module Str = struct
  type t = Parsetree.structure

  let to_loc (s : Parsetree.structure) : Location.t =
    list_to_loc Stri.to_loc s

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { structure; _ } -> structure);
    set = (fun structure iterator -> { iterator with structure })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { structure; _ } -> structure);
    set = (fun structure mapper -> { mapper with structure })
  }

  let of_payload (payload : Parsetree.payload) : Parsetree.structure =
    match payload with
    | PStr str -> str
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc "Structure expected"

  let to_payload (str : Parsetree.structure) : Parsetree.payload =
    PStr str
end

module Sigi = struct
  type t = Parsetree.signature_item

  let to_loc (s : Parsetree.signature_item) : Location.t =
    s.psig_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { signature_item; _ } -> signature_item);
    set = (fun signature_item iterator -> { iterator with signature_item })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { signature_item; _ } -> signature_item);
    set = (fun signature_item mapper -> { mapper with signature_item })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Sig.extension ?attrs e

  let destruct_extension (s : Parsetree.signature_item)
      : Parsetree.extension option =
    match s.psig_desc with
    | Psig_extension (e, _) -> Some e
    | _ -> None

  let of_payload (payload : Parsetree.payload) : Parsetree.signature_item =
    match payload with
    | PSig [item] -> item
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc
          "Single signature item expected"

  let to_payload (item : Parsetree.signature_item) : Parsetree.payload =
    PSig [item]

  let of_list (signature : Parsetree.signature) : Parsetree.signature_item =
    Ast_helper.Sig.include_ (Ast_helper.Incl.mk
      (Ast_helper.Mty.signature signature))
end

module Sig = struct
  type t = Parsetree.signature

  let to_loc (s : Parsetree.signature) : Location.t =
    list_to_loc Sigi.to_loc s

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { signature; _ } -> signature);
    set = (fun signature iterator -> { iterator with signature })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { signature; _ } -> signature);
    set = (fun signature mapper -> { mapper with signature })
  }

  let of_payload (payload : Parsetree.payload) : Parsetree.signature =
    match payload with
    | PSig sgn -> sgn
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc "Signature expected"

  let to_payload (signature : Parsetree.signature) : Parsetree.payload =
    PSig signature
end

type value = {
    exp : Parsetree.expression;
    pat : Parsetree.pattern;
  }

(** {1 Generic signature for expressions and patterns} *)

module type BaseValueS = sig
  include ExtensibleS

  val var : ?attrs:Parsetree.attributes -> string -> t

  val of_constant : ?attrs:Parsetree.attributes -> Parsetree.constant -> t

  val of_bytes : ?attrs:Parsetree.attributes -> bytes -> t

  val force_tuple : ?attrs:Parsetree.attributes -> t list -> t

  val force_construct :
      ?attrs:Parsetree.attributes -> Ast_helper.lid -> t option -> t

  val array : ?attrs:Parsetree.attributes -> t list -> t

  val record : ?attrs:Parsetree.attributes -> (Longident.t * t) list -> t

  val variant : ?attrs:Parsetree.attributes -> string -> t option -> t

  val lazy_ :  ?attrs:Parsetree.attributes -> t -> t

  val choice :
      (unit -> Parsetree.expression) -> (unit -> Parsetree.pattern) -> t

  val of_payload : Parsetree.payload -> t

  val to_payload : t -> Parsetree.payload
end

module type ValueS = sig
  include BaseValueS

  val of_int : ?attrs:Parsetree.attributes -> int -> t

  val of_string : ?attrs:Parsetree.attributes -> string -> t

  val of_char : ?attrs:Parsetree.attributes -> char -> t

  val of_unit : ?attrs:Parsetree.attributes -> unit -> t

  val of_bool : ?attrs:Parsetree.attributes -> bool -> t

  val of_float : ?attrs:Parsetree.attributes -> float -> t

  val of_int32 : ?attrs:Parsetree.attributes -> int32 -> t

  val of_int64 : ?attrs:Parsetree.attributes -> int64 -> t

  val of_nativeint : ?attrs:Parsetree.attributes -> nativeint -> t

  val none : ?attrs:Parsetree.attributes -> unit -> t

  val some : ?attrs:Parsetree.attributes -> t -> t

  val option : ?attrs:Parsetree.attributes -> t option -> t

  val of_longident : Longident.t -> t

  val construct : ?attrs:Parsetree.attributes -> Longident.t -> t list -> t

  val tuple : ?attrs:Parsetree.attributes -> t list -> t

  val nil : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> unit -> t

  val cons : ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t -> t -> t

  val list :
      ?attrs:Parsetree.attributes -> ?prefix:Longident.t -> t list -> t
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
    of_constant ?attrs (Ast_helper.Const.int i)

  let of_string ?attrs s =
    of_constant ?attrs (Ast_helper.Const.string s)

  let of_char ?attrs s =
    of_constant ?attrs (Ast_helper.Const.char s)

  let of_unit ?attrs () =
    force_construct ?attrs (mkloc (Longident.Lident unit_ctor)) None

  let of_float ?attrs f =
    of_constant ?attrs (Ast_helper.Const.float (string_of_float f))

  let of_int32 ?attrs i =
    of_constant ?attrs (Ast_helper.Const.int32 i)

  let of_int64 ?attrs i =
    of_constant ?attrs (Ast_helper.Const.int64 i)

  let of_nativeint ?attrs i =
    of_constant ?attrs (Ast_helper.Const.nativeint i)

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
end

include Header

module Exp = ExtendValue (struct
  type t = Parsetree.expression

  let to_loc (e : Parsetree.expression) : Location.t =
    e.pexp_loc

  let var ?attrs x =
    ident ?attrs (Lident x)

  let of_constant ?attrs cst =
    Ast_helper.Exp.constant ?attrs cst

  let of_bytes ?attrs b =
    apply ?attrs (ident (Ldot (Lident "Bytes", "of_string")))
      [of_constant (Ast_helper.Const.string (Bytes.to_string b))]

  let force_tuple ?attrs (args : t list) : t =
    Ast_helper.Exp.tuple ?attrs args

  let force_construct ?attrs (lid : Ast_helper.lid) (args : t option) : t =
    Ast_helper.Exp.construct ?attrs lid args

  let array ?attrs (items : t list) : t =
    Ast_helper.Exp.array ?attrs items

  let record ?attrs (fields : (Longident.t * t) list) : t =
    Ast_helper.Exp.record ?attrs
      (List.map (fun (field, value) -> (mkloc field, value)) fields)
      None

  let variant ?attrs (ctor : string) (arg : t option) : t =
    Ast_helper.Exp.variant ?attrs ctor arg

  let lazy_ ?attrs (arg : t) : t =
    Ast_helper.Exp.lazy_ ?attrs arg

  let choice (e : unit -> Parsetree.expression) (_p : unit ->Parsetree.pattern)
      : t =
    e ()

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { expr; _ } -> expr);
    set = (fun expr iterator -> { iterator with expr })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { expr; _ } -> expr);
    set = (fun expr mapper -> { mapper with expr })
  }

  let extension ?attrs (e : Parsetree.extension) =
    Ast_helper.Exp.extension ?attrs e

  let destruct_extension (e : Parsetree.expression)
      : Parsetree.extension option =
    match e.pexp_desc with
    | Pexp_extension extension -> Some extension
    | _ -> None

  let of_payload (payload : Parsetree.payload) : Parsetree.expression =
    match payload with
    | PStr [{ pstr_desc = Pstr_eval (expr, []); _ }] ->
        expr
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc
          "Expression expected"

  let to_payload (e : Parsetree.expression) : Parsetree.payload =
    PStr (structure_of_expression e)
end)

module Typ = struct
  type t = Parsetree.core_type

  let to_loc (ty : Parsetree.core_type) : Location.t =
    ty.ptyp_loc

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { typ; _ } -> typ);
    set = (fun typ iterator -> { iterator with typ })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { typ; _ } -> typ);
    set = (fun typ mapper -> { mapper with typ })
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    Ast_helper.Typ.extension ?attrs e

  let destruct_extension (ty : Parsetree.core_type)
      : Parsetree.extension option =
    match ty.ptyp_desc with
    | Ptyp_extension e -> Some e
    | _ -> None

  let of_payload (payload : Parsetree.payload) : Parsetree.core_type =
    match payload with
    | PTyp typ -> typ
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc "Type expected"

  let to_payload (typ : Parsetree.core_type) : Parsetree.payload =
    PTyp typ
end

module Footer = struct
module Pat = ExtendValue (struct
  type t = Parsetree.pattern

  let to_loc (p : Parsetree.pattern) : Location.t =
    p.ppat_loc

  let var ?attrs x =
    Ast_helper.Pat.var ?attrs (mkloc x)

  let of_constant ?attrs cst =
    Ast_helper.Pat.constant ?attrs cst

  let of_bytes ?attrs:_ _b =
    failwith "Pat.of_bytes: bytes cannot be turned into patterns"

  let force_tuple ?attrs (args : t list) : t =
    Ast_helper.Pat.tuple ?attrs args

  let force_construct ?attrs (lid : Ast_helper.lid) (args : t option) : t =
    Ast_helper.Pat.construct ?attrs lid args

  let record ?attrs (fields : (Longident.t * t) list) : t =
    Ast_helper.Pat.record ?attrs
      (List.map (fun (field, value) -> (mkloc field, value)) fields)
      Closed

  let array ?attrs (items : t list) : t =
    Ast_helper.Pat.array ?attrs items

  let variant ?attrs (ctor : string) (arg : t option) : t =
    Ast_helper.Pat.variant ?attrs ctor arg

  let lazy_ ?attrs (arg : t) : t =
    Ast_helper.Pat.lazy_ ?attrs arg

  let choice (_e : unit -> Parsetree.expression) (p : unit -> Parsetree.pattern)
      : t =
    p ()

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = (fun { pat; _ } -> pat);
    set = (fun pat iterator -> { iterator with pat })
  }

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = (fun { pat; _ } -> pat);
    set = (fun pat mapper -> { mapper with pat })
  }

  let extension ?attrs (e : Parsetree.extension) =
    Ast_helper.Pat.extension ?attrs e

  let destruct_extension (e : Parsetree.pattern) : Parsetree.extension option =
    match e.ppat_desc with
    | Ppat_extension extension -> Some extension
    | _ -> None

  let of_payload (payload : Parsetree.payload) : Parsetree.pattern =
    match payload with
    | PPat (pat, None) -> pat
    | _ ->
        Location.raise_errorf ~loc:!Ast_helper.default_loc "Pattern expected"
  let to_payload (pat : Parsetree.pattern) : Parsetree.payload =
    PPat (pat, None)
end)

module Value : ValueS with type t = value = ExtendValue (struct
  type t = value

  let rec split (l : value list)
      : Parsetree.expression list * Parsetree.pattern list =
    match l with
    | [] -> ([], [])
    | hd :: tl ->
        let (tl_exp, tl_pat) = split tl in
        (hd.exp :: tl_exp, hd.pat :: tl_pat)

  let split_option (o : value option)
      : Parsetree.expression option * Parsetree.pattern option =
      match o with
      | None -> (None, None)
      | Some { exp; pat } -> (Some exp, Some pat)

  let rec split_assoc (l : ('a * value) list)
      : ('a * Parsetree.expression) list * ('a * Parsetree.pattern) list =
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

  let force_construct ?attrs (lid : Ast_helper.lid) (args : t option) : t =
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

  let choice (e : unit -> Parsetree.expression) (p : unit -> Parsetree.pattern)
      : t =
    { exp = e ();
      pat = p (); }

  let no_iterator _ =
    failwith "value cannot be iterated"

  let iterator : (Ast_iterator.iterator, t iterator_item) accessor = {
    get = no_iterator;
    set = (fun _ -> no_iterator)
  }

  let no_mapper _ =
    failwith "value cannot be mapped"

  let mapper : (Ast_mapper.mapper, t mapper_item) accessor = {
    get = no_mapper;
    set = (fun _ -> no_mapper)
  }

  let extension ?attrs (e : Parsetree.extension) : t =
    { exp = Exp.extension ?attrs e;
      pat = Pat.extension ?attrs e; }

  let destruct_extension (v : value) : Parsetree.extension option =
    match v.exp.pexp_desc with
    | Pexp_extension extension -> Some extension
    | _ -> None

  let of_payload _ =
    failwith "value cannot be obtained from payload"

  let to_payload (v : value) : Parsetree.payload =
    Exp.to_payload v.exp
end)

(** {1 Payload extraction} *)

let int_of_payload (payload : Parsetree.payload) : int =
  int_of_expression (Exp.of_payload payload)

let string_of_payload (payload : Parsetree.payload) : string =
  string_of_expression (Exp.of_payload payload)

let bool_of_payload (payload : Parsetree.payload) : bool =
  bool_of_expression (Exp.of_payload payload)

(** {1 Payload construction (ctd) *)

let payload_of_int (i : int) : Parsetree.payload =
  Exp.to_payload (Exp.of_int i)

(** {1 Coercions (ctd)} *)

let sequence (list : Parsetree.expression list) : Parsetree.expression =
  match list with
  | [] -> Exp.of_unit ()
  | [singleton] -> singleton
  | hd :: tl ->
      List.fold_left Ast_helper.Exp.sequence hd tl

(** {1 General purpose functions} *)

let update f ref =
  let (result, new_contents) = f !ref in
  ref := new_contents;
  result

let mutate f ref =
  ref := f !ref
end

include Footer
