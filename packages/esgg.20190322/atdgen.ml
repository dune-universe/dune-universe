open Printf
open ExtLib
open Common

let loc = Atd.Ast.dummy_loc
let annot section l = (section, (loc, List.map (fun (k,v) -> k, (loc, Some v)) l))
let annots a = Atd.Annot.merge @@ List.map (fun (s,l) -> annot s l) a
let list ?(a=[]) t = Atd.Ast.List (loc,t,annots a)
let tuple l = Atd.Ast.Tuple (loc, List.map (fun t -> (loc,t,[])) l, [])
let record fields = Atd.Ast.Record (loc,fields,[])
let field ?(kind=Atd.Ast.Required) ?(a=[]) n t = `Field (loc, (n, kind, annots a), t)
let pname ?(a=[]) t params = Atd.Ast.Name (loc,(loc,t,params),annots a)
let tname ?a t = pname ?a t []
let nullable ?(a=[]) t = Atd.Ast.Nullable (loc,t,annots a)
let wrap t ocaml = Atd.Ast.Wrap (loc,t,annots ["ocaml",ocaml])
let tvar t = Atd.Ast.Tvar (loc,t)
let ptyp ?(a=[]) name params t = Atd.Ast.Type (loc, (name,params,annots a), t)
let typ ?a name t = ptyp ?a name [] t

let of_simple_type =
  function
  | `Int -> tname "int"
  | `Int64 -> tname ~a:["ocaml",["repr","int64"]] "int"
  | `String -> tname "string"
  | `Double -> tname "float"
  | `Bool -> tname "bool"
  | `Json -> tname "basic_json"

let wrap_ref ref t : Atd.Ast.type_expr = wrap t ["module",ES_name.to_ocaml ref]

let of_var_type {multi;ref;typ} : Atd.Ast.type_expr =
  let t = of_simple_type typ in
  let t = match ref with Some es_name -> wrap_ref es_name t | None -> t in
  match multi with
  | One -> t
  | Many -> list t

(* http://caml.inria.fr/pub/docs/manual-ocaml-4.07/manual049.html *)
let ocaml_keywords = [
"and";
"as";
"asr";
"assert";
"begin";
"class";
"constraint";
"do";
"done";
"downto";
"else";
"end";
"exception";
"external";
"false";
"for";
"fun";
"function";
"functor";
"if";
"in";
"include";
"inherit";
"initializer";
"land";
"lazy";
"let";
"lor";
"lsl";
"lsr";
"lxor";
"match";
"method";
"mod";
"module";
"mutable";
"new";
"nonrec";
"object";
"of";
"open!";
"open";
"or";
"private";
"rec";
"sig";
"struct";
"then";
"to";
"true";
"try";
"type";
"val";
"virtual";
"when";
"while";
"with";
]

let safe_ident name =
  let safe =
    if List.mem name ocaml_keywords then
      Some (name ^ "_")
    else
      let s' = to_valid_ident ~prefix:"t_" name in
      if name = s' then None else Some s'
  in
  match safe with
  | Some s -> ["json",["name",name]], s
  | None -> [], name

(* diligent copy of Ast.amap_type_expr *)
let rec reloc_type_expr f (x : Atd.Ast.type_expr) =
  match x with
      Atd.Ast.Sum (loc, vl, a) -> Atd.Ast.Sum (f loc, List.map (reloc_variant f) vl, reloc_annot f a)
    | Record (loc, fl, a) -> Record (f loc, List.map (reloc_field f) fl, reloc_annot f a)
    | Tuple (loc, tl, a) -> Tuple (f loc, List.map (reloc_cell f) tl, reloc_annot f a)
    | List (loc, t, a) -> List (f loc, reloc_type_expr f t, reloc_annot f a)
    | Option (loc, t, a) -> Option (f loc, reloc_type_expr f t, reloc_annot f a)
    | Nullable (loc, t, a) -> Nullable (f loc, reloc_type_expr f t, reloc_annot f a)
    | Shared (loc, t, a) -> Shared (f loc, reloc_type_expr f t, reloc_annot f a)
    | Wrap (loc, t, a) -> Wrap (f loc, reloc_type_expr f t, reloc_annot f a)
    | Tvar (loc, s) -> Tvar (f loc, s)
    | Name (loc, (loc2, name, args), a) ->
        Name (f loc, (f loc2, name, List.map (reloc_type_expr f) args), reloc_annot f a)

and reloc_variant f = function
    Atd.Ast.Variant (loc, (name, a), o) ->
      let o =
        match o with
            None -> None
          | Some x -> Some (reloc_type_expr f x)
      in
      Variant (f loc, (name, reloc_annot f a), o)
  | Inherit (loc, x) ->
      Inherit (f loc, reloc_type_expr f x)

and reloc_field f = function
    `Field (loc, (name, kind, a), x) ->
      `Field (f loc, (name, kind, reloc_annot f a), reloc_type_expr f x)
  | `Inherit (loc, x) ->
      `Inherit (f loc, reloc_type_expr f x)

and reloc_cell f (loc, x, a) =
  (f loc, reloc_type_expr f x, reloc_annot f a)

and reloc_annot f = List.map (fun (s,(loc,a)) -> (s,(f loc,List.map (fun (s,(loc,a)) -> (s,(f loc,a))) a)))


module New_types() : sig

open Atd.Ast

val new_ : module_item -> unit
val get : unit -> module_item list

end = struct

  let names = Hashtbl.create 10
  let types = ref []

  let fresh_name ?name t =
    let candidates =
      (match name with Some name -> [ name, name ] | None -> [])
      @
      match t with
      | Atd.Ast.Record (_,[`Field (_,(name,_,_),_)],_) -> [ name, name ]
      | _ -> []
    in
    let candidates = if candidates = [] then [ "t", "t0" ] else candidates in
    let check name =
      let (_,name) (* TODO *) = safe_ident name in
      match Hashtbl.mem names name with
      | true -> None
      | false -> Some name
    in
    let rec loop acc candidates n =
      match candidates with
      | [] -> loop [] (List.rev acc) (n+1)
      | (prefix,name)::xs ->
        match check name with
        | None -> loop ((prefix,sprintf "%s%d" prefix n)::acc) xs n
        | Some name -> name
    in
    loop [] candidates 1

  let new_ typ =
    tuck types typ;
    let Atd.Ast.Type (_,(name,_,_),_) = typ in
    assert (not @@ Hashtbl.mem names name);
    Hashtbl.add names name ()

  (* atdgen does topological sorting itself, but still lets try to keep natural order *)
  let replace name typ =
    match List.partition (function Atd.Ast.Type (_,(n,_,_),_) -> n = name) !types with
    | [Atd.Ast.Type (loc,(n,tp,annot),_)], rest -> types := (Type (loc,(n,tp,annot),typ)) :: rest
    | _ -> assert false

  let ref_ ?name t =
    let reloc = reloc_type_expr (fun _loc -> Atd.Ast.dummy_loc) in
    let t = reloc t in
    match t with
    | Atd.Ast.Record _ ->
      begin match List.find (fun (Atd.Ast.Type (_,_,v)) -> t = reloc v) !types with
      | Atd.Ast.Type (_,(name,[],_),_) -> tname name
      | _ -> assert false (* parametric type cannot match *)
      | exception _ ->
        let name = fresh_name ?name t in
        new_ @@ typ name t;
        tname name
      end
    | _ -> t

  (* remove nested records, producing more types as needed *)
  let unrecord name t =
    let rec map name t =
      match t with
      | Atd.Ast.Record (loc, fields, annot) -> ref_ ?name (map_record (loc, fields, annot))
      | Sum (_loc, _variants, _annot) -> fail "variants are not supported"
      | Tuple (loc, cells, annot) -> Tuple (loc, List.map (fun (loc,t,annot) -> loc, map None t, annot) cells, annot)
      | List (loc, t, annot) ->
        let name =
          match name with
          | Some name when String.ends_with name "s" && String.length name > 1 -> Some (String.slice ~last:(-1) name)
          | Some name -> Some (name ^ "_elem")
          | None -> None
        in
        List (loc, map name t, annot)
      | Option (loc, t, annot) -> Option (loc, map name t, annot)
      | Nullable (loc, t, annot) -> Nullable (loc, map name t, annot)
      | Shared (loc, t, annot) -> Shared (loc, map name t, annot)
      | Wrap (loc, t, annot) -> Wrap (loc, map name t, annot)
      | Name (loc, (loc', n, tl), annot) ->
        let name = match n, tl with "buckets", [_] -> name | _ -> None in
        Name (loc, (loc', n, List.map (map name) tl), annot)
      | Tvar _ -> t
    and map_record (loc,fields,annot) =
      let fields = fields |> List.map begin function
      | `Inherit _ -> fail "inherit not supported"
      | `Field (loc,(name,_,_ as f),t) -> `Field (loc, f, map (Some name) t)
      end
      in
      Record (loc,fields,annot)
    in
    match t with
    | Atd.Ast.Record (loc, fields, annot) -> map_record (loc, fields, annot) (* record type at top-level, no need to unnest *)
    | _ -> map (Some name) t

  let get () =
    !types |> List.rev |> List.iter (function Atd.Ast.Type (_,(name,_,_),t) -> replace name (unrecord name t));
    List.rev !types

end

let make_abstract ((_loc,annot),init) types =
  let name (Atd.Ast.Type (_,(name,_,_),_)) = name in
  types |> List.map begin fun t ->
    match List.find (fun i -> name i = name t) init with (* match by name, because initial types are not renamed *)
    | exception Not_found -> t
    | _ -> Atd.Ast.Type (loc, (name t,[],annot),tname "abstract")
  end

let safe_record map fields =
  fields |> List.map begin fun (name,t) ->
    let kind = match t with `Maybe _ -> Atd.Ast.Optional | `List _ -> With_default | _ -> Required in
    let (a,name) = safe_ident name in (* TODO check unique *)
    field ~a ~kind name (map t)
  end |> record

let basic_json = typ "basic_json" ~a:["ocaml",["module","Json";"t","t"]] (tname "abstract")

let of_shape ~init name (shape:result_type) : Atd.Ast.full_module =
  let module Types = New_types() in
  List.iter Types.new_ (snd init);
  Types.new_ @@ ptyp "doc_count" ["key"] (record [field "key" (tvar "key"); field "doc_count" (tname "int")]);
  Types.new_ @@ ptyp "buckets" ["a"] (record [field "buckets" (list (tvar "a"))]);
  Types.new_ @@ typ "int_as_float" (wrap (tname "float") ["t","int"; "wrap","int_of_float"; "unwrap","float_of_int"]);
  Types.new_ @@ ptyp "value_agg'" ["a"] (record [field "value" (tvar "a")]);
  Types.new_ @@ ptyp "value_agg" ["a"]
    (wrap (pname "value_agg'" [tvar "a"]) [ "t", "'a"; "wrap", "fun { value; } -> value"; "unwrap", "fun value -> { value; }"]);
  Types.new_ @@ basic_json;
  let rec map shape =
    match shape with
    | #simple_type as t -> of_simple_type t
    | `Maybe t -> nullable @@ map t
    | `Ref (ref,t) -> wrap_ref ref (of_simple_type t)
    | `List t -> list (map t)
    | `Object t -> list ~a:["json",["repr","object"]] (tuple [map `String; map t])
    | `Dict ["key",k; "doc_count", `Int] -> pname "doc_count" [map k]
    | `Dict ["buckets", `List t] -> pname "buckets" [map t]
    | `Dict ["value", `Dict ["override int as float hack", `Int]] -> pname "value_agg" [tname "int_as_float"]
    | `Dict ["value", t] -> pname "value_agg" [map t]
    | `Dict fields -> safe_record map fields
  in
  Types.new_ @@ typ name (map shape);
  (loc,[]), (make_abstract init (Types.get ()))

let of_vars ~init (l:input_vars) =
  let module Types = New_types() in
  List.iter Types.new_ (snd (init:Atd.Ast.full_module));
  Types.new_ basic_json;
  let rec map_field (req,t) : (_ * Atd.Ast.type_expr) =
    let t =
      match t with
      | `Group l -> map l
      | `Simple t ->
        match t with
        | None -> tname "basic_json"
        | Some t -> of_var_type t
    in
    match req with
    | `Required -> Atd.Ast.Required, t
    | `Optional -> Optional, nullable t
  and map l =
    match l with
    | [] -> tname "unit"
    | _ -> l |> List.map begin fun (name,value) ->
      let (a,name) = safe_ident name in (* TODO check unique *)
      let (kind,t) = map_field value in
      field ~a ~kind name t
     end |> record
  in
  Types.new_ @@ typ "input" (map l);
  (loc,[]), (make_abstract init (Types.get ()))

let parse_file filename =
  let open Atd in
  let ch = open_in filename in 
  let lexbuf = Lexing.from_channel ch in
  Parser.full_module Lexer.token lexbuf
