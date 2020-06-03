open Ast_405
open Ast_convenience_405

open Spotlib.Spot
open Longident
open Asttypes
open Parsetree
open Ppxx.Helper

module Convert_to_current = Convert(OCaml_405)(OCaml_current)
module Convert_from_current = Convert(OCaml_current)(OCaml_405)

let string_of_core_type = 
  Ppx_deriving.string_of_core_type *< Convert_to_current.copy_core_type
  
let (^.^) x y = x ^ "." ^ y

(* poly_a for 'a *)
let poly_var v = "poly_" ^ v

(** <'a -> 'b t> ==>   ( <'a 'b . 'a -> 'b t>, ["poly_a"; "poly_b"], <poly_a -> poly_b t> ) *)
let gadtize ty =
  let open Ast_mapper in
  let vars = ref [] in
  let extend super = 
    let typ self cty = match cty.ptyp_desc with
      | Ptyp_var a -> 
          let n = poly_var a in
          if not & List.mem a !vars then vars := a :: !vars;
          { cty with ptyp_desc = Ptyp_constr ({ txt= Lident n; loc= cty.ptyp_loc }, []) }
      | _ -> super.typ self cty
    in
    { super with typ }
  in
  let mapper = extend Ast_mapper.default_mapper in
  let ty' = mapper.typ mapper ty in
  Typ.poly (List.map (!@) !vars) ty,
  List.map poly_var !vars,
  ty'

(** let [n] : [ty] = body, but with gadt type thing, i.e.
    let [n] : type a b . ty[a/'a,b/'b] = body 
 *)
let vb_newtypes n ty body =
  let poly, vars, ty' = gadtize ty in
  let exp = Exp.constraint_ body ty' in
  let exp = List.fold_right Exp.newtype (List.map (!@) vars) exp in
  Vb.mk (Pat.constraint_ (Pat.var' n) poly) exp

(** Make [@ocaml.<k> <e>] *)
let ocaml_attr k e =
  ({ txt = "ocaml" ^.^ k; loc = e.pexp_loc },
   PStr [Str.eval e])

(* It seems there is no good way to surpress the warning 39
   of a toplevel let locally via attributes.
   (See http://caml.inria.fr/mantis/view.php?id=6677)

   let rec x = 1
   and y = 2

   let [@ocaml.warning "-39"] rec x = 1        is not valid

   We have a workaround here:

   let (x,y) =
     let [@ocaml.warning "-39"] rec x = 1
     and y = 2
     in
     (x,y)
*)

(** Get <p> of [@conv.<k> <p>] *)
let get_conv_attribute k attrs =
  List.filter_map (fun ({txt}, p) ->
    match String.is_prefix' "conv." txt with
    | Some s when s = k -> Some p
    | _ -> None) attrs

(** Payload of [@conv.<k>] *)
let parse_conv_payload = function
  | PStr [] -> `None
  | PStr [ { pstr_desc = Pstr_eval (e, _) } ] ->
      let rec parse_payload_expr e = match e.pexp_desc with
        | Pexp_ident {txt = Longident.Lident s} -> `Ident s   (* x *)
        | Pexp_construct ({txt= Longident.Lident s}, None) -> `Constr s (* X.x *)
        | Pexp_constant (Pconst_string (s, _)) -> `String s (* "x" *)
        | Pexp_record (fields, None) -> (* { x=e; ..; x=e } *)
            `Record (List.map (function
              | ({txt=Longident.Lident s}, e) -> s, parse_payload_expr e
              | _ -> failwith "[@conv.xxx] payload record cannot be qualified: x is ok, but X.x is not.") fields)
        | _ -> failwith "[@conv.xxx] payload format error"
      in
      parse_payload_expr e
  | _ -> failwith "[@conv.xxx] payload format error"
      
(** [@conv.as <name>] 
    <name> can be an ident/string/constr or records whose keys are modes: { ocaml="..."; sexp="..." }
*)
let conv_as mode default attrs =
  match
    get_conv_attribute "as" attrs 
    |> List.map parse_conv_payload
  with
  | [] -> default
  | [(`Ident s | `String s | `Constr s)] -> s
  | [`Record r] -> 
      begin match List.assoc_opt mode r with
      | None -> default
      | Some (`Ident s | `String s | `Constr s) -> s
      | _ -> failwith "[@conv.as ...] payload format error"
      end
  | _ -> failwith "Multiple [@conv.as ...] are not allowed" 

(** [@conv.ignore_unknown_fields {ocaml; sexp}] *)      
let conv_ignore_unknown_fields mode attrs =
  match
    get_conv_attribute "ignore_unknown_fields" attrs 
    |> List.map parse_conv_payload
  with
  | [] -> false
  | [`None] -> true
  | [`Record r] -> 
      begin match List.assoc_opt mode r with
      | None -> false
      | Some (`Ident s) when mode = s -> true 
      | _ -> failwith "[@conv.ignore_unknown_fields ...] payload format error"
      end
  | _ -> failwith "[@conv.ignore_unknown_fields] payload format error"

(** let (x,y,z) = let rec x = .. and y = .. and z = .. in (x,y,z) 
    This indirection is required for [@ocaml.warning "-39"].
    Probably this will not be required in future.

    We also do a fix for Warning 32 here
*)
let str_value_maybe_rec vbs =
  let rec the_var_of_pattern p = match p.ppat_desc with
    | Ppat_var sloc -> sloc
    | Ppat_constraint (p,_) -> the_var_of_pattern p
    | _ -> assert false
  in
  let the_var_of_vb vb = the_var_of_pattern vb.pvb_pat in
  let vars = List.map the_var_of_vb vbs in
  Str.value Nonrecursive
    [ Vb.mk (ptuple & List.map (fun {txt;loc} -> pvar ~loc txt) vars)
        (Exp.let_ Recursive vbs ~attrs:[ocaml_attr "warning" (str "-27-39")]

         & tuple & List.map (fun {txt;loc} -> evar ~loc txt) vars)
    ]
  :: List.map (fun {txt;loc} -> 
      Str.value Nonrecursive [ Vb.mk (Pat.any ()) (Exp.var ~loc txt) ] ) vars
                         
let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "a%d"

module Make(M : sig
  val deriver : string
  (** ex. conv *)

  val name : string
  (** ex. ocaml *)

  val type_ : string
  (** ex. s *)

  val conv : string
  (** ex. Ocaml_conv *)
end) : sig
  module Encoder: sig
    val str_of_type 
          : is_gadt:bool 
            -> path:string list 
            -> type_declaration 
            -> value_binding list

    val sig_of_type 
        : path:string list  (* XXX not used! *)
          -> type_declaration 
          -> signature_item list

    val expr_of_typ 
        : (string * string) list (*+ env from type var names to variables *)
          -> core_type 
          -> expression
  end

  module Decoder: sig
    val str_of_type 
        : path:string list 
          -> type_declaration 
          -> value_binding list

    val sig_of_type 
        : path:string list  (* XXX not used! *)
          -> type_declaration 
          -> signature_item list

    val expr_of_typ : core_type -> expression
    val expr_of_typ_exn : core_type -> expression
  end
end = struct

  (* Ocaml.t *)    
  let the_target_type = lid M.type_

  (* Ocaml_conv.s *)
  let conv s = M.conv ^.^ s

  (* Ocaml_conv.s *)
  let econv s = Exp.ident & lid & M.conv ^.^ s

  (** True if [ty] is [<M.type_> mc_leftovers] *)
  let is_for_leftovers ty = match ty with
    | [%type: [%t? typ] mc_leftovers] ->
        begin match typ with
        | { ptyp_desc = Ptyp_constr ({txt}, []) } -> txt = the_target_type.txt
        | _ -> false
        end
    | _ -> false

  (** True if [ty] is [<M.type_> mc_option_embeded] or [<M.type_> mc_embeded] *)
  let is_for_embeded (l, _, ty) = match ty with
    | [%type: [%t? typ] mc_option_embeded] -> Some (l, `Option_embeded, typ)
    | [%type: [%t? typ] mc_embeded]        -> Some (l, `Embeded,        typ)
    | _ -> None

  (** True if [ty] is [t mc_option] for some [t] *)
  let is_mc_option ty = match ty with
    | [%type: [%t? typ] mc_option] -> Some typ
    | _ -> None

  module Encoder = struct

    type fields = 
      | Field of string * core_type * string
      | OptionField of string * core_type * string
      | Leftovers of string
      | Embeded of core_type * string
      | Option_embeded of core_type * string

    (** conv.ocaml_of *)   
    let deriver = M.deriver ^.^ M.name ^ "_of"

    (** ocaml_of_<n> *)
    let of_ n = Exp.ident & lid & M.name ^ "_of_" ^ n

    (** Ocaml_conv.Constr.variant *)
    let constr n = econv & "Constr" ^.^ n

    (** Ocaml_conv.Deconstr.variant *)
    let deconstr n = econv & "Deconstr" ^.^ n

    (** M.ocaml_of_t for M.t *)
    let mangle_lid = Ppx_deriving.mangle_lid ~fixpoint:" impos " (`Prefix (M.name ^ "_of"))

    (** ocaml_of_t for type t *)      
    let mangle_type_decl = Ppx_deriving.mangle_type_decl ~fixpoint:" impos " (`Prefix (M.name ^ "_of"))

    let rec expr_of_typ env typ = 
      if typ.ptyp_loc == Location.none then assert false;
      (* Location.print_loc and others somehow breaks already printed things *)
      Ast_helper.with_default_loc typ.ptyp_loc & fun () ->
      match typ with
      | [%type: int       ]                        -> of_ "int"
      | [%type: int32     ] | [%type: Int32.t]     -> of_ "int32"
      | [%type: int64     ] | [%type: Int64.t]     -> of_ "int64"
      | [%type: nativeint ] | [%type: Nativeint.t] -> of_ "nativeint"
      | [%type: float     ]                        -> of_ "float"
      | [%type: bool      ]                        -> of_ "bool"
      | [%type: char      ]                        -> of_ "char"
      | [%type: string    ]                        -> of_ "string"
      | [%type: bytes     ]                        -> of_ "bytes"
      | [%type: [%t? typ] ref]   ->
          [%expr [%e of_ "ref"] [%e expr_of_typ env  typ] ]
      | [%type: [%t? typ] list]  ->
          [%expr [%e of_ "list"] [%e expr_of_typ env  typ] ]
      | [%type: [%t? typ] array] ->
          [%expr [%e of_ "array"] [%e expr_of_typ env  typ] ]
      | [%type: [%t? typ] option] ->
          [%expr [%e of_ "option"] [%e expr_of_typ env  typ] ]
      | { ptyp_desc = Ptyp_arrow (_l, t1, t2) } ->
          [%expr [%e of_ "arrow"] [%e expr_of_typ env  t1] [%e expr_of_typ env  t2]]
      | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ env  typ
      | { ptyp_desc = Ptyp_var name } ->
          begin match List.assoc_opt name env with
          | Some name' -> [%expr [%e evar name']]
          | None -> [%expr fun _ -> assert false]
          end
      | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
          let args_pp = List.map (fun typ -> [%expr [%e expr_of_typ env  typ]]) args in
            app (Exp.lident (mangle_lid lid)) args_pp
      | { ptyp_desc = Ptyp_tuple typs } ->
          let args = List.mapi (fun i typ -> app (expr_of_typ env  typ) [evar (argn i)]) typs in
          [%expr
              fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
                [%e constr "tuple"] [%e list args]
          ]
      | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc } ->
          let cases =
            fields |> List.map (fun field ->
              match field with
              | Rtag (label, attrs, true (*empty*), []) -> (* `L *)
                  let label' = conv_as M.name label attrs in
                  Exp.case (Pat.variant label None)
                    [%expr
                        [%e constr "poly_variant" ]
                        [%e str "<poly_var>"]
                        [%e str label']
                        []
                    ]
              | Rtag (label, attrs, false, [typ]) -> (* `L of typ *)
                  let label' = conv_as M.name label attrs in
                  Exp.case (Pat.variant label (Some [%pat? x]))
                    [%expr
                        [%e constr "poly_variant" ]
                        [%e str "<poly_var>"]
                        [%e str label']
                        [ [%e expr_of_typ env  typ] x ]
                    ]
              | Rinherit ({ ptyp_desc = Ptyp_constr (tname, []) } as typ) -> (* [ tname ] *)
                  Exp.case [%pat? [%p Pat.type_ tname] as x]
                    [%expr [%e expr_of_typ env  typ] x]
              | _ ->
                  raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                      deriver (string_of_core_type typ))
          in
          Exp.function_ cases

      | { ptyp_desc = Ptyp_object (_fields, Open); ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for open object type %s"
            deriver (string_of_core_type typ)
          
      | { ptyp_desc = Ptyp_object (labels, Closed) } ->
          let make_block =
            [%expr 
                [%e constr "object_"]
                [%e str "<object>"]
            ]
          in
          let get name = Exp.send (evar "x") name in
          let make_fields = function
            | Field (name', ty, name) ->
                [%expr [ ([%e str name'],
                          [%e expr_of_typ env  ty] [%e get !@name ]) ] 
                ]
            | OptionField (name', ty, name) ->
                [%expr
                    match [%e get !@name] with
                    | None -> []
                    | Some __v -> [ [%e str name'], 
                                    [%e expr_of_typ env  ty] __v ]
                ]
            | Leftovers name -> get !@name
            | Embeded (ty, name) ->
                [%expr
                    [%e deconstr "object_"]
                    "<object>"
                    ([%e expr_of_typ env  ty] [%e get !@name])
                ]
            | Option_embeded (ty, name) ->
                [%expr 
                    match [%e get !@name ] with
                    | None -> []
                    | Some o -> 
                        [%e deconstr "object_"]
                          "<object>"
                          ([%e expr_of_typ env  ty] o)
                ]
          in
          [%expr fun x ->
            [%e expr_of_fields ~make_block ~make_fields labels
            ]
          ]
          
      | { ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
            deriver (string_of_core_type typ)

    (* has free variable [x] inside *)            
    and expr_of_fields ~make_block ~make_fields (labels : (string loc * attributes * core_type) list) = 
      let fields =
        labels |> List.map (fun ({txt=name}, attrs, ty) ->
          if is_for_leftovers ty then Leftovers name
          else 
            match is_for_embeded (name, attrs, ty) with
            | None ->
                let name' = conv_as M.name name attrs in
                begin match is_mc_option ty with
                | None -> Field (name', ty, name)
                | Some ty -> OptionField (name', ty, name)
                end
            | Some (_, `Embeded, typ) -> Embeded (typ, name)
            | Some (_, `Option_embeded, typ) -> Option_embeded (typ, name)
        )
      in
      (* <make_block> (List.concat <map make_fields fields>) *)
      [%expr 
        (* CR jfuruse: record must check the dups. Does it do already? *)
        [%e make_block] (List.concat [%e list & List.map make_fields fields])
      ]

    let sig_of_encoder type_decl_current =
      let typ = Ppx_deriving.core_type_of_type_decl type_decl_current in
      let enc typ = Ast_convenience.tconstr (M.conv ^.^ "encoder") [typ] in
      let polymorphize = Ppx_deriving.poly_arrow_of_type_decl enc type_decl_current in
      Convert_from_current.copy_core_type & polymorphize (enc typ)

    let sig_of_type ~path:_ type_decl =
      Ast_helper.with_default_loc type_decl.ptype_loc & fun () ->
      let type_decl_current = Convert_to_current.copy_type_declaration type_decl in
      [ Sig.value & Val.mk
          (at (mangle_type_decl type_decl_current))
          (sig_of_encoder type_decl_current)
      ]

    (** build [ocaml_of_t], [t_of_ocaml] etc for type [t] *)       
    let str_of_type ~is_gadt ~path ({ ptype_loc = loc } as type_decl) =
      Ast_helper.with_default_loc loc & fun () ->
      (* let path = Ppx_deriving.path_of_type_decl ~path type_decl in *)
      let type_name = String.concat "." (path @ [type_decl.ptype_name.txt]) in
      let conv_of =
        let env = List.concat_map (fun (ty, _) ->
          match ty.ptyp_desc with
          | Ptyp_any -> []
          | Ptyp_var v -> [(v, poly_var v)]
          | _ -> assert false) type_decl.ptype_params
        in

        (* has free var [x] *)
        let record (labels : label_declaration list) : expression =
          let make_block =
            (* XXX_conv.Constr.record "<type_name>" *)
            [%expr
                [%e constr "record"]
                [%e str type_name]
            ]
          in
          let get n =
            (* x.<n> *)
            Exp.field (evar "x") & lid n
          in
          let make_fields = function
            | Field (name', ty, name) ->
                (* [ "<name'>", xxx_of_ty (x.<name>) ] *)
                [%expr [ [%e str name'],
                         [%e expr_of_typ env ty]
                           [%e get name ] ] ]
            | OptionField (name', ty, name) ->
                (* match x.<name> with
                   | None -> []
                   | Some __v -> [ "<name'>", xxx_of_ty __v ]
                *)
                [%expr
                    match [%e get name ] with
                    | None -> []
                    | Some __v -> [ [%e str name'], 
                                    [%e expr_of_typ env  ty] __v ]
                ]
            | Leftovers name ->
                (* x.<name> *)
                get name
            | Embeded (ty, name) ->
                (* XXX_conv.Deconstr.record "<embeded record>"
                     (xxx_of_ty x.<name>)
                *)
                [%expr
                    [%e deconstr "record"]
                    "<embeded record>" (* XXX must be type name of ty *)
                    ([%e expr_of_typ env  ty]
                        [%e get name ])
                ]
            | Option_embeded (ty, name) ->
                (* match x.<name> with
                   | None -> []
                   | Some o -> 
                       XXX_conv.Deconstr.record "<embeded record>" 
                         (xxx_of_ty x.<name>)
                *)
                [%expr 
                    match [%e get name ] with
                    | None -> []
                    | Some o -> 
                        [%expr
                            [%e deconstr "record"]
                            "<embeded record>" (* XXX must be type name of ty *)
                            ([%e expr_of_typ env ty] o)
                        ]
                ]
          in
          expr_of_fields ~make_block ~make_fields 
          &  List.map (fun pld ->
            ( pld.pld_name,
              pld.pld_attributes,
              pld.pld_type )) labels
        in
        match type_decl.ptype_kind, type_decl.ptype_manifest with
        | Ptype_abstract, Some manifest ->
            (* XXX Do we really need this eta expansion? *)
            (* fun x -> xxx_of_manifest x *)
            [%expr fun x -> [%e expr_of_typ env  manifest] x]
        | Ptype_variant constrs, _ ->
            let cases =
              constrs |> List.map (fun { pcd_name= { txt= constr_name }
                                       ; pcd_args
                                       ; pcd_attributes= attrs
                                       ; pcd_res } ->
                let env = match pcd_res with
                  | None -> env
                  | Some ( { ptyp_desc= Ptyp_constr (_lidloc, ctys) } ) ->
                      (* It's gadt. [poly_a] can be not for ['a]. *)
                      List.concat_map (function
                        | (({ptyp_desc= Ptyp_var vo},_), {ptyp_desc= Ptyp_var v}) -> [(v, poly_var vo)]
                        | _ -> [] (* CR jfuruse: we cannot do further? *))
                        (List.combine type_decl.ptype_params ctys)
                  | _ -> assert false
                in
                let constr_name' = conv_as M.name constr_name attrs in
                let args = match pcd_args with
                  | Pcstr_tuple ctys ->
                      List.mapi (fun i typ ->
                        (* expr_of_<typ> a<i> *)
                        app (expr_of_typ env typ) [evar (argn i)]) ctys
                  | Pcstr_record lds ->
                      [ record lds ]
                in
                let result =
                  (* Ocaml_conv.Constr.variant "XXX.<type_name>" "<constr_name'>" [<args>] *)
                  [%expr [%e constr "variant"]
                      [%e str type_name]
                      [%e str constr_name']
                      [%e list args]
                  ]
                in
                match pcd_args with
                | Pcstr_tuple ctys ->
                    (* <constr_name>(a0,...,an) -> <result> *)
                    Exp.case (pconstr constr_name (List.mapi (fun i _ -> pvar (argn i)) ctys)) result
                | Pcstr_record _ ->
                    (* <constr_name> x -> {l0=a0;...;ln=a0} *)
                    Exp.case (pconstr constr_name [pvar "x"]) result
              )
            in
            Exp.function_ cases
        | Ptype_record labels, _ -> [%expr fun x -> [%e record labels]]

        | Ptype_abstract, None ->
            raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
        | Ptype_open, _        ->
            raise_errorf ~loc "%s cannot be derived for open types" deriver
      in
      let type_decl_current = Convert_to_current.copy_type_declaration type_decl in
      let polymorphize = 
        Convert_from_current.copy_expression
        *< Ppx_deriving.poly_fun_of_type_decl type_decl_current 
        *< Convert_to_current.copy_expression
      in
      if is_gadt then
        let ty = sig_of_encoder type_decl_current in
        [vb_newtypes (mangle_type_decl type_decl_current)
            ty
            (polymorphize conv_of)
        ]
      else
        [Vb.mk
            (pvar (mangle_type_decl type_decl_current))
            (polymorphize conv_of)
        ]
  end

  module Decoder = struct

    let deriver = M.deriver ^.^ "of_" ^ M.name

    (* n_of_ocaml *)
    let of_ n = Exp.ident & lid & n ^ "_of_" ^ M.name

    (* e => Ocaml_conv.exn e *)  
    let exn e = [%expr [%e econv "exn" ] [%e e]]

    (* Ocaml_conv.DeconstrDecoder.variant_exn *)
    let deconstr n = econv & "DeconstrDecoder" ^.^ n ^ "_exn"

    (* M.t_of_ocaml_exn for M.t *)
    let mangle_lid_exn = Ppx_deriving.mangle_lid ~fixpoint:" impos " (`Suffix ("of_" ^ M.name ^ "_exn"))

    (* M.t_of_ocaml for M.t *)
    let mangle_lid = Ppx_deriving.mangle_lid ~fixpoint:" impos " (`Suffix ("of_" ^ M.name))

    (* t_of_ocaml_exn for type t*)
    let mangle_type_decl_exn = Ppx_deriving.mangle_type_decl ~fixpoint:" impos " (`Suffix ("of_" ^ M.name ^ "_exn"))

    (* t_of_ocaml for type t *)
    let mangle_type_decl = Ppx_deriving.mangle_type_decl ~fixpoint:" impos " (`Suffix ("of_" ^ M.name))

    (* t Ocaml_conv.decoder_exn *)
    let decoder_exn typ = 
      Convert_to_current.copy_core_type
      & tconstr (conv "decoder_exn") [ Convert_from_current.copy_core_type typ]

    (* t Ocaml_conv.decoder *)
    let decoder typ = Convert_to_current.copy_core_type 
                      & tconstr (conv "decoder") [ Convert_from_current.copy_core_type typ]

    (* type of t_of_ocaml_exn, ex. 'a Ocaml_conv.decoder -> 'a t Ocaml_conv.decoder_exn *)
    let sig_of_decoder_exn type_decl =
      let typ = Ppx_deriving.core_type_of_type_decl type_decl in
      let polymorphize = Ppx_deriving.poly_arrow_of_type_decl decoder type_decl in
      polymorphize & decoder_exn typ

    (* type of t_of_ocaml, ex. 'a Ocaml_conv.decoder -> 'a t Ocaml_conv.decoder *)
    let sig_of_decoder type_decl =
      let typ = Ppx_deriving.core_type_of_type_decl type_decl in
      let polymorphize = Ppx_deriving.poly_arrow_of_type_decl decoder type_decl in
      polymorphize & decoder typ


    let is_base_type = function
      | [%type: int       ]
      | [%type: int32     ] | [%type: Int32.t]
      | [%type: int64     ] | [%type: Int64.t]
      | [%type: nativeint ] | [%type: Nativeint.t]
      | [%type: float     ] 
      | [%type: bool      ]  
      | [%type: char      ]  
      | [%type: string    ]
      | [%type: bytes     ] 
      | [%type: [%t? _] ref]
      | [%type: [%t? _] list]
      | [%type: [%t? _] array]
      | [%type: [%t? _] option]
      | { ptyp_desc = Ptyp_arrow (_, _, _) } -> true
      | _ -> false

    let rec expr_of_typ_exn typ = 
      Ast_405.Ast_helper.with_default_loc typ.ptyp_loc & fun () ->
      match typ with
      | _ when is_base_type typ -> exn & expr_of_typ typ
      | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
          let args_pp = List.map (fun typ -> [%expr [%e expr_of_typ typ]]) args in
          exn & app (Exp.lident (mangle_lid lid)) args_pp
      | { ptyp_desc = Ptyp_tuple typs; ptyp_loc } ->
          [%expr fun ?trace:(__t = []) __v ->
            [%e expr_of_tuple_exn ptyp_loc typs ]
          ]
      | { ptyp_desc = Ptyp_variant (fields, _, _); (* ptyp_loc *) } ->
          [%expr fun ?trace:(__t = []) __v ->
            [%e expr_of_poly_variant_exn typ fields ]
          ]
      | { ptyp_desc = Ptyp_object (_fields, Open); ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for open object type %s"
            deriver (string_of_core_type typ)
          
      | { ptyp_desc = Ptyp_object (labels, Closed) } -> 
          [%expr fun ?trace:(__t = []) __v ->
            [%e expr_of_object_exn typ labels ]
          ]
      | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ_exn typ
      | { ptyp_desc = Ptyp_var name } -> exn & evar & poly_var name
      | { ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
            deriver (string_of_core_type typ)

    and expr_of_typ typ = 
      Ast_405.Ast_helper.with_default_loc typ.ptyp_loc & fun () ->
      match typ with
      | [%type: int       ]                        -> of_ "int"
      | [%type: int32     ] | [%type: Int32.t]     -> of_ "int32"
      | [%type: int64     ] | [%type: Int64.t]     -> of_ "int64"
      | [%type: nativeint ] | [%type: Nativeint.t] -> of_ "nativeint"
      | [%type: float     ]                        -> of_ "float"
      | [%type: bool      ]                        -> of_ "bool"
      | [%type: char      ]                        -> of_ "char"
      | [%type: string    ]                        -> of_ "string"
      | [%type: bytes     ]                        -> of_ "bytes"
      | [%type: [%t? typ] ref]   ->
          [%expr [%e of_ "ref"] [%e expr_of_typ typ] ]
      | [%type: [%t? typ] list]  ->
          [%expr [%e of_ "list"] [%e expr_of_typ typ] ]
      | [%type: [%t? typ] array] ->
          [%expr [%e of_ "array"] [%e expr_of_typ typ] ]
      | [%type: [%t? typ] option] ->
          [%expr [%e of_ "option"] [%e expr_of_typ typ] ]
      | { ptyp_desc = Ptyp_arrow (_l, t1, t2) } ->
          [%expr [%e of_ "arrow"] [%e expr_of_typ t1] [%e expr_of_typ t2]]
      | { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
          let args_pp = List.map (fun typ -> [%expr [%e expr_of_typ typ]]) args in
          app (Exp.lident (mangle_lid lid)) args_pp
      | { ptyp_desc = Ptyp_tuple typs; ptyp_loc } ->
          [%expr fun ?trace:(__t = []) __v ->
            [%e econv "catch"]
              (fun () -> [%e expr_of_tuple_exn ptyp_loc typs ])
              ()
          ]
      | { ptyp_desc = Ptyp_variant (fields, _, _); (* ptyp_loc *) } ->
          [%expr fun ?trace:(__t = []) __v ->
            [%e econv "catch"]
              (fun () -> [%e expr_of_poly_variant_exn typ fields ])
              ()
          ]

      | { ptyp_desc = Ptyp_object (_fields, Open); ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for open object type %s"
            deriver (string_of_core_type typ)
          
      | { ptyp_desc = Ptyp_object (labels, Closed) } ->
          [%expr fun ?trace:(__t = []) __v ->
            [%e econv "catch"]
              (fun () -> [%e expr_of_object_exn typ labels])
              ()
          ]
          
      | { ptyp_desc = Ptyp_alias (typ, _) } -> expr_of_typ typ
      | { ptyp_desc = Ptyp_var name } -> 
          [%expr [%e evar & poly_var name]]
      | { ptyp_loc } ->
          raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
            deriver (string_of_core_type typ)

    and expr_of_tuple_exn _ptyp_loc typs =
      (* match DeconstrDecoder.tuple_exn ~trace: __t __v with
         | [a1; ..; an] ->
             (xxx_of_ttt0_exn ~trace:(`Pos 0 :: __t) a0,
              ..
              xxx_of_tttn_exn ~trace:(`Pos n :: __t) an)
         | _ -> throw ...
      *)
      Exp.match_ 
        [%expr [%e deconstr "tuple"] ~trace: __t __v]
        [ Exp.case
            (plist (List.mapi (fun i _ -> pvar (argn i)) typs))
            (tuple (List.mapi (fun i typ ->
              [%expr
                  [%e exn & expr_of_typ typ] (* XXX expr_of_typ_exn typ ? *)
                  ~trace:(`Pos [%e int i] :: __t)
                  [%e evar (argn i)]
              ]) typs))
        ; Exp.case [%pat? __vs]
          [%expr 
              [%e econv "throw"]
                (`Wrong_arity ([%e int & List.length typs],
                               List.length __vs,
                               None),
                 __v,
                 __t)
          ]
        ]

    and expr_of_poly_variant_exn ({ptyp_loc} as typ) fields =
      let cases =
        fields |> List.map (fun field ->
          match field with
          | Rtag (label, attrs, true (*empty*), []) -> (* `L *)
              let label' = conv_as M.name label attrs in
              [ Exp.case 
                  [%pat?
                      ( [%p pstr label'],
                        [%p plist [] ] )
                  ]
                  (Exp.variant label None)
              ; Exp.case
                  [%pat? ( [%p pstr label'], args ) ]
                  [%expr 
                     let __t = `Field [%e str & "`" ^ label'] :: __t in
                     [%e econv "throw"] 
                       (`Wrong_arity (
                         [%e int 0],
                         List.length args,
                         None),
                        __v,
                        __t)
                  ]
              ]
          | Rtag (label, attrs, false, [typ]) -> (* `L of typ *)
              let label' = conv_as M.name label attrs in
              [ Exp.case 
                  [%pat?
                      ( [%p pstr label'],
                        [__v] )
                  ]
                  [%expr
                     let __t = `Field [%e str & "`" ^ label'] :: __t in
                     [%e Exp.variant label 
                       & Some [%expr [%e expr_of_typ_exn typ] ~trace:__t __v]]
                  ]
              ; Exp.case
                  [%pat? ( [%p pstr label'], args ) ]
                  [%expr 
                     let __t = `Field [%e str & "`" ^ label'] :: __t in
                     [%e econv "throw"] 
                       (`Wrong_arity (
                         [%e int 1],
                         List.length args,
                         None),
                        __v,
                        __t)
                  ]
              ]
          | Rinherit ({ ptyp_desc = Ptyp_constr (_tname, []) } as _typ') -> (* [ tname ] *)
              (*Exp.case [%pat? [%p Pat.type_ tname] as x]
                [%expr [%e expr_of_typ typ] x]
              *)
              raise_errorf ~loc:ptyp_loc "Rinherit decoding is not supported yet"
                deriver (string_of_core_type typ)
          | _ ->
              raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s"
                deriver (string_of_core_type typ))
      in
      Exp.match_
        [%expr [%e deconstr "poly_variant"] "<poly_var>" ~trace: __t __v]
        &
        (List.concat cases 
         @ [ Exp.case [%pat? (name,_)]
              [%expr 
                  [%e econv "throw"] 
                   (* CR jfuruse: TODO *)
                  (`Unknown_tag ([%e str "<poly_var>"], name),
                   __v,
                   __t)
              ]
          ])

    and expr_of_object_exn ?type_name ?attrs typ labels =
      let type_name = match type_name with
        | None -> string_of_core_type typ
        | Some s -> s
      in
      let make_fields fields =
        Exp.object_ 
        & Cstr.mk (Pat.any ())
        & List.map (fun (n, e) -> Cf.method_concrete (at n) e) fields
      in
      expr_of_fields_exn
        ~type_name
        ~make_fields
        (* We have [?attrs] for [type t [&conv.ignore_unknown_fields] ] *)
        ~attrs: (match attrs with
                 | None -> typ.ptyp_attributes
                 | Some attrs -> attrs @ typ.ptyp_attributes)
        labels

    (* have [__t] and [__v] as free vars *)        
    and expr_of_fields_exn 
        ~type_name
        ~make_fields
        ~attrs  labels =
      let leftover_field = 
        match List.find_all (fun (_, _, ty) -> is_for_leftovers ty) labels with
        | [x] -> Some x
        | [] -> None
        | _ -> 
            (* CR jfuruse: must use ptyp_loc *)
            failwithf "Multiple mc_leftovers for <object> is not allowed" 
      in
      let ignore_unknown_fields =
        conv_ignore_unknown_fields M.name attrs
        || leftover_field <> None
      in
      let embeded_fields = List.filter_map is_for_embeded labels in
      let extract ({txt=name}, attrs, typ) k =
        let name' = conv_as M.name name attrs in
        match is_mc_option typ with
        | None ->
            [%expr
             let [%p pvar name] = 
               Meta_conv.Internal.field_assoc_exn 
                 [%e str type_name] 
                 [%e str name'] 
                 primary_fields
                 [%e econv "throw"]
                 [%e expr_of_typ_exn typ]
                 ~trace: __t __v
             in
             [%e k]
            ]
        | Some ty ->
            [%expr
             let [%p pvar name] = 
               Meta_conv.Internal.field_assoc_optional_exn 
                 [%e str type_name] 
                 [%e str name'] 
                 primary_fields
                 [%e expr_of_typ_exn ty]
                 ~trace: __t __v
             in
             [%e k]
            ]
      in
      let primary_labels =
        List.filter_map (fun ((name, _, _) as l) -> 
          if Some l <> leftover_field 
             && not (List.exists (fun (l', _, _) -> name = l') embeded_fields) 
          then Some l
          else None) labels
      in
      let secondary_field_empty_check =
        if ignore_unknown_fields then [%expr ()]
        else
          [%expr
              if __secondary_fields <> [] then
                [%e econv "throw"]
                  (`Unknown_fields
                      ([%e str type_name],
                       (List.map fst __secondary_fields),
                       Obj.repr res),
                   __v,
                   __t)
          ]
      in
      (* method l = l *)
      let primaries = 
        List.map (fun (name, _, _) -> (name.txt, evar name.txt)) primary_labels
      in

      let scrape_embeded ({txt=l}, k, typ) code =
        let return_v = match k with
          | `Embeded ->        [%expr v]
          | `Option_embeded -> [%expr Some v]
        in
        let recovery_e = match k with
          | `Embeded ->
              (* no error recovery *)
              [%expr [%e econv "throw"] __e]
          | `Option_embeded -> [%expr __secondary_fields, None]
        in
        [%expr
         let __secondary_fields, [%p pvar l] = (* [%e pvar l] went into inf loop *)
           let trace = `Field [%e str l] :: __t in
           match [%e expr_of_typ typ] ~trace __v with
           | Ok v -> [], [%e return_v]
           | Error (`Unknown_fields (_, unk_fields, obj), __v', trace') when __v == __v' && trace == trace' ->
               List.filter (fun (x,_) -> List.mem x unk_fields) __secondary_fields,
             [%e 
                 match k with
                 | `Embeded -> [%expr (Obj.obj obj : [%t typ]) ]
                 | `Option_embeded -> [%expr Some (Obj.obj obj : [%t typ]) ]
             ]
           | Error __e -> [%e recovery_e]
         in
         [%e code]
        ]
      in

      (* method l = l *)
      let embededs = List.map (function
        | ({txt=l}, _, _) -> (l, evar l)) embeded_fields
      in

      (* method name = __secondary_fields *)
      let left_over = match leftover_field with
        | None -> []
        | Some ({txt=name}, _, _) -> 
            [ (name, evar "__secondary_fields") ]
      in

      [%expr (* This should have the following outside: fun ?trace:(__t = []) __v ->  *)
        let primary_labels = 
          [%e list & List.map (fun ({txt=name}, attrs, _ty) ->
            str & conv_as M.name name attrs) primary_labels
          ]
        in
        let __fields = [%e deconstr "record"] [%e str type_name ] ~trace:__t __v in
        let (primary_fields, __secondary_fields) =
          Meta_conv.Internal.filter_fields primary_labels __fields
        in
        let res, __secondary_fields = 
          [%e 
           List.fold_right scrape_embeded embeded_fields
             (List.fold_right extract primary_labels 
                [%expr 
                    [%e make_fields (primaries @ left_over @ embededs)],
                    [%e evar "__secondary_fields"]])
          ]
        in
        [%e secondary_field_empty_check];
        res
      ]

    let sig_of_type ~path:_ type_decl =
      Ast_405.Ast_helper.with_default_loc type_decl.ptype_loc & fun () ->
      let type_decl_current = Convert_to_current.copy_type_declaration type_decl in
      [ Sig.value & Val.mk
          (at & mangle_type_decl_exn type_decl_current)
          (Convert_from_current.copy_core_type & sig_of_decoder_exn type_decl_current)

      ; Sig.value & Val.mk
          (at & mangle_type_decl type_decl_current)
          (Convert_from_current.copy_core_type & sig_of_decoder type_decl_current)
      ]

    let str_of_type ~path ({ ptype_loc = loc; ptype_attributes } as type_decl) =
      Ast_405.Ast_helper.with_default_loc loc & fun () ->
      let type_name = String.concat "." (path @ [type_decl.ptype_name.txt]) in
      let is_gadt = match type_decl.ptype_kind with
        | Ptype_variant constrs ->
            List.exists (fun c -> c.pcd_res <> None) constrs
        | _ -> false
      in
      let of_exn = match type_decl.ptype_kind, type_decl.ptype_manifest with
        | Ptype_abstract, Some ({ ptyp_desc = Ptyp_object (labels, Closed) } as typ) ->
            [%expr fun ?trace:(__t = []) __v ->
              [%e expr_of_object_exn ~type_name typ labels]
            ]
        | Ptype_abstract, Some manifest ->
            [%expr
                fun ?trace:(__t = []) __v ->
                  [%e expr_of_typ_exn manifest] ~trace:__t __v
            ]
        | Ptype_variant constrs, _ ->
            let cases =
              constrs |> List.map begin fun { pcd_name = { txt = constr_name }; pcd_args; pcd_attributes = attrs } ->
                let constr_name' = conv_as M.name constr_name attrs in
                match pcd_args with
                | Pcstr_tuple [cty] ->
                    (* | (constr_name, args) ->
                           let arg = 
                             match args with
                             | [arg] -> arg
                             | _ -> XXX_conv.Constr.tuple args
                           in
                           let __t = `Field "constr_name" :: __t in
                           Constr_name ( expr_of_typ_exn cty ~trace:(`Pos 0 :: __t) arg) 
                    *)
                    [ Exp.case
                        (* (<constr_name'>, args) -> *)
                        [%pat?
                            ( [%p pstr constr_name'], args )
                        ]
                        [%expr
                           let arg = match args with [arg] -> arg | _ -> [%e Encoder.constr "tuple"] args in
                           let __t = `Field [%e str constr_name'] :: __t in
                           [%e
                               constr constr_name
                                 [ [%expr
                                     [%e expr_of_typ_exn cty]
                                       ~trace:(`Pos [%e int 0] :: __t)
                                       arg
                                   ] ]
                           ]
                        ]
                    ]
                | Pcstr_tuple ctys ->
                    (* | (constr_name, [a0; ..; an]) ->
                           let __t = `Field "constr_name" :: __t in
                           Constr_name ( expr_of_typ_exn tyi ~trace:(`Pos i :: __t) ai, .. ) 
                       | (constr_name, args) -> ERROR
                    *)
                    [ Exp.case
                        (* (<constr_name'>, [a0; ..; an]) -> *)
                        [%pat?
                            ( [%p pstr constr_name'],
                              [%p plist (List.mapi (fun i _ -> pvar (argn i)) ctys)]
                            )
                        ]
                        [%expr
                           let __t = `Field [%e str constr_name'] :: __t in
                           [%e
                               (* <constr_name>(a0, ..., an) *)
                               constr constr_name
                                 ( List.mapi (fun i typ ->
                                     [%expr
                                         [%e expr_of_typ_exn typ]
                                         ~trace:(`Pos [%e int i] :: __t)
                                         [%e evar (argn i) ]
                                     ] ) ctys )
                           ]
                        ]
                    ; Exp.case
                        [%pat? ( [%p pstr constr_name'], args)]
                        [%expr [%e econv "throw"] 
                            (`Wrong_arity (
                                [%e int (List.length ctys)],
                                List.length args,
                                Some ([%e str type_name], [%e str constr_name])
                             ),
                             __v,
                             __t)
                        ]
                    ]
                | Pcstr_record labels ->
                    let constr_record fields = constr constr_name [ record fields ] in
                    (* <constr_name'> __v -> ... *)
                    [ Exp.case
                        [%pat? ( [%p pstr constr_name'], [ [%p pvar "__v" ] ] ) ]
                        [%expr
                           let __t = `Field [%e str constr_name'] :: __t in
                           [%e expr_of_fields_exn
                               ~type_name
                               ~make_fields: constr_record
                               ~attrs: ptype_attributes
                             & List.map (fun pld -> (pld.pld_name,
                                                      pld.pld_attributes,
                                                      pld.pld_type)) labels
                           ]
                        ]
                    ; Exp.case
                        [%pat? ( [%p pstr constr_name'], args)]
                        [%expr [%e econv "throw"] 
                            (`Wrong_arity (1,
                                List.length args,
                                Some ([%e str type_name], [%e str constr_name])
                             ),
                             __v,
                             __t)
                        ]
                    ]
              end
            in
            [%expr fun ?trace:(__t = []) __v ->
              let __t = `Node __v :: __t in
              [%e Exp.match_
                  [%expr [%e deconstr "variant"]
                           [%e str type_name]
                           ~trace:__t
                           __v
                  ]
                  (List.concat cases
                   @ [ Exp.case
                         [%pat? (name, _)]
                         [%expr 
                           [%e econv "throw"] 
                             (`Unknown_tag ([%e str type_name], name),
                              __v,
                              __t)
                         ]
                     ])
              ]
            ]

        | Ptype_record labels, _ ->

            [%expr fun ?trace:(__t = []) __v ->
              [%e expr_of_fields_exn
                  ~type_name
                  ~make_fields: record
                  ~attrs: ptype_attributes
                & List.map (fun pld -> (pld.pld_name,
                                        pld.pld_attributes,
                                        pld.pld_type)) labels
              ]
            ]

      | Ptype_abstract, None ->
          raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
      | Ptype_open, _        ->
          raise_errorf ~loc "%s cannot be derived for open types" deriver
      in
      let type_decl_current = Convert_to_current.copy_type_declaration type_decl in
      let polymorphize e = Convert_from_current.copy_expression & Ppx_deriving.poly_fun_of_type_decl type_decl_current & Convert_to_current.copy_expression e in
      if is_gadt then 
        let ty_exn = sig_of_decoder_exn type_decl_current in
        let ty = sig_of_decoder type_decl_current in
        [ vb_newtypes (mangle_type_decl_exn type_decl_current)
            (Convert_from_current.copy_core_type ty_exn)
            (polymorphize of_exn)
        ; vb_newtypes (mangle_type_decl type_decl_current)
            (Convert_from_current.copy_core_type ty)
            (polymorphize 
               (let args_pp = List.map (fun (typ,_) -> [%expr [%e expr_of_typ typ]]) type_decl.ptype_params in
                [%expr
                    fun ?trace:__t __v ->
                      [%e econv "result"]
                        [%e 
                         let lid = Longident.Lident type_decl.ptype_name.txt in
                         app (Exp.lident (mangle_lid_exn lid)) args_pp ]
                        ?trace:__t __v
                ]))
        ]
      else
        [ Vb.mk
          (* We need here type constraint since ocaml cannot
             infer the option label of poly_a *)
          (Pat.constraint_
             (pvar (mangle_type_decl_exn type_decl_current))
             (Convert_from_current.copy_core_type & sig_of_decoder_exn type_decl_current))
          (polymorphize of_exn)
        ; Vb.mk
          (pvar (mangle_type_decl type_decl_current))
          (polymorphize
             (let args_pp = List.map (fun (typ,_) -> [%expr [%e expr_of_typ typ]]) type_decl.ptype_params in
             [%expr
                 fun ?trace:__t __v ->
                 [%e econv "result"]
                 [%e 
                  let lid = Longident.Lident type_decl.ptype_name.txt in
                  app (Exp.lident (mangle_lid_exn lid)) args_pp ]
                   ?trace:__t __v
             ]))
      ]

  end
end

let parse_option s = 
  match String.is_prefix' "of_" s, String.is_postfix' "_of" s with
  | None   , None   -> s, `Both
  | Some s , None   -> s, `Decoder
  | None   , Some s -> s, `Encoder
  | _ -> failwith "You really want to have of_xxx_of thing???"

(** Get the labels from [{ x; y }] *)  
let parse_options deriver = List.map (fun (name, expr) ->
    match expr with
    | { pexp_desc = Pexp_ident {txt = Lident n} } when n = name -> name
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let big_one () =

  let deriver = "conv" in

  let structure ~options ~path type_decls =
    let open List in
    let options = map (fun (s,e) -> (s,Convert_from_current.copy_expression e)) options in
    let type_decls = map Convert_from_current.copy_type_declaration type_decls in
    let targets = parse_options deriver options in
    Convert_to_current.copy_structure
    & concat & flip map targets (fun s ->
      let name, which = parse_option s in
      let module M = Make(struct
        let deriver = deriver
        let name = name
        let type_ = String.capitalize_ascii name ^ ".t"
        let conv = String.capitalize_ascii name ^ "_conv"
      end) in
      let has_gadt = exists Ppxx.Compilerlib.XParsetree.is_gadt type_decls in
      let enc () = 
        str_value_maybe_rec 
        & concat_map (M.Encoder.str_of_type ~is_gadt:has_gadt ~path) type_decls
      in
      let dec () =
        str_value_maybe_rec 
        & concat_map (M.Decoder.str_of_type ~path) type_decls
      in
      match which with
      | `Both -> enc () @ dec ()
      | `Decoder -> dec ()
      | `Encoder -> enc ())
  in

  let signature ~options ~path type_decls =
    let open List in
    let options = map (fun (s,e) -> (s,Convert_from_current.copy_expression e)) options in
    let type_decls = map Convert_from_current.copy_type_declaration type_decls in
    let targets = parse_options deriver options in
    Convert_to_current.copy_signature
    & concat & concat & concat & flip map targets (fun s ->
      let name, which = parse_option s in
      let module M = Make(struct
        let deriver = deriver
        let name = name
        let type_ = String.capitalize_ascii name ^ ".t"
        let conv = String.capitalize_ascii name ^ "_conv"
      end) in
      map (fun td ->
        let enc () = M.Encoder.sig_of_type ~path td in
        let dec () = M.Decoder.sig_of_type ~path td in
        match which with
        | `Both -> [enc (); dec ()]
        | `Decoder -> [dec ()]
        | `Encoder -> [enc ()]) type_decls)
  in

  let open Ppx_deriving in
  register & create deriver
    ~type_decl_str: structure
    ~type_decl_sig: signature
    ()

let splitted name =
(*  let open List in *)
  let module M = Make(struct
    let deriver = "ppx_meta_conv"
    let name = name
    let type_ = String.capitalize_ascii name ^ ".t"
    let conv = String.capitalize_ascii name ^ "_conv"
  end)
  in
  
  let open Ppx_deriving in
  let f g = Convert_to_current.copy_expression *< g *< Convert_from_current.copy_core_type in
  (* [%derive.xxx_of: ty] *)
  register & create (name ^ "_of")
    ~core_type: (f & M.Encoder.expr_of_typ [])
    ();
    
  (* [%derive.of_xxx: ty] *)
  register & create ("of_" ^ name)
    ~core_type: (f M.Decoder.expr_of_typ) ();
    
  (* [%derive.of_xxx_exn: ty] *)
  register & create ("of_" ^ name ^ "_exn")
    ~core_type: (f M.Decoder.expr_of_typ_exn) ()

