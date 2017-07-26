(* Pa_type_conv: Preprocessing Module for Registering Type Conversions *)

open Ppx_core
open Ast_builder.Default

module Spellcheck   = Ppx_core.Spellcheck
module Ppx_deriving = Ppx_deriving_backend

let keep_w32_impl = ref false
let keep_w32_intf = ref false
let () =
  Ppx_driver.add_arg "-type-conv-keep-w32"
    (Symbol
       (["impl"; "intf"; "both"],
        (function
          | "impl" -> keep_w32_impl := true
          | "intf" -> keep_w32_intf := true
          | "both" ->
            keep_w32_impl := true;
            keep_w32_intf := true
          | _ -> assert false)))
    ~doc:" Do not try to disable warning 32 for the generated code"

let keep_w32_impl () = !keep_w32_impl || Ppx_driver.pretty ()
let keep_w32_intf () = !keep_w32_intf || Ppx_driver.pretty ()

module List = struct
  include List
  let concat_map xs ~f = concat (map xs ~f)

  let rec filter_map l ~f =
    match l with
    | [] -> []
    | x :: l ->
      match f x with
      | None   ->      filter_map l ~f
      | Some y -> y :: filter_map l ~f
end

module Args = struct
  include (Ast_pattern : module type of struct include Ast_pattern end
           with type ('a, 'b, 'c) t := ('a, 'b, 'c) Ast_pattern.t)

  type 'a param =
    { name    : string
    ; pattern : (expression, 'a) Ast_pattern.Packed.t
    ; default : 'a
    }

  let arg name pattern =
    { name
    ; default = None
    ; pattern = Ast_pattern.Packed.create pattern (fun x -> Some x)
    }
  ;;

  let flag name =
    let pattern = pexp_ident (lident (string name)) in
    { name
    ; default = false
    ; pattern = Ast_pattern.Packed.create pattern true
    }
  ;;

  type (_, _) t =
    | Nil  : ('m, 'm) t
    | Cons : ('m1, 'a -> 'm2) t * 'a param -> ('m1, 'm2) t

  let empty = Nil
  let ( +> ) a b = Cons (a, b)

  let rec names : type a b. (a, b) t -> string list = function
    | Nil -> []
    | Cons (t, p) -> p.name :: names t
  ;;

  module Instance = struct
    type (_, _) instance =
      | I_nil  : ('m, 'm) instance
      | I_cons : ('m1, 'a -> 'm2) instance * 'a -> ('m1, 'm2) instance

    let rec create
      : type a b. (a, b) t -> (string * expression) list -> (a, b) instance
      = fun spec args ->
        match spec with
        | Nil -> I_nil
        | Cons (t, p) ->
          let value =
            match List.Assoc.find args ~equal:String.equal p.name with
            | None -> p.default
            | Some expr -> Ast_pattern.Packed.parse p.pattern expr.pexp_loc expr
          in
          I_cons (create t args, value)
    ;;

    let rec apply : type a b. (a, b) instance -> a -> b = fun t f ->
      match t with
      | I_nil -> f
      | I_cons (t, x) -> apply t f x
    ;;
  end

  let apply t args f = Instance.apply (Instance.create t args) f
end

(* +-----------------------------------------------------------------+
   | Generators                                                      |
   +-----------------------------------------------------------------+ *)

module Generator = struct
  type ('a, 'b) t =
    | T :
        { spec           : ('c, 'a) Args.t
        ; gen            : loc:Location.t -> path:string -> 'b -> 'c
        ; arg_names      : Set.M(String).t
        ; attributes     : Attribute.packed list
        } -> ('a, 'b) t
    (* Generator imported from ppx deriving. Arguments are passed to the callback as
       it. *)
    | Imported_from_ppx_deriving of
        { gen : loc:Location.t -> path:string -> args:(string * expression) list
            -> 'b -> 'a }

  let make ?(attributes=[]) spec gen =
    let arg_names = Set.of_list (module String) (Args.names spec) in
    T { spec
      ; gen
      ; arg_names
      ; attributes
      }
  ;;

  let make_noarg ?attributes gen = make ?attributes Args.empty gen

  let merge_accepted_args l =
    let rec loop acc = function
      | [] -> Some acc
      | T t :: rest -> loop (Set.union acc t.arg_names) rest
      | Imported_from_ppx_deriving _ :: _ -> None
    in
    loop (Set.empty (module String)) l

  let check_arguments name generators (args : (string * expression) list) =
    List.iter args ~f:(fun (label, e) ->
      if String.is_empty label then
        Location.raise_errorf ~loc:e.pexp_loc
          "ppx_type_conv: generator arguments must be labelled");
    Option.iter (List.find_a_dup args ~compare:(fun (a, _) (b, _) -> String.compare a b))
      ~f:(fun (label, e) ->
        Location.raise_errorf ~loc:e.pexp_loc
          "ppx_type_conv: argument labelled '%s' appears more than once" label);
    match merge_accepted_args generators with
    | None -> ()
    | Some accepted_args ->
      List.iter args ~f:(fun (label, e) ->
        if not (Set.mem accepted_args label) then
          let spellcheck_msg =
            match Spellcheck.spellcheck (Set.to_list accepted_args) label with
            | None -> ""
            | Some s -> ".\n" ^ s
          in
          Location.raise_errorf ~loc:e.pexp_loc
            "ppx_type_conv: generator '%s' doesn't accept argument '%s'%s"
            name label spellcheck_msg);
  ;;

  let apply t ~name:_ ~loc ~path x args =
    match t with
    | T { spec; gen; _ } -> Args.apply spec args (gen ~loc ~path x)
    | Imported_from_ppx_deriving { gen } -> gen ~loc ~path ~args x
  ;;

  let apply_all ?(rev=false) ~loc ~path entry (name, generators, args) =
    check_arguments name.txt generators args;
    let results =
      if rev
      then
        (* Map actual_generators right->left so side effects (gensym) match camlp4 *)
        List.rev generators
        |> List.map ~f:(fun t -> apply t ~name:name.txt ~loc ~path entry args)
        |> List.rev
      else
        generators
        |> List.map ~f:(fun t -> apply t ~name:name.txt ~loc ~path entry args)
    in
    List.concat results
  ;;

  let apply_all ?rev ~loc ~path entry generators =
    let generators = List.rev generators in
    List.concat_map generators ~f:(apply_all ?rev ~loc ~path entry)
  ;;
end

type t = string
let ignore (_ : t) = ()

module Deriver = struct
  module Actual_deriver = struct
    type t =
      { name          : string
      ; str_type_decl : (structure, rec_flag * type_declaration list) Generator.t option
      ; str_type_ext  : (structure, type_extension                  ) Generator.t option
      ; str_exception : (structure, extension_constructor           ) Generator.t option
      ; sig_type_decl : (signature, rec_flag * type_declaration list) Generator.t option
      ; sig_type_ext  : (signature, type_extension                  ) Generator.t option
      ; sig_exception : (signature, extension_constructor           ) Generator.t option
      ; extension     : (loc:Location.t -> path:string -> core_type -> expression) option
      }
  end

  module Alias = struct
    type t =
      { str_type_decl : string list
      ; str_type_ext  : string list
      ; str_exception : string list
      ; sig_type_decl : string list
      ; sig_type_ext  : string list
      ; sig_exception : string list
      }
  end

  module Field = struct
    type kind = Str | Sig

    type ('a, 'b) t =
      { name    : string
      ; kind    : kind
      ; get     : Actual_deriver.t -> ('a, 'b) Generator.t option
      ; get_set : Alias.t -> string list
      }

    let str_type_decl = { kind = Str; name = "type"
                        ; get     = (fun t -> t.str_type_decl)
                        ; get_set = (fun t -> t.str_type_decl) }
    let str_type_ext  = { kind = Str; name = "type extension"
                        ; get     = (fun t -> t.str_type_ext)
                        ; get_set = (fun t -> t.str_type_ext ) }
    let str_exception = { kind = Str; name = "exception"
                        ; get     = (fun t -> t.str_exception)
                        ; get_set = (fun t -> t.str_exception) }
    let sig_type_decl = { kind = Sig; name = "signature type"
                        ; get     = (fun t -> t.sig_type_decl)
                        ; get_set = (fun t -> t.sig_type_decl) }
    let sig_type_ext  = { kind = Sig; name = "signature type extension"
                        ; get     = (fun t -> t.sig_type_ext)
                        ; get_set = (fun t -> t.sig_type_ext ) }
    let sig_exception = { kind = Sig; name = "signature exception"
                        ; get     = (fun t -> t.sig_exception)
                        ; get_set = (fun t -> t.sig_exception) }
  end

  type t =
    | Actual_deriver of Actual_deriver.t
    | Alias of Alias.t

  let all : (string, t) Hashtbl.t = Hashtbl.create (module String) () ~size:42

  exception Not_supported of string

  let resolve_internal (field : (_, _) Field.t) name =
    let rec loop name collected =
      if List.exists collected ~f:(fun (d : Actual_deriver.t) -> String.equal d.name name) then
        collected
      else
        match Hashtbl.find all name with
        | Some (Actual_deriver drv) -> drv :: collected
        | Some (Alias alias) ->
          let set = field.get_set alias in
          List.fold_right set ~init:collected ~f:loop
        | None -> raise (Not_supported name)
    in
    let actual_derivers_rev = loop name [] in
    List.rev_map actual_derivers_rev ~f:(fun drv ->
      match field.get drv with
      | None -> raise (Not_supported name)
      | Some g -> g)
  ;;

  let supported_for field =
    Hashtbl.fold all ~init:(Set.empty (module String))
      ~f:(fun ~key ~data:_ acc ->
        match resolve_internal field key with
        | _ -> Set.add acc key
        | exception Not_supported _ -> acc)
    |> Set.to_list
  ;;

  let not_supported (field : (_, _) Field.t) ?(spellcheck=true) name =
    let spellcheck_msg =
      if spellcheck then
        match Spellcheck.spellcheck (supported_for field) name.txt with
        | None -> ""
        | Some s -> ".\n" ^ s
      else
        ""
    in
    Location.raise_errorf ~loc:name.loc
      "ppx_type_conv: '%s' is not a supported %s type-conv generator%s"
      name.txt field.name spellcheck_msg
  ;;

  let resolve field name =
    try
      resolve_internal field name.txt
    with Not_supported name' ->
      not_supported field ~spellcheck:(String.equal name.txt name') name
  ;;

  let resolve_all field derivers =
    List.map derivers ~f:(fun (name, args) ->
      (name, resolve field name, args))
  ;;

  let export name =
    let resolve_opt field =
      match resolve_internal field name with
      | generators                  -> Some generators
      | exception (Not_supported _) -> None
    in
    let str_type_decl = resolve_opt Field.str_type_decl in
    let str_type_ext  = resolve_opt Field.str_type_ext  in
    let sig_type_decl = resolve_opt Field.sig_type_decl in
    let sig_type_ext  = resolve_opt Field.sig_type_ext  in
    (* Ppx deriving works on the compiler AST while type conv work on the version
       selected by Ppx_ast, so we need to convert. *)
    let module Js = Ppx_ast.Selected_ast in
    let core_type =
      match Hashtbl.find_exn all name with
      | Alias _ -> None
      | Actual_deriver drv ->
        Option.map drv.extension ~f:(fun f ->
          fun ty ->
            Js.of_ocaml Core_type ty
            |> f ~loc:ty.ptyp_loc ~path:""
            |> Js.to_ocaml Expression
        )
    in
    let convert_args args =
      List.map args ~f:(fun (name, expr) ->
        (name, Js.of_ocaml Expression expr))
    in
    let import_path l = String.concat l ~sep:"." in
    let make_type_decl gens result_kind =
      Option.map gens ~f:(fun gens ->
        fun ~options ~path tds ->
          let path = import_path path in
          let options = convert_args options in
          Generator.check_arguments name gens options;
          let tds = Js.of_ocaml (List Type_declaration) tds in
          List.concat_map gens ~f:(fun gen ->
            Generator.apply gen ~name ~loc:Location.none ~path (Recursive, tds) options)
          |> Js.to_ocaml result_kind)
    in
    let make_type_ext gens result_kind =
      Option.map gens ~f:(fun gens ->
        fun ~options ~path te ->
          let path = import_path path in
          let options = convert_args options in
          Generator.check_arguments name gens options;
          let te = Js.of_ocaml Type_extension te in
          List.concat_map gens ~f:(fun gen ->
            Generator.apply gen ~name ~loc:Location.none ~path te options)
          |> Js.to_ocaml result_kind)
    in
    let type_decl_str = make_type_decl str_type_decl Structure in
    let type_decl_sig = make_type_decl sig_type_decl Signature in
    let type_ext_str  = make_type_ext  str_type_ext  Structure in
    let type_ext_sig  = make_type_ext  sig_type_ext  Signature in
    Ppx_deriving.register
      (Ppx_deriving.create name
         ?core_type
         ?type_ext_str
         ?type_ext_sig
         ?type_decl_str
         ?type_decl_sig
         ())
  ;;

  (* When connecting with ppx_deriving, both ppx_type_conv and ppx_deriving forwards new
     derivers to each other. This reference is used to break the loop. *)
  let disable_import = ref false

  let safe_add ?(connect_with_ppx_deriving=Ppx_deriving.real) id t =
    (match Hashtbl.add all ~key:id ~data:t with
     | `Ok -> ()
     | `Duplicate ->
       failwith ("ppx_type_conv: generator '" ^ id ^ "' defined multiple times"));
    if connect_with_ppx_deriving then begin
      let save = !disable_import in
      disable_import := true;
      try
        export id;
        disable_import := save
      with exn ->
        disable_import := save;
        raise exn
    end
  ;;

  let add
        ?connect_with_ppx_deriving
        ?str_type_decl
        ?str_type_ext
        ?str_exception
        ?sig_type_decl
        ?sig_type_ext
        ?sig_exception
        ?extension
        name
    =
    let actual_deriver : Actual_deriver.t =
      { name
      ; str_type_decl
      ; str_type_ext
      ; str_exception
      ; sig_type_decl
      ; sig_type_ext
      ; sig_exception
      ; extension
      }
    in
    safe_add name (Actual_deriver actual_deriver) ?connect_with_ppx_deriving;
    (match extension with
     | None -> ()
     | Some f ->
       let extension = Extension.declare name Expression Ast_pattern.(ptyp __) f in
       Ppx_driver.register_transformation ("ppx_type_conv." ^ name)
         ~rules:[ Context_free.Rule.extension extension ]);
    name
  ;;

  let split_path path = String.split path ~on:'.'

  module Ppx_deriving_import = struct
    include struct
      open Migrate_parsetree.OCaml_current.Ast
      open Parsetree

      type ('output_ast, 'input_ast) generator
        =  options:(string * expression) list
        -> path:string list
        -> 'input_ast
        -> 'output_ast

      type deriver =
        { name          : string
        ; core_type     : (core_type -> expression) option
        ; type_decl_str : (structure, type_declaration list) generator
        ; type_ext_str  : (structure, type_extension       ) generator
        ; type_decl_sig : (signature, type_declaration list) generator
        ; type_ext_sig  : (signature, type_extension       ) generator
        }
    end

    let import d =
      if !disable_import then () else begin
        (* Ppx deriving works on the compiler AST while type conv work on the version
           selected by Ppx_ast, so we need to convert. *)
        let module Js = Ppx_ast.Selected_ast in
        let convert_args args =
          List.map args ~f:(fun (name, expr) ->
            (name, Js.to_ocaml Expression expr))
        in
        let map_type_ext f result_kind =
          Generator.Imported_from_ppx_deriving
            { gen = fun ~loc:_ ~path ~args x ->
                Js.to_ocaml Type_extension x
                |> f ~options:(convert_args args) ~path:(split_path path)
                |> Js.of_ocaml result_kind
            }
        in
        let map_type_decl f result_kind =
          Generator.Imported_from_ppx_deriving
            { gen = fun ~loc:_ ~path ~args (_rf, x) ->
                Js.to_ocaml (List Type_declaration) x
                |> f ~options:(convert_args args) ~path:(split_path path)
                |> Js.of_ocaml result_kind
            }
        in
        let extension =
          match d.core_type with
          | None -> None
          | Some f ->
            Some (fun ~loc:_ ~path:_ ct ->
              Js.to_ocaml Core_type ct
              |> f
              |> Js.of_ocaml Expression)
        in
        ignore (
          add d.name ~connect_with_ppx_deriving:false
            ~str_type_decl: (map_type_decl d.type_decl_str Structure)
            ~str_type_ext:  (map_type_ext  d.type_ext_str  Structure)
            ~sig_type_decl: (map_type_decl d.type_decl_sig Signature)
            ~sig_type_ext:  (map_type_ext  d.type_ext_sig  Signature)
            ?extension
          : string
        )
      end
    ;;
  end

  let import (d : Ppx_deriving.deriver) =
    Ppx_deriving_import.import
      { name          = d.name
      ; core_type     = d.core_type
      ; type_decl_str = d.type_decl_str
      ; type_ext_str  = d.type_ext_str
      ; type_decl_sig = d.type_decl_sig
      ; type_ext_sig  = d.type_ext_sig
      }

  let () =
    if Ppx_deriving.real then begin
      Ppx_deriving.add_register_hook import;
      List.iter (Ppx_deriving.derivers ()) ~f:import
    end
  ;;

  let add_alias
        name
        ?str_type_decl
        ?str_type_ext
        ?str_exception
        ?sig_type_decl
        ?sig_type_ext
        ?sig_exception
        set
    =
    let alias : Alias.t =
      let get = function
        | None     -> set
        | Some set -> set
      in
      { str_type_decl = get str_type_decl
      ; str_type_ext  = get str_type_ext
      ; str_exception = get str_exception
      ; sig_type_decl = get sig_type_decl
      ; sig_type_ext  = get sig_type_ext
      ; sig_exception = get sig_exception
      }
    in
    safe_add name (Alias alias);
    name
  ;;
end

let add       = Deriver.add ?connect_with_ppx_deriving:None
let add_alias = Deriver.add_alias

module Ppx_deriving_import = Deriver.Ppx_deriving_import

(* +-----------------------------------------------------------------+
   | [@@deriving ] parsing                                           |
   +-----------------------------------------------------------------+ *)

let invalid_with ~loc = Location.raise_errorf ~loc "invalid [@@deriving ] attribute syntax"

let generator_name_of_id loc id =
  match Longident.flatten_exn id with
  | l -> { loc; txt = String.concat ~sep:"." l }
  | exception _ -> invalid_with ~loc:loc
;;

let mk_deriving_attr context ~suffix =
  Attribute.declare
    ("type_conv.deriving" ^ suffix)
    context
    Ast_pattern.(
      let label =
        map' __ ~f:(fun loc f label ->
          match label with
          | Nolabel | Optional _ ->
            Location.raise_errorf ~loc "non-optional labeled argument expected"
          | Labelled label ->
            f label)
      in
      let generator_name () =
        map' (pexp_ident __) ~f:(fun loc f id -> f (generator_name_of_id loc id))
      in
      let arg = pack2 (label ** __) in
      let generator () =
        map (generator_name ()) ~f:(fun f x -> f (x, [])) |||
        pack2 (pexp_apply (generator_name ()) (many arg))
      in
      let generators =
        pexp_tuple (many (generator ())) |||
        map (generator ()) ~f:(fun f x -> f [x])
      in
      pstr (pstr_eval generators nil ^:: nil)
    )
    (fun x -> x)
;;

module Attr = struct
  let suffix = ""
  let td = mk_deriving_attr ~suffix Type_declaration
  let te = mk_deriving_attr ~suffix Type_extension
  let ec = mk_deriving_attr ~suffix Extension_constructor

  module Expect = struct
    let suffix = "_inline"
    let td = mk_deriving_attr ~suffix Type_declaration
    let te = mk_deriving_attr ~suffix Type_extension
    let ec = mk_deriving_attr ~suffix Extension_constructor
  end
end

(* +-----------------------------------------------------------------+
   | Unused warning stuff                                            |
   +-----------------------------------------------------------------+ *)

(* [do_insert_unused_warning_attribute] -- If true, generated code contains compiler
   attribute to disable unused warnings, instead of inserting [let _ = ... ].
   We wont enable this yet, otherwise it will make it much harder to compare the code
   generated by ppx with that of the pa version *)
let do_insert_unused_warning_attribute = false

let disable_unused_warning_attribute ~loc =
  ({ txt = "ocaml.warning"; loc }, PStr [%str "-32"])
;;

let disable_unused_warning_str ~loc st =
  if keep_w32_impl () then
    st
  else if not do_insert_unused_warning_attribute then
    Ignore_unused_warning.add_dummy_user_for_values#structure st
  else
    [pstr_include ~loc
       (include_infos ~loc
          (pmod_structure ~loc
             (pstr_attribute ~loc (disable_unused_warning_attribute ~loc)
              :: st)))]
;;

let disable_unused_warning_sig ~loc sg =
  if keep_w32_intf () then
    sg
  else
    [psig_include ~loc
       (include_infos ~loc
          (pmty_signature ~loc
             (psig_attribute ~loc (disable_unused_warning_attribute ~loc)
              :: sg)))]
;;

(* +-----------------------------------------------------------------+
   | Remove attributes used by syntax extensions                     |
   +-----------------------------------------------------------------+ *)
(*
let remove generators =
  let attributes =
    List.concat_map generators ~f:(fun (_, actual_generators, _) ->
      List.concat_map actual_generators ~f:(fun (Generator.T g) -> g.attributes))
  in
  object
    inherit Ast_traverse.map

    (* Don't recurse through attributes and extensions *)
    method! attribute x = x
    method! extension x = x

    method! label_declaration ld =
      Attribute.remove_seen Attribute.Context.label_declaration attributes ld

    method! constructor_declaration cd =
      Attribute.remove_seen Attribute.Context.constructor_declaration attributes cd
  end
*)
(* +-----------------------------------------------------------------+
   | Main expansion                                                  |
   +-----------------------------------------------------------------+ *)

let types_used_by_type_conv (tds : type_declaration list)
  : structure_item list =
  if keep_w32_impl () then
    []
  else
    List.map tds ~f:(fun td ->
      let typ = core_type_of_type_declaration td in
      let loc = td.ptype_loc in
      [%stri let _ = fun (_ : [%t typ]) -> () ]
    )

let merge_generators field l =
  List.filter_map l ~f:(fun x -> x)
  |> List.concat
  |> Deriver.resolve_all field

let expand_str_type_decls ~loc ~path rec_flag tds values =
  let generators = merge_generators Deriver.Field.str_type_decl values in
  let generated =
    types_used_by_type_conv tds
    @ Generator.apply_all ~rev:true ~loc ~path (rec_flag, tds) generators;
  in
  disable_unused_warning_str ~loc generated

let expand_sig_type_decls ~loc ~path rec_flag tds values =
  let generators = merge_generators Deriver.Field.sig_type_decl values in
  let generated = Generator.apply_all ~loc ~path (rec_flag, tds) generators in
  disable_unused_warning_sig ~loc generated

let expand_str_exception ~loc ~path ec generators =
  let generators = Deriver.resolve_all Deriver.Field.str_exception generators in
  let generated = Generator.apply_all ~rev:true ~loc ~path ec generators in
  disable_unused_warning_str ~loc generated

let expand_sig_exception ~loc ~path ec generators =
  let generators = Deriver.resolve_all Deriver.Field.sig_exception generators in
  let generated = Generator.apply_all ~rev:true ~loc ~path ec generators in
  disable_unused_warning_sig ~loc generated

let expand_str_type_ext ~loc ~path te generators =
  let generators = Deriver.resolve_all Deriver.Field.str_type_ext generators in
  let generated = Generator.apply_all ~rev:true ~loc ~path te generators in
  disable_unused_warning_str ~loc generated

let expand_sig_type_ext ~loc ~path te generators =
  let generators = Deriver.resolve_all Deriver.Field.sig_type_ext generators in
  let generated = Generator.apply_all ~rev:true ~loc ~path te generators in
  disable_unused_warning_sig ~loc generated

let () =
  Ppx_driver.register_transformation "type_conv"
    ~rules:[ Context_free.Rule.attr_str_type_decl
               Attr.td
               expand_str_type_decls
           ; Context_free.Rule.attr_sig_type_decl
               Attr.td
               expand_sig_type_decls
           ; Context_free.Rule.attr_str_type_ext
               Attr.te
               expand_str_type_ext
           ; Context_free.Rule.attr_sig_type_ext
               Attr.te
               expand_sig_type_ext
           ; Context_free.Rule.attr_str_exception
               Attr.ec
               expand_str_exception
           ; Context_free.Rule.attr_sig_exception
               Attr.ec
               expand_sig_exception

           (* [@@deriving_inline] *)
           ; Context_free.Rule.attr_str_type_decl_expect
               Attr.Expect.td
               expand_str_type_decls
           ; Context_free.Rule.attr_sig_type_decl_expect
               Attr.Expect.td
               expand_sig_type_decls
           ; Context_free.Rule.attr_str_type_ext_expect
               Attr.Expect.te
               expand_str_type_ext
           ; Context_free.Rule.attr_sig_type_ext_expect
               Attr.Expect.te
               expand_sig_type_ext
           ; Context_free.Rule.attr_str_exception_expect
               Attr.Expect.ec
               expand_str_exception
           ; Context_free.Rule.attr_sig_exception_expect
               Attr.Expect.ec
               expand_sig_exception
           ]
;;
