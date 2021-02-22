module Counter = struct
  type t = int ref

  let make () =
    ref 0

  let count counter =
    let result = !counter in
    counter := succ result;
    result
end

let extension_of_index (i : int) : Ppxlib.extension =
  (Metapp_preutils.mkloc "meta", Metapp_preutils.payload_of_int i)

let deref (e : Ppxlib.expression) : Ppxlib.expression =
  Metapp_preutils.apply (Metapp_preutils.Exp.var "!") [e]

let array_get (a : Ppxlib.expression) (index : int) : Ppxlib.expression =
  let i = Metapp_preutils.Exp.of_int index in
  Metapp_preutils.apply
    (Metapp_preutils.Exp.ident (Ldot (Lident "Array", "get")))
    [a; i]

let array_set (a : Ppxlib.expression) (index : int)
    (v : Ppxlib.expression) : Ppxlib.expression =
  let i = Metapp_preutils.Exp.of_int index in
  Metapp_preutils.apply
    (Metapp_preutils.Exp.ident (Ldot (Lident "Array", "set")))
    [a; i; v]

let string_list_of_payload (payload : Ppxlib.payload) : string list =
  List.map Metapp_preutils.string_of_arbitrary_expression
    (Metapp_preutils.list_of_tuple (Metapp_preutils.Exp.of_payload payload))

module Options = struct
  include Dyncompile.Options

  let handle ((({ txt; _ }, payload), _) : Metapp_preutils.destruct_extension) :
      (t -> t) option =
    match txt with
    | "metaload" ->
        let add_object_file object_file =
          Dynlink.loadfile object_file;
          let dir_name = Filename.dirname object_file in
          if dir_name = Filename.current_dir_name then
            None
          else
            Some dir_name in
        Some (add_directories (List.filter_map add_object_file
          (string_list_of_payload payload)))
    | "metapackage" -> Some (add_packages (string_list_of_payload payload))
    | "metadir" -> Some (add_directories (string_list_of_payload payload))
    | "metaflag" -> Some (add_flags (string_list_of_payload payload))
    | "metaplainsource" -> Some (set_plainsource true)
    | "metadebug_findlib" -> Some (set_debug_findlib true)
    | "metaverbose" -> Some (set_verbose true)
    | _ -> None
end

type instruction =
  | Expression of Ppxlib.expression
  | Definition of Ppxlib.structure Location.loc

let get_expression (instruction : instruction)
    : Ppxlib.expression =
  match instruction with
  | Expression expression -> expression
  | Definition definition ->
      Location.raise_errorf ~loc:definition.loc
        "Definitions are only allowed at top-level"

module rec AccuTypes : sig
  type 'a escape = {
      instructions : instruction list;
      quotation : unit -> 'a Metapp_api.ArrayQuotation.quotation;
    }

  type 'a quotations = 'a escape Metapp_preutils.Accu.t ref

  type 'a metapoints = Location.t Metapp_preutils.Accu.t ref
end = struct
  include AccuTypes
end
and MutableQuotations : Metapp_api.QuotationsWithMakeS with
type 'a x = 'a AccuQuotation.t =
    Metapp_api.QuotationsWithMake (AccuQuotation)
and AccuQuotation : Metapp_api.UnaryMakeS
with type 'a t = 'a AccuTypes.quotations
    = struct
  type 'a t = 'a AccuTypes.quotations

  let make () =
    ref Metapp_preutils.Accu.empty
end
and MutableMetapoints : Metapp_api.MetapointsWithMakeS
with type 'a x = 'a AccuMetapoint.t =
    Metapp_api.MetapointsWithMake (AccuMetapoint)
and AccuMetapoint : Metapp_api.UnaryMakeS
with type 'a t = 'a AccuTypes.metapoints = struct
  type 'a t = 'a AccuTypes.metapoints

  let make () =
    ref Metapp_preutils.Accu.empty
end

module type MetapointsMapperS =
  functor (Metapoint : Metapp_api.MetapointS) -> sig
    val map : Ppxlib.payload -> Metapoint.t
  end

module Metapoint_mapper (Mapper : MetapointsMapperS) = struct
  module Mapper' (Metapoint : Metapp_api.MetapointS) = struct
    let map (super : Metapoint.t Metapp_preutils.map)
        (m : Metapoint.t) : Metapoint.t =
      Ppxlib.Ast_helper.with_default_loc (Metapoint.to_loc m) @@ fun () ->
        match Metapoint.destruct_extension m with
        | Some (({ txt = "meta"; _ }, payload), _) ->
            let module Map = Mapper (Metapoint) in
            Map.map payload
      | _ -> super m
  end

  class map = object
    inherit Ppxlib.Ast_traverse.map as super

    method! expression =
      let module M = Mapper' (Metapp_api.Exp) in M.map super#expression

    method! pattern =
      let module M = Mapper' (Metapp_api.Pat) in M.map super#pattern

    method! core_type =
      let module M = Mapper' (Metapp_api.Typ) in M.map super#core_type

    method! class_type =
      let module M = Mapper' (Metapp_api.Cty) in M.map super#class_type

    method! class_type_field =
      let module M = Mapper' (Metapp_api.Ctf) in M.map super#class_type_field

    method! class_expr =
      let module M = Mapper' (Metapp_api.Cl) in M.map super#class_expr

    method! class_field =
      let module M = Mapper' (Metapp_api.Cf) in M.map super#class_field

    method! module_type =
      let module M = Mapper' (Metapp_api.Mty) in M.map super#module_type

    method! module_expr =
      let module M = Mapper' (Metapp_api.Mod) in M.map super#module_expr

    method! signature_item =
      let module M = Mapper' (Metapp_api.Sigi) in M.map super#signature_item

    method! structure_item =
      let module M = Mapper' (Metapp_api.Stri) in M.map super#structure_item
  end
end

let unmut_metapoints (context : MutableMetapoints.t)
    : Metapp_api.OptionArrayMetapoints.t =
  let module Map = Metapp_api.MetapointMap (MutableMetapoints)
      (Metapp_api.OptionArrayMetapoints)
      (struct
        type 'a x = 'a AccuMetapoint.t

        type 'a y = 'a option array

        let map accu = Array.make (Metapp_preutils.Accu.length !accu) None
      end) in
  Map.map context

let unmut_loc (context : MutableMetapoints.t)
    : Metapp_api.MetapointsLocation.t =
  let module Map = Metapp_api.MetapointMap (MutableMetapoints)
      (Metapp_api.MetapointsLocation) (struct
        type 'a x = 'a AccuMetapoint.t

        type _ y = Location.t array

        let map accu = Metapp_preutils.Accu.to_array !accu
      end) in
  Map.map context

let unmut_subquotations (context : MutableQuotations.t)
    : Metapp_api.ArrayQuotations.t =
  let module Map = Metapp_api.QuotationMap (MutableQuotations)
      (Metapp_api.ArrayQuotations) (struct
        type 'a x = 'a AccuQuotation.t

        type 'a y = 'a Metapp_api.ArrayQuotation.t

        let map accu = Array.map
            (fun quotation -> quotation.AccuTypes.quotation)
            (Metapp_preutils.Accu.to_array !accu)
      end) in
  Map.map context

let context_var = "__context"

let fill_var = "__fill"

let metapoints_field = "metapoints"

let loc_field = "loc"

let subquotations_field = "subquotations"

let field_get (expr : Ppxlib.expression) (field : string)
    : Ppxlib.expression =
  Ppxlib.Ast_helper.Exp.field expr
    (Metapp_preutils.mkloc (Longident.Lident field))

let context_get (field : string) : Ppxlib.expression =
  field_get (Metapp_preutils.Exp.var context_var) field

let replace_metapoints (contents : Metapp_api.OptionArrayMetapoints.t)
    : Ppxlib.Ast_traverse.map =
  let module Mapper (Metapoint : Metapp_api.MetapointS) = struct
    module Accessor =
      Metapoint.MetapointAccessor (Metapp_api.OptionArrayMetapoints)

    let map (payload : Ppxlib.payload) : Metapoint.t =
      Option.get
        (Accessor.get contents).(Metapp_preutils.int_of_payload payload)
  end in
  let module Mapper' = Metapoint_mapper (Mapper) in
  new Mapper'.map

let metapp_api = Longident.Lident "Metapp_api"

module type Map = sig
  class map : Ppxlib.Ast_traverse.map
end

let rec extract_subquotations (quotations : MutableQuotations.t) :
    Ppxlib.Ast_traverse.map = object
  inherit Ppxlib.Ast_traverse.map as super

  method! expression (e : Ppxlib.expression) : Ppxlib.expression =
    Ppxlib.Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
    match
      match e.pexp_desc with
      | Pexp_extension ({ txt; _ }, payload) ->
          Option.map (fun antiquotable -> (antiquotable, payload))
          ((match txt with
          | "e" | "expr" -> Some (module Metapp_api.Exp)
          | "p" | "pat" -> Some (module Metapp_api.Pat)
          | "t" | "type" -> Some (module Metapp_api.Typ)
          | "sig" -> Some (module Metapp_api.Sig)
          | "sigi" -> Some (module Metapp_api.Sigi)
          | "str" -> Some (module Metapp_api.Str)
          | "stri" -> Some (module Metapp_api.Stri)
          | _ -> None
          ) : ((module Metapp_api.QuotationS) option))
      | _ -> None
    with
    | None -> super#expression e
    | Some (antiquotable, payload) ->
        let module M = (val antiquotable) in
        let module Quotation = M.QuotationAccessor (MutableQuotations) in
        let module Name = M.QuotationAccessor (Metapp_api.QuotationName) in
        let quotation = M.of_payload payload in
        let (map_module, k) = extract_metapoints () in
        let module Map = (val map_module : Map) in
        let map = new Map.map in
        let quotation = M.map map quotation in
        let escape : 'a AccuTypes.escape = k () in
        let quote () =
          let quotation' = escape.quotation () in
          let fill () =
            let map = replace_metapoints quotation'.context.metapoints in
            M.map map quotation in
          { quotation' with fill } in
        let index =
          Metapp_preutils.update
            (Metapp_preutils.Accu.add { escape with quotation = quote})
            (Quotation.get quotations) in
        let loc = !Ppxlib.Ast_helper.default_loc in
        let field_name = Name.get Metapp_api.quotation_name in
        [%expr let
            { Metapp_api.ArrayQuotation.context = __context; fill = __fill } =
          ([%e field_get (context_get subquotations_field) field_name]).(
          [%e Metapp_preutils.Exp.of_int index]) () in
          [%e (Metapp_preutils.sequence
            (List.map get_expression escape.instructions @
              [[%expr __fill ()]]))]]
end

and extract_metapoints () : (module Map) * (unit -> unit AccuTypes.escape) =
  let accu = ref [] in
  let metapoints = MutableMetapoints.make () in
  let subquotations = MutableQuotations.make () in
  let map_subquotations = extract_subquotations subquotations in
  let module Mapper (Metapoint : Metapp_api.MetapointS) = struct
    module Accessor = Metapoint.MetapointAccessor (MutableMetapoints)
    module Name = Metapoint.MetapointAccessor (Metapp_api.MetapointName)
    let map (payload : Ppxlib.payload) : Metapoint.t =
      let e = Metapp_preutils.Exp.of_payload payload in
      Ppxlib.Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
      let extracted_expr = map_subquotations#expression e in
      let index =
        Metapp_preutils.update
          (Metapp_preutils.Accu.add !Ppxlib.Ast_helper.default_loc)
          (Accessor.get metapoints) in
      let field = Name.get Metapp_api.metapoint_name in
      let metapoint_field = field_get (context_get metapoints_field) field in
        let loc = !Ppxlib.Ast_helper.default_loc in
      let extracted_expr : Ppxlib.expression =
        [%expr Some (Ppxlib.Ast_helper.with_default_loc
            [%e array_get (field_get (context_get loc_field) field) index]
            (function () -> [%e extracted_expr]))] in
      accu |> Metapp_preutils.mutate (List.cons
        (Expression (array_set metapoint_field index extracted_expr)));
      Metapoint.extension (extension_of_index index)
  end in
  let module Meta_map = Metapoint_mapper (Mapper) in
  let module Metadef (Item : Metapp_preutils.ItemS) = struct
    let map (super : Item.t Metapp_preutils.map) (item : Item.t) : Item.t =
      Ppxlib.Ast_helper.with_default_loc (Item.to_loc item) @@ fun () ->
      match Item.destruct_extension item with
      | Some (({ txt = "metadef"; _ }, payload), _) ->
          let defs =
            map_subquotations#structure
              (Metapp_preutils.Str.of_payload payload) in
          accu |> Metapp_preutils.mutate (List.cons (Definition
            (Metapp_preutils.mkloc defs)));
          Item.of_list []
      | _ -> super item
  end in
  let module Map = struct
    class map = object
      inherit Meta_map.map as super

      method! structure_item =
        let module M = Metadef (Metapp_preutils.Stri) in
        M.map super#structure_item

      method! signature_item =
        let module M = Metadef (Metapp_preutils.Sigi) in
        M.map super#signature_item
    end
  end in
  let k () : unit AccuTypes.escape = {
    instructions = List.rev !accu;
    quotation = fun () -> {
      fill = (fun () -> ());
      context = {
        metapoints = unmut_metapoints metapoints;
        loc = unmut_loc metapoints;
        subquotations = unmut_subquotations subquotations; }}} in
  ((module Map), k)

let transform (root_mapper : Ppxlib.structure Metapp_preutils.map)
    (get_mapper : #Ppxlib.Ast_traverse.map -> 'a Metapp_preutils.map)
    (s : 'a) : 'a =
  let (meta_map_module, k) = extract_metapoints () in
  let module Meta_map = (val meta_map_module) in
  let accu_options = ref { Options.empty with packages = ["ppxlib"] } in
  let module Metaopt (Item : Metapp_preutils.ItemS) = struct
    let map (super : Item.t Metapp_preutils.map) (item : Item.t) : Item.t =
      Ppxlib.Ast_helper.with_default_loc (Item.to_loc item) @@ fun () ->
      match Option.bind (Item.destruct_extension item) Options.handle with
      | None -> super item
      | Some option ->
          accu_options |> Metapp_preutils.mutate option;
          Item.of_list []
  end in
  let map = object
    inherit Meta_map.map as super

    method! structure_item =
      let module M = Metaopt (Metapp_preutils.Stri) in
      M.map super#structure_item

    method! signature_item =
      let module M = Metaopt (Metapp_preutils.Sigi) in
      M.map super#signature_item
  end in
  let s = get_mapper map s in
  match k () with
  | { instructions = []; _ } -> s
  | { instructions; quotation } ->
  match quotation () with { context; _ } ->
  let initial_parsetree =
    [Ppxlib.Ast_helper.Str.value Nonrecursive
      [Ppxlib.Ast_helper.Vb.mk (Metapp_preutils.Pat.var context_var)
        (Ppxlib.Ast_helper.Exp.match_ (deref (Metapp_preutils.Exp.ident
          (Ldot (metapp_api, "top_context"))))
          [Ppxlib.Ast_helper.Exp.case (Metapp_preutils.Pat.none ())
            (Ppxlib.Ast_helper.Exp.assert_ (Metapp_preutils.Exp.of_bool false));
            Ppxlib.Ast_helper.Exp.case
              (Metapp_preutils.Pat.some (Metapp_preutils.Pat.var context_var))
              (Metapp_preutils.Exp.var context_var)])]] in
  let make_instruction (accu : Ppxlib.structure) (instruction : instruction)
      : Ppxlib.structure =
    match instruction with
    | Expression expr ->
        let item =
          Ppxlib.Ast_helper.Str.value Nonrecursive
            [Ppxlib.Ast_helper.Vb.mk (Metapp_preutils.Pat.of_unit ()) expr] in
        item :: accu
    | Definition definition -> List.rev_append definition.txt accu in
  let accu = List.fold_left make_instruction initial_parsetree instructions in
  let parsetree = root_mapper (List.rev accu) in
  Metapp_api.top_context := Some context;
  let options = Options.rev !accu_options in
  if options.packages <> [] then
    begin
      Findlib_for_ppx.init_predicates ();
      Findlib.init ();
      Findlib_for_ppx.load_packages ~debug:options.debug_findlib
        options.packages;
    end;
  begin try
    Dyncompile.compile_and_load options
      (Ppxlib.Selected_ast.To_ocaml.copy_structure parsetree);
  with Dynlink.Error error ->
    Location.raise_errorf "%s" (Dynlink.error_message error)
  end;
  let mapper = replace_metapoints context.metapoints in
  get_mapper mapper s

let map = object (self)
  inherit Ppxlib.Ast_traverse.map as super

  method! structure s =
    transform self#structure (fun map -> map#structure) s

  method! signature s =
    transform self#structure (fun map -> map#signature) s
end

let () =
  Ppxlib.Driver.register_transformation "metapp"
    ~preprocess_impl:map#structure
    ~preprocess_intf:map#signature
