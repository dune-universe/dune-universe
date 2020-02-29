module Counter = struct
  type t = int ref

  let make () =
    ref 0

  let count counter =
    let result = !counter in
    counter := succ result;
    result
end

let extension_of_index (i : int) : Parsetree.extension =
  (Metapp_preutils.mkloc "meta", Metapp_preutils.payload_of_int i)

let deref (e : Parsetree.expression) : Parsetree.expression =
  Metapp_preutils.apply (Metapp_preutils.ident (Lident "!")) [e]

let array_get (a : Parsetree.expression) (index : int) : Parsetree.expression =
  let i = Metapp_preutils.Exp.of_int index in
  Metapp_preutils.apply (Metapp_preutils.ident (Ldot (Lident "Array", "get")))
    [a; i]

let array_set (a : Parsetree.expression) (index : int)
    (v : Parsetree.expression) : Parsetree.expression =
  let i = Metapp_preutils.Exp.of_int index in
  Metapp_preutils.apply (Metapp_preutils.ident (Ldot (Lident "Array", "set")))
    [a; i; v]

let string_list_of_payload (payload : Parsetree.payload) : string list =
  List.map Metapp_preutils.string_of_arbitrary_expression
    (Metapp_preutils.list_of_tuple (Metapp_preutils.Exp.of_payload payload))

module Options = struct
  type t = {
      packages : string list;
      directories : string list;
      flags : string list;
      plainsource : bool;
      debug_findlib : bool;
    }

  let empty = {
    packages = [];
    directories = [];
    flags = [];
    plainsource = false;
    debug_findlib = false;
  }

  let rev options =
    { options with
      packages = List.rev options.packages;
      directories = List.rev options.directories;
      flags = List.rev options.flags }

  let add_directories directories options =
    { options with
      directories = List.rev_append directories options.directories }

  let add_packages packages options =
    { options with
      packages = List.rev_append packages options.packages }

  let add_flags flags options =
    { options with
      flags = List.rev_append flags options.flags }

  let set_plainsource plainsource options =
    { options with plainsource }

  let set_debug_findlib debug_findlib options =
    { options with debug_findlib }

  let handle (({ txt; _ }, payload) : Parsetree.extension) : (t -> t) option =
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
    | _ -> None
end

type instruction =
  | Expression of Parsetree.expression
  | Definition of Parsetree.structure Location.loc

let get_expression (instruction : instruction)
    : Parsetree.expression =
  match instruction with
  | Expression expression -> expression
  | Definition definition ->
      Location.raise_errorf ~loc:definition.loc
        "Definitions are only allowed at top-level"

module rec AccuTypes : sig
  type escape = {
      instructions : instruction list;
      context : Metapp_api.context;
    }

  type 'a quotations = ((unit -> 'a) * escape) Accu.t ref

  type 'a metapoints = Location.t Accu.t ref
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
    ref Accu.empty
end
and MutableMetapoints : Metapp_api.MetapointsWithMakeS
with type 'a x = 'a AccuMetapoint.t =
    Metapp_api.MetapointsWithMake (AccuMetapoint)
and AccuMetapoint : Metapp_api.UnaryMakeS
with type 'a t = 'a AccuTypes.metapoints = struct
  type 'a t = 'a AccuTypes.metapoints

  let make () =
    ref Accu.empty
end

module type MetapointsMapperS =
  functor (Metapoint : Metapp_api.MetapointS) -> sig
    val map : Parsetree.payload -> Metapoint.t
  end

let metapoint_mapper (mapper : (module MetapointsMapperS)) : Ast_mapper.mapper =
  let module Mapper = (val mapper) in
  let module Mapper' (Metapoint : Metapp_api.MetapointS) = struct
    let map (mapper : Ast_mapper.mapper) (m : Metapoint.t) : Metapoint.t =
      Ast_helper.with_default_loc (Metapoint.to_loc m) @@ fun () ->
      match Metapoint.destruct_extension m with
      | Some ({ txt = "meta"; _ }, payload) ->
          let module Map = Mapper (Metapoint) in
          Map.map payload
    | _ -> Metapoint.mapper.get Ast_mapper.default_mapper mapper m
  end in
  { Ast_mapper.default_mapper with
    expr = (let module M = Mapper' (Metapp_api.Exp) in M.map);
    pat = (let module M = Mapper' (Metapp_api.Pat) in M.map);
    typ = (let module M = Mapper' (Metapp_api.Typ) in M.map);
    class_type = (let module M = Mapper' (Metapp_api.Cty) in M.map);
    class_type_field = (let module M = Mapper' (Metapp_api.Ctf) in M.map);
    class_expr = (let module M = Mapper' (Metapp_api.Cl) in M.map);
    class_field = (let module M = Mapper' (Metapp_api.Cf) in M.map);
    module_type = (let module M = Mapper' (Metapp_api.Mty) in M.map);
    module_expr = (let module M = Mapper' (Metapp_api.Mod) in M.map);
    signature_item = (let module M = Mapper' (Metapp_api.Sigi) in M.map);
    structure_item = (let module M = Mapper' (Metapp_api.Stri) in M.map);
  }

(*
module UnaryInt = struct
  type 'a t = int
end

module HolesCount = Metapp_api.Holes (UnaryInt)
*)

let unmut_metapoints (context : MutableMetapoints.t)
    : Metapp_api.OptionArrayMetapoints.t =
  let module Map = Metapp_api.MetapointMap (MutableMetapoints)
      (Metapp_api.OptionArrayMetapoints)
      (struct
        type 'a x = 'a AccuMetapoint.t

        type 'a y = 'a option array

        let map accu = Array.make (Accu.length !accu) None
      end) in
  Map.map context

let unmut_loc (context : MutableMetapoints.t)
    : Metapp_api.MetapointsLocation.t =
  let module Map = Metapp_api.MetapointMap (MutableMetapoints)
      (Metapp_api.MetapointsLocation) (struct
        type 'a x = 'a AccuMetapoint.t

        type _ y = Location.t array

        let map accu = Accu.to_array !accu
      end) in
  Map.map context

let unmut_subquotations (context : MutableQuotations.t)
    : Metapp_api.ArrayQuotations.t =
  let module Map = Metapp_api.QuotationMap (MutableQuotations)
      (Metapp_api.ArrayQuotations) (struct
        type 'a x = 'a AccuQuotation.t

        type 'a y = 'a Metapp_api.ArrayQuotation.t

        let map accu = Array.map
            (fun (fill, { AccuTypes.context; _ }) ->
              ({ context; fill } : 'a Metapp_api.ArrayQuotation.quotation))
            (Accu.to_array !accu)
      end) in
  Map.map context

(*
let unmut_holes_count (holes_count : MutableHolesCount.t) : HolesCount.t =
  let module Map = Metapp_api.HolesMap (MutableHolesCount.UnaryCounter)
      (UnaryInt) (struct
    type 'a x = Counter.t

    type 'a y = int

    let map = (!)
  end) in
  Map.map holes_count
*)

let context_var = "__context"

let fill_var = "__fill"

let metapoints_field = "metapoints"

let loc_field = "loc"

let subquotations_field = "subquotations"

let field_get (expr : Parsetree.expression) (field : string)
    : Parsetree.expression =
  Ast_helper.Exp.field expr (Metapp_preutils.mkloc (Longident.Lident field))

let context_get (field : string) : Parsetree.expression =
  field_get (Metapp_preutils.ident (Lident context_var)) field

let replace_metapoints (contents : Metapp_api.OptionArrayMetapoints.t)
    : Ast_mapper.mapper =
  let module Mapper (Metapoint : Metapp_api.MetapointS) = struct
    module Accessor =
      Metapoint.MetapointAccessor (Metapp_api.OptionArrayMetapoints)

    let map (payload : Parsetree.payload) : Metapoint.t =
      Option.get
        (Accessor.get contents).(Metapp_preutils.int_of_payload payload)
  end in
  metapoint_mapper (module Mapper)

let metapp_api = Longident.Lident "Metapp_api"

let rec extract_subquotations
    (quotations : MutableQuotations.t) : Ast_mapper.mapper =
  let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression)
      : Parsetree.expression =
    Ast_helper.with_default_loc e.pexp_loc @@ fun () ->
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
    | None -> Ast_mapper.default_mapper.expr mapper e
    | Some (antiquotable, payload) ->
        let module M = (val antiquotable) in
        let module Quotation = M.QuotationAccessor (MutableQuotations) in
        let module Name = M.QuotationAccessor (Metapp_api.QuotationName) in
        let quotation = M.of_payload payload in
        let (mapper, k) = extract_metapoints () in
        let quotation = M.mapper.get mapper mapper quotation in
        let escape = k () in
        let fill () =
          let mapper = replace_metapoints escape.context.metapoints in
          M.mapper.get mapper mapper quotation in
        let index =
          Metapp_preutils.update (Accu.add (fill, escape))
            (Quotation.get quotations) in
        let field_name = Name.get Metapp_api.quotation_name in
        Ast_helper.Exp.let_ Nonrecursive
          [Ast_helper.Vb.mk (Metapp_preutils.Pat.record [
             (Ldot (Ldot (metapp_api, "ArrayQuotation"), "context"),
               Metapp_preutils.Pat.var context_var);
               (Lident "fill", Metapp_preutils.Pat.var fill_var)])
            (array_get
              (field_get (context_get subquotations_field) field_name) index)]
          (Metapp_preutils.sequence
            (List.map get_expression escape.instructions @
              [Metapp_preutils.apply (Metapp_preutils.ident (Lident fill_var))
                [Metapp_preutils.Exp.of_unit ()]])) in
  { Ast_mapper.default_mapper with expr }

and extract_metapoints () : Ast_mapper.mapper * (unit -> AccuTypes.escape) =
  let accu = ref [] in
  let metapoints = MutableMetapoints.make () in
  let subquotations = MutableQuotations.make () in
  let mapper_subquotations = extract_subquotations subquotations in
  let module Mapper (Metapoint : Metapp_api.MetapointS) = struct
    module Accessor = Metapoint.MetapointAccessor (MutableMetapoints)
    module Name = Metapoint.MetapointAccessor (Metapp_api.MetapointName)
    let map (payload : Parsetree.payload) : Metapoint.t =
      let e = Metapp_preutils.Exp.of_payload payload in
      let extracted_expr = mapper_subquotations.expr mapper_subquotations e in
      let index =
        Metapp_preutils.update (Accu.add !Ast_helper.default_loc)
          (Accessor.get metapoints) in
      let field = Name.get Metapp_api.metapoint_name in
      let metapoint_field = field_get (context_get metapoints_field) field in
      let extracted_expr =
        Metapp_preutils.Exp.some
          (Metapp_preutils.apply
            (Metapp_preutils.ident
              (Ldot (Lident "Ast_helper", "with_default_loc")))
            [array_get (field_get (context_get loc_field) field) index;
              Ast_helper.Exp.function_
                [Ast_helper.Exp.case (Metapp_preutils.Pat.of_unit ())
                  extracted_expr]]) in
      accu |> Metapp_preutils.mutate (List.cons
        (Expression (array_set metapoint_field index extracted_expr)));
      Metapoint.extension (extension_of_index index)
  end in
  let meta_mapper = metapoint_mapper (module Mapper) in
  let module Metadef (Item : Metapp_preutils.ItemS) = struct
    let map (mapper : Ast_mapper.mapper) (item : Item.t) : Item.t =
      Ast_helper.with_default_loc (Item.to_loc item) @@ fun () ->
      match Item.destruct_extension item with
      | Some ({ txt = "metadef"; _ }, payload) ->
          let defs =
            mapper_subquotations.structure mapper_subquotations
              (Metapp_preutils.Str.of_payload payload) in
          accu |> Metapp_preutils.mutate (List.cons (Definition
            (Metapp_preutils.mkloc defs)));
          Item.of_list []
      | _ -> Item.mapper.get meta_mapper mapper item
  end in
  let mapper = { meta_mapper with
    structure_item = (let module M = Metadef (Metapp_preutils.Stri) in M.map);
    signature_item = let module M = Metadef (Metapp_preutils.Sigi) in M.map } in
  let k () : AccuTypes.escape = {
    instructions = List.rev !accu;
    context = {
      metapoints = unmut_metapoints metapoints;
      loc = unmut_loc metapoints;
      subquotations = unmut_subquotations subquotations; }} in
  (mapper, k)

let output_structure (channel : out_channel) (s : Parsetree.structure) =
  let fmt = Format.formatter_of_out_channel channel in
  Pprintast.structure fmt s;
  Format.pp_print_flush fmt ()

type compiler = {
    command : string;
    archive_option : string;
    archive_suffix : string;
  }

let compiler : compiler =
  if Dynlink.is_native then {
    command = "ocamlopt";
    archive_option = "-shared";
    archive_suffix = ".cmxs";
  }
  else {
    command = "ocamlc";
    archive_option = "-a";
    archive_suffix = ".cma";
  }

let compile (options : Options.t) (source_filename : string)
    (object_filename : string) : unit =
  let flags =
    options.flags @
    List.concat_map (fun directory -> ["-I"; directory])
      options.directories @
    ["-I"; "+compiler-libs"; "-w"; "-40"; compiler.archive_option;
      source_filename; "-o"; object_filename] in
  let preutils_cmi = "metapp_preutils.cmi" in
  let api_cmi = "metapp_api.cmi" in
  let dune_preutils_path = "preutils/.metapp_preutils.objs/byte/" in
  let dune_api_path = "api/.metapp_api.objs/byte/" in
  let (flags, packages) =
    if Sys.file_exists preutils_cmi && Sys.file_exists api_cmi then
      (flags, options.packages)
    else if Sys.file_exists (Filename.concat dune_preutils_path preutils_cmi) &&
      Sys.file_exists (Filename.concat dune_api_path api_cmi) then
      (["-I"; dune_preutils_path; "-I"; dune_api_path] @ flags,
        options.packages)
    else
      (flags, ["metapp.preutils"; "metapp.api"] @ options.packages) in
  let commands =
    match packages with
    | [] ->
        [(compiler.command ^ ".opt", flags); (compiler.command, flags)]
    | _ ->
        [("ocamlfind",
          [compiler.command; "-package"; String.concat "," packages] @
          flags)] in
  let rec try_commands list =
    match list with
    | [] -> assert false
    | (command, args) :: tl ->
        let command_line = Filename.quote_command command args in
        match Sys.command command_line with
        | 0 -> ()
        | 127 when tl <> [] -> try_commands tl
        | exit_code ->
            Location.raise_errorf ~loc:!Ast_helper.default_loc
              "@[Unable@ to@ compile@ preprocessor:@ command-line@ \"%s\"@ \
                failed@ with@ exit-code@ %d@]@."
              (String.escaped command_line) exit_code in
  try_commands commands

(* Code taken from pparse.ml (adapted for a channel instead of a filename to use
   open_temp_file), because Pparse.write_ast is introduced in OCaml 4.04.0. *)
let write_ast (plainsource : bool) (channel : out_channel)
    (structure : Parsetree.structure) : unit =
  if plainsource then
    Format.fprintf (Format.formatter_of_out_channel channel)
      "%a@." Pprintast.structure structure
  else
    begin
      output_string channel Config.ast_impl_magic_number;
      output_value channel !Location.input_name;
      output_value channel structure
    end

let compile_and_load (options : Options.t) (structure : Parsetree.structure)
  : unit =
  let (source_filename, channel) = Filename.open_temp_file "metapp" ".ml" in
  Fun.protect (fun () ->
    Fun.protect (fun () ->
      write_ast options.plainsource channel structure)
      ~finally:(fun () -> close_out channel);
    let object_filename =
      Filename.remove_extension source_filename ^
      compiler.archive_suffix in
    compile options source_filename object_filename;
    Fun.protect (fun () -> Dynlink.loadfile object_filename)
      ~finally:(fun () -> Sys.remove object_filename))
    ~finally:(fun () -> Sys.remove source_filename)

let transform (root_mapper : Ast_mapper.mapper)
    (get_mapper : Ast_mapper.mapper -> 'a Metapp_preutils.mapper_item)
    (s : 'a) : 'a =
  let (meta_mapper, k) = extract_metapoints () in
  let accu_options = ref Options.empty in
  let module Metaopt (Item : Metapp_preutils.ItemS) = struct
    let map (mapper : Ast_mapper.mapper) (item : Item.t) : Item.t =
      Ast_helper.with_default_loc (Item.to_loc item) @@ fun () ->
      match Option.bind (Item.destruct_extension item) Options.handle with
      | None -> Item.mapper.get meta_mapper mapper item
      | Some option ->
          accu_options |> Metapp_preutils.mutate option;
          Item.of_list []
  end in
  let mapper = { meta_mapper with
    structure_item = (let module M = Metaopt (Metapp_preutils.Stri) in M.map);
    signature_item = let module M = Metaopt (Metapp_preutils.Sigi) in M.map } in
  let s = get_mapper mapper mapper s in
  match k () with
  | { instructions = []; _ } -> s
  | { instructions; context } ->
  let initial_parsetree =
    [Ast_helper.Str.value Nonrecursive
      [Ast_helper.Vb.mk (Metapp_preutils.Pat.var context_var)
        (Ast_helper.Exp.match_ (deref (Metapp_preutils.ident
          (Ldot (metapp_api, "top_context"))))
          [Ast_helper.Exp.case (Metapp_preutils.Pat.none ())
            (Ast_helper.Exp.assert_ (Metapp_preutils.Exp.of_bool false));
            Ast_helper.Exp.case
              (Metapp_preutils.Pat.some (Metapp_preutils.Pat.var context_var))
              (Metapp_preutils.Exp.var context_var)])]] in
  let make_instruction (accu : Parsetree.structure) (instruction : instruction)
      : Parsetree.structure =
    match instruction with
    | Expression expr ->
        let item =
          Ast_helper.Str.value Nonrecursive
            [Ast_helper.Vb.mk (Metapp_preutils.Pat.of_unit ()) expr] in
        item :: accu
    | Definition definition -> List.rev_append definition.txt accu in
  let accu = List.fold_left make_instruction initial_parsetree instructions in
  let parsetree = root_mapper.structure root_mapper (List.rev accu) in
  Metapp_api.top_context := Some context;
  let options = Options.rev !accu_options in
  if options.packages <> [] then
    begin
      Findlib_for_ppx.init_predicates ();
      Findlib.init ();
      Findlib_for_ppx.load_packages ~debug:options.debug_findlib
        options.packages;
    end;
  compile_and_load options parsetree;
  let mapper = replace_metapoints context.metapoints in
  get_mapper mapper mapper s

let mapper : Ast_mapper.mapper =
  { Ast_mapper.default_mapper with
    structure =
      (fun mapper -> transform mapper (fun mapper -> mapper.structure));
    signature =
      (fun mapper -> transform mapper (fun mapper -> mapper.signature)); }

let rewriter _config _cookies : Ast_mapper.mapper =
  mapper

let () =
  Migrate_parsetree.Driver.register ~name:"metapp" ~position:(-20)
    (module Migrate_parsetree.OCaml_current)
    rewriter
