module type Ast_types = sig
  type item

  type module_binding

  type module_expr
end

module Ast_definitions (Types : Ast_types) = struct
  include Types

  type contents = item list

  type include_declaration = module_expr Parsetree.include_infos

  type item_desc =
    | Extension of Parsetree.extension * Parsetree.attributes
    | Type of Asttypes.rec_flag * Parsetree.type_declaration list
    | Module of module_binding
    | Modtype of Parsetree.module_type_declaration
    | Include of include_declaration
    | Other of item

  type wrapped_item = item_desc Location.loc

  type 'a attributed = {
      attrs : Parsetree.attributes;
      contents : 'a;
    }

  let mkattr ~loc ?(attrs = []) contents : _ attributed Location.loc =
    { loc; txt = { attrs; contents }}

  type module_binding_desc = {
      name : string option Location.loc;
      expr : module_expr;
    }

  type wrapped_module_binding = module_binding_desc attributed Location.loc

  type module_expr_desc =
    | Ident of Longident.t Location.loc
    | Contents of contents
    | Functor of Metapp.functor_parameter * module_expr
    | Constraint of module_expr Lazy.t * Parsetree.module_type
    | Other of module_expr

  type wrapped_module_expr = module_expr_desc attributed Location.loc
end

module type S = sig
  module Types : Ast_types

  include module type of Ast_definitions (Types)

  val empty : unit -> item

  val destruct : item -> wrapped_item

  val build : wrapped_item -> item

  val choose :
      (unit -> Parsetree.structure_item) ->
        (unit -> Parsetree.signature_item) -> item

  val map : Ast_mapper.mapper -> Ast_mapper.mapper -> contents -> contents

  val map_item : Ast_mapper.mapper -> Ast_mapper.mapper -> item -> item

  val format : Format.formatter -> contents -> unit

  val destruct_payload : loc:Location.t -> Parsetree.payload -> contents

  val destruct_module_binding : module_binding -> wrapped_module_binding

  val build_module_binding : wrapped_module_binding -> module_binding

  val destruct_module_expr : module_expr -> wrapped_module_expr

  val build_module_expr : wrapped_module_expr -> module_expr

  val choose_module_expr :
      (unit -> Parsetree.module_expr) -> (unit -> Parsetree.module_type) ->
        module_expr
end

module Structure_types = struct
  type item = Parsetree.structure_item

  type module_binding = Parsetree.module_binding

  type module_expr = Parsetree.module_expr
end

let rec longident_of_module_expr (expr : Parsetree.module_expr) : Longident.t =
  match expr.pmod_desc with
  | Pmod_ident lid -> lid.txt
  | Pmod_apply (e, x) ->
      Lapply (longident_of_module_expr e, longident_of_module_expr x)
  | _ -> invalid_arg "longident_of_module_expr"

let rec module_expr_of_longident ?(attrs = [])
    (lid : Longident.t Location.loc) =
  let loc = lid.loc in
  match lid.txt with
  | Lapply (e, x) ->
      Ast_helper.Mod.apply ~loc ~attrs
        (module_expr_of_longident { loc; txt = e })
        (module_expr_of_longident { loc; txt = x })
  | _ -> Ast_helper.Mod.ident ~loc ~attrs lid

module Structure : S with module Types = Structure_types = struct
  module Types = Structure_types

  include Ast_definitions (Types)

  let empty () : item = [%stri include struct end]

  let destruct (item : item) : wrapped_item =
    let txt =
      match item.pstr_desc with
      | Pstr_extension (ext, attrs) -> Extension (ext, attrs)
      | Pstr_type (rec_flag, list) -> Type (rec_flag, list)
      | Pstr_module binding -> Module binding
      | Pstr_modtype declaration -> Modtype declaration
      | Pstr_include inc -> Include inc
      | _ -> Other item in
    { loc = item.pstr_loc; txt }

  let build (desc : wrapped_item) : item =
    let loc = desc.loc in
    match desc.txt with
    | Extension (ext, attrs) -> Ast_helper.Str.extension ~loc ~attrs ext
    | Type (rec_flag, list) -> Ast_helper.Str.type_ ~loc rec_flag list
    | Module binding -> Ast_helper.Str.module_ ~loc binding
    | Modtype declaration -> Ast_helper.Str.modtype ~loc declaration
    | Include inc -> Ast_helper.Str.include_ ~loc inc
    | Other item -> Ast_helper.Str.mk ~loc item.pstr_desc

  let choose structure _signature =
    structure ()

  let map (mapper : Ast_mapper.mapper) submapper (contents : contents)
      : contents =
    mapper.structure submapper contents

  let map_item (mapper : Ast_mapper.mapper) submapper item =
    mapper.structure_item submapper item

  let format = Pprintast.structure

  let destruct_payload ~loc (payload : Parsetree.payload) =
    let structure_expected preceding_symbol =
      Location.raise_errorf ~loc
        "Structure expected (try to remove the preceding `%s')."
        preceding_symbol in
    match payload with
    | PStr s -> s
    | PPat (_p, _e) -> structure_expected "?"
    | PSig _ | PTyp _ -> structure_expected ":"

  let destruct_module_binding (binding : module_binding)
      : wrapped_module_binding =
    let name =
      Metapp.map_loc Metapp.string_option_of_module_name binding.pmb_name in
    { loc = binding.pmb_loc; txt = {
      attrs = binding.pmb_attributes; contents = {
        name; expr = binding.pmb_expr; }}}

  let build_module_binding (binding : wrapped_module_binding) =
    match binding with { loc; txt = { attrs; contents = { name; expr }}} ->
      let name = Metapp.map_loc Metapp.module_name_of_string_option name in
      Ast_helper.Mb.mk ~loc ~attrs name expr

  let destruct_module_expr (expr : module_expr) : wrapped_module_expr =
    let contents =
      match expr.pmod_desc with
      | Pmod_ident lid -> Ident lid
      | Pmod_structure s -> Contents s
      | Pmod_constraint (m, t) -> Constraint (Lazy.from_val m, t)
      | Pmod_apply (e, x) ->
          begin
            match longident_of_module_expr e, longident_of_module_expr x with
            | e, x -> Ident { loc = expr.pmod_loc; txt = Lapply (e, x) }
            | exception (Invalid_argument _) -> Other expr
          end
      | _ ->
          match Metapp.Mod.destruct_functor expr with
          | Some (f, s) -> Functor (f, s)
          | None -> Other expr in
    { loc = expr.pmod_loc; txt = {
      attrs = expr.pmod_attributes; contents }}

  let build_module_expr (expr : wrapped_module_expr) =
    match expr with { loc; txt = { attrs; contents }} ->
      match contents with
      | Ident lid -> module_expr_of_longident lid
      | Contents s -> Ast_helper.Mod.structure ~loc ~attrs s
      | Functor (f, s) -> Metapp.Mod.functor_ ~loc ~attrs f s
      | Constraint (m, t) ->
          Ast_helper.Mod.constraint_ ~loc ~attrs (Lazy.force m) t
      | Other expr -> Ast_helper.Mod.mk ~loc ~attrs expr.pmod_desc

  let choose_module_expr make_expr _make_type =
    make_expr ()
end

module Signature_types = struct
  type item = Parsetree.signature_item

  type module_binding = Parsetree.module_declaration

  type module_expr = Parsetree.module_type
end

module Signature : S with module Types = Signature_types = struct
  module Types = Signature_types

  include Ast_definitions (Types)

  let empty () : item = [%sigi: include sig end]

  let destruct (item : item) : item_desc Location.loc =
    let txt =
      match item.psig_desc with
      | Psig_extension (ext, attrs) -> Extension (ext, attrs)
      | Psig_type (rec_flag, list) -> Type (rec_flag, list)
      | Psig_module declaration -> Module declaration
      | Psig_modtype declaration -> Modtype declaration
      | Psig_include inc -> Include inc
      | _ -> Other item in
    { loc = item.psig_loc; txt }

  let build (desc : item_desc Location.loc) =
    let loc = desc.loc in
    match desc.txt with
    | Extension (ext, attrs) -> Ast_helper.Sig.extension ~loc ~attrs ext
    | Type (rec_flag, list) -> Ast_helper.Sig.type_ ~loc rec_flag list
    | Module declaration -> Ast_helper.Sig.module_ ~loc declaration
    | Modtype declaration -> Ast_helper.Sig.modtype ~loc declaration
    | Include inc -> Ast_helper.Sig.include_ ~loc inc
    | Other item -> item

  let choose _make_structure make_signature =
    make_signature ()

  let map (mapper : Ast_mapper.mapper) submapper contents =
    mapper.signature submapper contents

  let map_item (mapper : Ast_mapper.mapper) submapper item =
    mapper.signature_item submapper item

  let format formatter contents =
    Pprintast.signature formatter contents

  let destruct_payload ~loc (payload : Parsetree.payload) =
    match payload with
    | PSig s -> s
    | PTyp _t ->
        Location.raise_errorf ~loc
          "Signature expected (try to capitalize the leading identifier)."
    | PPat _ ->
        Location.raise_errorf ~loc
          "Signature expected (try to replace the preceding `?' by `:`)."
    | PStr _ ->
        Location.raise_errorf ~loc
          "Signature expected (try to add `:' before)."

  let destruct_module_binding (declaration : module_binding)
      : wrapped_module_binding =
    let name =
      Metapp.map_loc Metapp.string_option_of_module_name declaration.pmd_name in
    { loc = declaration.pmd_loc; txt = {
      attrs = declaration.pmd_attributes; contents = {
        name; expr = declaration.pmd_type; }}}

  let build_module_binding (binding : wrapped_module_binding) =
    match binding with { loc; txt = { attrs; contents = { name; expr }}} ->
      let name = Metapp.map_loc Metapp.module_name_of_string_option name in
      Ast_helper.Md.mk ~loc ~attrs name expr

  let destruct_module_expr (expr : module_expr) : wrapped_module_expr =
    let contents =
      match expr.pmty_desc with
      | Pmty_ident lid -> Ident lid
      | Pmty_signature s -> Contents s
      | _ ->
          match Metapp.Mty.destruct_functor expr with
          | Some (f, s) -> Functor (f, s)
          | _ -> Other expr in
    { loc = expr.pmty_loc; txt = {
      attrs = expr.pmty_attributes; contents }}

  let build_module_expr (expr : wrapped_module_expr) =
    match expr with { loc; txt = { attrs; contents }} ->
      match contents with
      | Ident lid -> Ast_helper.Mty.ident ~loc ~attrs lid
      | Contents s -> Ast_helper.Mty.signature ~loc ~attrs s
      | Functor (f, s) -> Metapp.Mty.functor_ ~loc ~attrs f s
      | Constraint (_m, t) -> t
      | Other expr -> Ast_helper.Mty.mk ~loc ~attrs expr.pmty_desc

  let choose_module_expr _make_expr make_type =
    make_type ()
end
