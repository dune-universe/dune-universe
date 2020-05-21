[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]

open Clang__bindings

open Clang__types

[%%meta Metapp.Stri.of_list (
  if Clangml_config.version >= { major = 3; minor = 5; subminor = 0 } then
    []
  else [%str
    type cxerrorcode =
      | Failure
      | Crashed
      | InvalidArguments
      | ASTReadError
    (** Error codes introduced in clang 3.5, declared here for compatibility.
        Only {!constr:Failure} will be used. *)

    let parse_translation_unit2 index filename command_line_args unsaved_files
        options : (cxtranslationunit, cxerrorcode) result =
      match
        parse_translation_unit index filename command_line_args unsaved_files
          options
      with
      | None -> Error Failure
      | Some tu -> Ok tu
    (** Compatibility wrapper for [parse_translation_unit2].
        In case of error, [Error Failure] will be returned. *)])]

[%%meta Metapp.Stri.of_list (
 if Clangml_config.version >= { major = 3; minor = 6; subminor = 0 } then [%str
   let predefined_expr_get_function_name cursor _decl =
     ext_predefined_expr_get_function_name cursor]
 else [%str
   let predefined_expr_get_function_name cursor decl =
     ext_predefined_expr_compute_name
       (ext_predefined_expr_get_ident_kind cursor) decl])]

[%%meta Metapp.Stri.of_list (
  if Clangml_config.version >= { major = 3; minor = 7; subminor = 0 } then
    []
  else [%str
    type cxvisitorresult =
      | Break
      | Continue

    let type_visit_fields ty visitor =
      visit_children (ty |> get_type_declaration) (fun cur _parent ->
        match get_cursor_kind cur with
        | FieldDecl ->
            begin
              match visitor cur with
              | Break -> Break
              | Continue -> Continue
            end
        | _ -> Continue)])]

[%%meta Metapp.Stri.of_list (
  if Clangml_config.version.major >= 8 then [%str
    let include_attributed_types =
      Cxtranslationunit_flags.include_attributed_types

    let type_non_null = TypeNonNull]
  else [%str
    let include_attributed_types =
      Cxtranslationunit_flags.none

    let type_get_modified_type ty =
      ty

    let type_non_null = NoAttr])]

let iter_visitor f visitor =
  let exn_ref = ref (None : exn option) in
  visitor begin fun cur ->
    try
      f cur;
      true
    with exn ->
      exn_ref := Some exn;
      false
  end;
  match !exn_ref with
  | None -> ()
  | Some exn -> raise exn

let list_of_iter iter =
  let children_ref = ref [] in
  iter begin fun cur ->
    children_ref := cur :: !children_ref
  end;
  List.rev !children_ref

let iter_children f c =
  iter_visitor f begin fun f ->
    ignore (visit_children c begin fun cur _par ->
      if f cur then
        Continue
      else
        Break
    end)
  end

let list_of_children c =
  list_of_iter (fun f -> iter_children f c)

let iter_type_fields f ty =
  iter_visitor f begin fun f ->
    ignore (type_visit_fields ty begin fun cur ->
      if f cur then
        Continue
      else
        Break
    end)
  end

let list_of_type_fields ty =
  list_of_iter (fun f -> iter_type_fields f ty)

let iter_decl_context f c =
  iter_visitor f begin fun f ->
    ignore (ext_decl_context_visit_decls c begin fun cur _par ->
      if f cur then
        Continue
      else
        Break
    end)
  end

let list_of_decl_context c =
  list_of_iter (fun f -> iter_decl_context f c)

let seq_of_diagnostics tu =
  let count = get_num_diagnostics tu in
  let rec next i () =
    if i < count then
      Seq.Cons (get_diagnostic tu i, next (succ i))
    else
      Seq.Nil in
  next 0

let format_diagnostics ?pp severities fmt tu =
  let filter diagnostics =
    List.mem (get_diagnostic_severity diagnostics) severities in
  match (seq_of_diagnostics tu |> Seq.filter filter) () with
  | Nil -> ()
  | Cons (hd, tl) ->
      let print_all_diagnostics fmt () =
        let print_diagnostics diagnostics =
          Format.fprintf fmt "@[%s@]@ "
            (format_diagnostic diagnostics
               Cxdiagnosticdisplayoptions.display_source_location) in
        print_diagnostics hd;
        tl |> Seq.iter print_diagnostics in
      match pp with
      | None -> Format.fprintf fmt "@[<v>%a@]" print_all_diagnostics ()
      | Some pp -> pp print_all_diagnostics fmt ()

let error = [Error; Fatal]

let warning_or_error = Warning :: error

let not_ignored_diagnostics = Note :: warning_or_error

let all_diagnostics = Ignored :: not_ignored_diagnostics

let seq_exists pred seq =
  (* "let exception" is OCaml >=4.04.0 only *)
  let module M = struct exception Exists end in
  try
    seq |> Seq.iter begin fun d ->
      if pred d then
        raise M.Exists
    end;
    false
  with M.Exists ->
    true

let has_severity l tu =
  seq_of_diagnostics tu |> seq_exists begin fun d ->
    List.mem (get_diagnostic_severity d) l
  end

(* is_integer, is_unsigned_integer, is_signed_integer and is_floating_point
   are implemented as Type::{isInteger, isUnsignedInteger, isSignedInteger,
   isFloatingPoint} in Type.h. *)
let is_unsigned_integer (ty : cxtypekind) =
  match ty with
  | Bool | Char_U | UChar | Char16 | Char32 | UShort | UInt | ULong | ULongLong
  | UInt128 -> true
  | _ -> false

let is_signed_integer (ty : cxtypekind) =
  match ty with
  | Char_S | SChar | WChar | Short | Int | Long | LongLong | Int128 -> true
  | _ -> false

let is_integer (ty : cxtypekind) =
  is_unsigned_integer ty || is_signed_integer ty

[%%meta Metapp.Stri.of_list (Metapp.filter.structure Metapp.filter [%str
let is_floating_point (ty : cxtypekind) =
  match ty with
  | Float | Double | LongDouble -> true
  | Float128
      [@if [%meta Metapp.Exp.of_bool
        (Clangml_config.version >= { major = 3; minor = 9; subminor = 0 })]] ->
      true
  | Half [@if [%meta Metapp.Exp.of_bool (Clangml_config.version.major >= 5)]] ->
      true
  | Float16
      [@if [%meta Metapp.Exp.of_bool (Clangml_config.version.major >= 6)]] ->
      true
  | _ -> false])]

let get_bits ~signed =
  if signed then
    ext_int_get_min_signed_bits
  else
    ext_int_get_active_bits

let int64_of_cxint_opt ?(signed = true) cxint =
  if get_bits ~signed cxint <= 64 then
    let result =
      if signed then
        ext_int_get_sext_value64 cxint
      else
        ext_int_get_zext_value64 cxint in
    Some result
  else
    None

let int64_of_cxint ?(signed = true) cxint =
  if get_bits ~signed cxint <= 64 then
    if signed then
      ext_int_get_sext_value64 cxint
    else
      ext_int_get_zext_value64 cxint
  else
    invalid_arg "int64_of_cxint"

let int_of_cxint_opt ?(signed = true) cxint =
  let bits = get_bits ~signed cxint in
  if bits <= (if signed then 32 else 31) then
    let result =
      if signed then
        ext_int_get_sext_value cxint
      else
        ext_int_get_zext_value cxint in
    Some result
  else if bits <= Sys.int_size then
    let result =
      if signed then
        ext_int_get_sext_value64 cxint
      else
        ext_int_get_zext_value64 cxint in
    Some (Int64.to_int result)
  else
    None

let int_of_cxint ?(signed = true) cxint =
  match int_of_cxint_opt ~signed cxint with
  | Some result -> result
  | None -> invalid_arg "int_of_cxint"

let string_of_cxint ?(signed = true) cxint =
  ext_int_to_string cxint 10 signed

let float_of_cxfloat_opt cxfloat =
  match ext_float_get_semantics cxfloat with
  | IEEEsingle -> Some (ext_float_convert_to_float cxfloat)
  | IEEEdouble -> Some (ext_float_convert_to_double cxfloat)
  | _ -> None

let float_of_cxfloat cxfloat =
  match ext_float_get_semantics cxfloat with
  | IEEEsingle -> ext_float_convert_to_float cxfloat
  | IEEEdouble -> ext_float_convert_to_double cxfloat
  | _ -> invalid_arg "float_of_cxfloat"

let string_of_cxfloat cxfloat =
  ext_float_to_string cxfloat

let string_of_cxerrorcode cxerrorcode =
  match cxerrorcode with
  | Failure -> "generic error code, no further details are available"
  | Crashed -> "libclang crashed while performing the requested operation"
  | InvalidArguments -> "the arguments violate the function contract"
  | ASTReadError -> "an AST deserialization error has occurred"

(* From CompilerInvocation.cpp:ParseFrontendArgs *)

let string_of_language language =
  match language with
  | C -> "c"
  | OpenCL -> "cl"
  | CUDA -> "cuda"
  | HIP -> "hip"
  | CXX -> "c++"
  | ObjC -> "objective-c"
  | ObjCXX -> "objective-c++"
  | RenderScript -> "renderscript"

let language_of_string s =
  match s with
  | "c" | "C" -> C
  | "cl" -> OpenCL
  | "cuda" -> CUDA
  | "hip" -> HIP
  | "c++" | "C++" | "cpp" | "CPP" -> CXX
  | "objective-c" | "objc" -> ObjC
  | "objective-c++" | "objc++" | "objcpp" -> ObjCXX
  | "renderscript" -> RenderScript
  | _ -> invalid_arg "language_of_string"

let language_of_string_opt s =
  try
    Some (language_of_string s)
  with Invalid_argument _ ->
    None

let suffix_of_language language =
  match language with
  | C -> ".c"
  | OpenCL -> ".cl"
  | CUDA -> ".cuda"
  | HIP -> ".hip"
  | CXX -> ".cpp"
  | ObjC -> ".m"
  | ObjCXX -> ".mm"
  | RenderScript -> ".renderscript"

let extern_of_language language =
  match language with
  | C -> "C"
  | CXX -> "C++"
  | _ -> invalid_arg "extern_of_language"

let parse_file_res ?(index = create_index true true)
    ?(command_line_args = []) ?(unsaved_files = [])
    ?(options = default_editing_translation_unit_options ()) filename =
  parse_translation_unit2 index filename (Array.of_list command_line_args)
    (Array.of_list unsaved_files) options

let parse_file ?index ?command_line_args ?unsaved_files ?options
    filename =
  match
    parse_file_res ?index ?command_line_args ?unsaved_files ?options
      filename
  with
  | Ok cxtranslationunit -> cxtranslationunit
  | Error cxerrorcode -> failwith (string_of_cxerrorcode cxerrorcode)

let parse_string_res ?index ?(filename = "<string>.c")
    ?command_line_args ?(unsaved_files = [])
    ?options contents =
  parse_file_res ?index ?command_line_args
    ~unsaved_files:({ filename; contents } :: unsaved_files)
    ?options filename

let parse_string ?index ?filename ?command_line_args ?unsaved_files
    ?options contents =
  match
    parse_string_res ?index ?filename ?command_line_args
      ?unsaved_files ?options contents
  with
  | Ok cxtranslationunit -> cxtranslationunit
  | Error cxerrorcode -> failwith (string_of_cxerrorcode cxerrorcode)

let string_of_cxx_access_specifier specifier =
  match specifier with
  | CXXInvalidAccessSpecifier ->
      invalid_arg "string_of_cxx_access_specifier"
  | CXXPublic -> "public"
  | CXXProtected -> "protected"
  | CXXPrivate -> "private"

let cursor_get_translation_unit cursor =
  Obj.obj (Obj.field (Obj.repr cursor) 1)

let sourcelocation_get_translation_unit cursor =
  Obj.obj (Obj.field (Obj.repr cursor) 1)

let binary_of_overloaded_operator_kind kind =
  match kind with
  | Plus -> Add
  | Minus -> Sub
  | Star -> Mul
  | Slash -> Div
  | Percent -> Rem
  | Amp -> And
  | Pipe -> Or
  | Equal  -> Assign
  | Less -> LT
  | Greater -> GT
  | PlusEqual -> AddAssign
  | MinusEqual -> SubAssign
  | StarEqual -> MulAssign
  | SlashEqual -> DivAssign
  | PercentEqual -> RemAssign
  | AmpEqual -> AndAssign
  | PipeEqual -> OrAssign
  | LessLess -> Shl
  | GreaterGreater -> Shr
  | LessLessEqual -> ShlAssign
  | GreaterGreaterEqual -> ShrAssign
  | EqualEqual -> EQ
  | ExclaimEqual -> NE
  | LessEqual -> LE
  | GreaterEqual -> GE
  | AmpAmp -> LAnd
  | PipePipe -> LOr
  | Comma -> Comma
  | ArrowStar -> PtrMemD
  | _ -> invalid_arg "binary_of_overloaded_operator_kind"
