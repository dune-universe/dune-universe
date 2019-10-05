
open Clang__bindings

open Clang__compat

open Clang__types

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
  let exception Exists in
  try
    seq |> Seq.iter begin fun d ->
      if pred d then
        raise Exists
    end;
    false
  with Exists ->
    true

let has_severity l tu =
  seq_of_diagnostics tu |> seq_exists begin fun d ->
    List.mem (get_diagnostic_severity d) l
  end

let int64_of_cxint_opt cxint =
  if ext_int_get_min_signed_bits cxint <= 64 then
    Some (ext_int_get_sext_value64 cxint)
  else
    None

let int64_of_cxint cxint =
  if ext_int_get_min_signed_bits cxint <= 64 then
    ext_int_get_sext_value64 cxint
  else
    invalid_arg "int64_of_cxint"

let int_of_cxint_opt cxint =
  if ext_int_get_min_signed_bits cxint <= Sys.int_size then
    Some (ext_int_get_sext_value cxint)
  else
    None

let int_of_cxint cxint =
  if ext_int_get_min_signed_bits cxint <= Sys.int_size then
    ext_int_get_sext_value cxint
  else
    invalid_arg "int_of_cxint"

let string_of_cxint cxint =
  ext_int_to_string cxint 10 true

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
