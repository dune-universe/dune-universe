[@@@ocaml.warning "-30"]

open Clang__bindings
open Clang__types

module Common = struct
let pp_cxint fmt cxint =
  Format.pp_print_string fmt (ext_int_to_string cxint 10 true)

let pp_cxfloat fmt cxfloat =
  Format.pp_print_string fmt (ext_float_to_string cxfloat)

(** {2 Abstractions from libclang's types} *)

(** The following types describe locations and literals that can be either
    produced by libclang or constructed from OCaml values to allow OCaml
    programs to construct parts of AST (for
    instance, to apply a transformation to the AST). *)

(** A {!type:source_location} can either be an internal
    {!type:cxsourcelocation} from libclang or a
    {!type:concrete_location}.
    {!type:concrete_location} can be obtained from
    {!type:source_location} with {!val:Clang.Ast.get_presumed_location}
    or {!val:Clang.Ast.get_expansion_location}: these functions are
    identical for {!type:source_location} constructed from
    {!type:concrete_location}, and call {!val:Clang.get_presumed_location}
    and {!val:Clang.get_expansion_location} for libclang's locations.
 *)

type source_location =
  | Clang of cxsourcelocation
  | Concrete of concrete_location

(*{[
[@@@ocaml.warning "-30"]
[@@@ocaml.warning "-9"]
[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]

open Stdcompat

let () =
  prerr_endline (Clang.get_clang_version ())

let success_count = ref 0 and failure_count = ref 0

let check pp parser source checker =
  prerr_endline source;
  let ast = parser source in
  try
    checker ast;
    incr success_count
  with e ->
    Printf.eprintf "failed with: %s\n" (Printexc.to_string e);
    Format.eprintf "@[parsed@ as:@ @[%a@]@]@."
      (Format.pp_print_list pp) ast;
    incr failure_count

let quote_decl = Refl.Lift.Exp.lift [%refl: Clang.Ast.decl] []

let quote_decl_list = Refl.Lift.Exp.lift [%refl: Clang.Ast.decl list] []

let quote_stmt_list = Refl.Lift.Exp.lift [%refl: Clang.Ast.stmt list] []

let quote_expr = Refl.Lift.Exp.lift [%refl: Clang.Ast.expr] []

let quote_type_loc = Refl.Lift.Exp.lift [%refl: Clang.Ast.type_loc] []

let pp_decl = Refl.pp [%refl: Clang.Ast.decl] []

let pp_decl_list = Refl.pp [%refl: Clang.Ast.decl list] []

let pp_stmt = Refl.pp [%refl: Clang.Ast.stmt] []

let pp_stmt_list = Refl.pp [%refl: Clang.Ast.stmt list] []

let pp_expr = Refl.pp [%refl: Clang.Ast.expr] []

let pp_type_loc = Refl.pp [%refl: Clang.Ast.type_loc] []

let check_result ?(result = fun (_ : 'b) -> ()) value =
  if
    match value with
    | Ok bindings ->
        begin try
          result bindings;
          true
        with e ->
          Printf.eprintf "failed with: %s\n" (Printexc.to_string e);
          false
        end
    | Error failure ->
        Format.printf "@[failed:@ %a@]@." Pattern.format_failure
          failure;
        false
  then
    incr success_count
  else
    incr failure_count

let check_pattern ?result quoter (parser : string -> 'a)
      (source : string) (pattern : ('a, 'b) Pattern.matcher) =
  prerr_endline source;
  let ast = parser source in
  check_result ?result (pattern ~quoted:(quoter ast) ast)
]}*)

(** {2 Aliases} *)

(** The following aliases provide more readable names for some types
from libclang. *)

type elaborated_type_keyword = clang_ext_elaboratedtypekeyword
(** Keyword associated to an elaborated type: [struct], [union],
    [enum], ... *)

and character_kind = clang_ext_stringkind
(** Character kind: ASCII, UTF8, UTF16, ... *)

and unary_expr_kind = clang_ext_unaryexpr
(** Kind of unary expression: [sizeof], [alignof], ... *)

and unary_operator_kind = clang_ext_unaryoperatorkind
(** Kind of unary operator: [_++], [++_], [-_], [&_], ... *)

and binary_operator_kind = clang_ext_binaryoperatorkind
(** Kind of binary operator: [_+_], [_=_], [_+=_], [_<<_], ... *)

and attribute_kind = clang_ext_attrkind
(** Kind of attribute: [FallThrough], [NonNull], ... *)

and builtin_type = cxtypekind
(** libclang's type kinds: [Int], [Void], [Bool], ... *)

and cxx_access_specifier = cx_cxxaccessspecifier
(** C++ access specifier: [public], [private], [protected] *)

and calling_conv = cxcallingconv
(** Calling convention *)

and linkage_kind = cxlinkagekind

and predefined_identifier_kind = clang_ext_predefinedexpr_identkind

and lambda_capture_default = clang_ext_lambdacapturedefault

and lambda_capture_kind = clang_ext_lambdacapturekind

and overloaded_operator_kind = clang_ext_overloadedoperatorkind

and string_kind = clang_ext_stringkind

and exception_specification_type = clang_ext_exceptionspecificationtype

and integer_literal =
  | Int of int
  | CXInt of (cxint [@opaque])

and floating_literal =
  | Float of float
  | CXFloat of (cxfloat [@opaque])

and languages = {
    c : bool;
    cxx : bool;
  }

and asm_compiler_extension = GCC | MS

and opaque_cxtype = cxtype [@opaque]

and opaque_type_loc = clang_ext_typeloc option [@opaque]
      [@@deriving refl]

module type NodeS = sig
  type 'a t

  val t__variable_positive0 : 'a -> 'b -> 'a
  val t__variable_negative0 : 'a -> 'b -> 'b
  val t__variable_direct0 : 'a -> 'b -> 'a

  type ('present, 'unknown) t__variable_positive0 = 'present
  type ('present, 'unknown) t__variable_negative0 = 'unknown
  type ('present, 'unknown) t__variable_direct0 = 'present

  type t__arity = [ `Succ of [ `Zero ] ]
  type t__structure
  type t__rec_group = (t__arity * t__structure) ref
  type t__kinds = [ `Lazy | `Variable ]
  type 'a0 t__gadt = unit

  type _ Refl.refl +=
    | Refl_t: 'a0 t Refl.refl

  val t_refl :
      ('a0 t, t__structure, ('a0 * unit), t__rec_group, [> t__kinds ],
       (([ `Present ], [ `Absent ]) t__variable_positive0 * unit),
       (([ `Present ], [ `Absent ]) t__variable_negative0 * unit),
       (([ `Present ], [ `Absent ]) t__variable_direct0 * unit),
       'a0 t__gadt) Refl.desc

  val from_fun : (unit -> 'a) -> 'a t

  val from_val : 'a -> 'a t

  val force : 'a t -> 'a
end

module IdNode = struct
  module Sub = struct
    type 'a t = 'a [@@deriving refl]
  end

  include (Sub : sig
    type 'a t = 'a [@@deriving refl]
  end with type t__kinds := Sub.t__kinds)

  type t__kinds = [ `Lazy | `Variable ]

  let from_fun f = f ()

  let from_val = Fun.id

  let force = Fun.id
end

module LazyNode = struct
  type 'a t = 'a Lazy.t [@@deriving refl]

  let from_fun = Lazy.from_fun

  let from_val = Lazy.from_val

  let force = Lazy.force
end
end

include Common

(** {2 Nodes and decorations} *)

(** AST nodes are of type ['a ]{!type:node} for some ['a] and
    carry a {!type:decoration}.
    If the node comes for a translation unit parsed by clang,
    the decoration is of the form {!const:Cursor}[ cursor],
    where [cursor] points to the corresponding node in clang
    internal AST.
    Decorations
    can be of the form {!const:Custom}[ custom_decoration],
    where the inlined record [custom_decoration] may optionnally
    carry a location, or a type, or both.

    To break type recursion between {!type:qual_type} and {!type:decoration},
    open types ['qual_type ]{!type:open_decoration} and
    [('a, 'qual_type) ]{!type:open_node} are defined first, and then
    {!type:node} and {!type:decoration} are defined as aliases
    with ['qual_type = ]{!type:qual_type}.

    Breaking recursion allows [visitors] to derive polymorphic
    visitors for [open_node] while deriving monomorphic visitors
    for the concrete AST nodes themselves.
*)

module Custom (Node : NodeS) = struct
type decoration =
  | Cursor of (cxcursor [@opaque])
  | Custom of {
      location : (source_location option [@opaque]);
      qual_type : qual_type option;
    }

and opaque_decoration = decoration [@opaque]

and 'a node = {
    decoration : opaque_decoration;
    desc : 'a Node.t;
  }

(** {2 Types and nodes} *)

(**
The following example declares the function [parse_declaration_list]
that returns the AST obtained from the parsing of [source] string as a
declaration list:
this function is used in the following examples to check the AST of
various programs.
    {[
let parse_declaration_list ?filename ?(command_line_args = []) ?language
    ?standard ?options ?clang_options source =
  let command_line_args =
    match language with
    | Some language -> Clang.Command_line.language language :: command_line_args
    | None -> command_line_args in
  let command_line_args =
    match standard with
    | Some standard -> Clang.Command_line.standard standard :: command_line_args
    | None -> command_line_args in
  let ast =
    Clang.Ast.parse_string ?filename ~command_line_args ?options
      ?clang_options source in
  Format.eprintf "%a@."
   (Clang.Ast.format_diagnostics Clang.not_ignored_diagnostics) ast;
  assert (not (Clang.Ast.has_severity Clang.error ast));
  ast.desc.items

let parse_declaration_list_last ?filename ?command_line_args ?language ?options
    ?clang_options source =
  List.hd (List.rev (parse_declaration_list ?filename ?command_line_args
    ?language ?options ?clang_options source))
   ]}*)

(** {3 Qualified types } *)

and qual_type = {
    cxtype : opaque_cxtype;
    type_loc : opaque_type_loc;
    const : bool;
(** [true] if the type is const-qualified.
      {[
let example = "const int one = 1;"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "one";
      var_type = {
        const = true;
        desc = BuiltinType Int};
      var_init = Some { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = "int x;"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x";
      var_type = {
        const = false;
        desc = BuiltinType Int};
      var_init = None }}] -> ()
  | _ -> assert false
     ]}*)
    volatile : bool;
(** [true] if the type is volatile-qualified.
    {[
let example = "volatile int x;"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x";
      var_type = {
        volatile = true;
        desc = BuiltinType Int}}}] -> ()
  | _ -> assert false

let example = "int x;"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x";
      var_type = {
        volatile = false;
        desc = BuiltinType Int}}}] -> ()
  | _ -> assert false
    ]}*)
    restrict : bool;
(** [true] if the type is restrict-qualified.
    {[
let example = "int * restrict x;"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x"; var_type = {
      restrict = true;
      desc = Pointer { desc = BuiltinType Int }}}}] -> ()
  | _ -> assert false

let example = "int * x;"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "x"; var_type = {
      restrict = false;
      desc = Pointer { desc = BuiltinType Int }}}}] -> ()
  | _ -> assert false
    ]}*)
    desc : type_desc Node.t;
  }

and type_desc =
  | Pointer of qual_type
(** Pointer.
    {[
let example = "char *s;"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Var { var_name = "s"; var_type = { desc =
      Pointer { desc = BuiltinType Char_S }}}}] -> ()
  | _ -> assert false
    ]}*)
  | LValueReference of qual_type
  | RValueReference of qual_type
  | ConstantArray of {
      element : qual_type;
      size : int;
      size_as_expr : expr option;
    }
(** Constant-sized array.

    {[
let example = "char s[21 * 2];"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Var { var_name = "s"; var_type = { desc = ConstantArray {
      element = { desc = BuiltinType Char_S };
      size = 42;
      size_as_expr = Some { desc = BinaryOperator {
        lhs = { desc = IntegerLiteral (Int 21)};
        kind = Mul;
        rhs = { desc = IntegerLiteral (Int 2)}}}}}}}]]

let example = "void f(char s[21 * 2]);"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Function { function_type = {
      parameters = Some { non_variadic = [{ desc = {
        qual_type = { desc = ConstantArray {
          element = { desc = BuiltinType Char_S };
          size = 42;
          size_as_expr = Some { desc = BinaryOperator {
            lhs = { desc = IntegerLiteral (Int 21)};
            kind = Mul;
            rhs = { desc = IntegerLiteral (Int 2)}}}}};
        name = "s";
        default = None; }}] }}}}]]
    ]}*)
  | Vector of {
      element : qual_type;
      size : int;
    }
(** Vector.
    {[
let example = {|
  #include <stdint.h>
  int32_t __attribute__((vector_size(16))) v;
    |}

let () =
  check_pattern quote_decl parse_declaration_list_last example
  [%pattern?
    { desc = Var {
        var_name = "v";
        var_type = { desc = Vector {
          element = { desc = Typedef ({ name = IdentifierName "int32_t" })};
          size = 4 }};
        attributes = [] }}]
    ]}*)
  | IncompleteArray of qual_type
(** Incomplete array.
    {[
let example = "struct s { int i; char array[]; };"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl { keyword = Struct; name = "s"; fields = [
      { desc = Field { name = "i"; qual_type = { desc = BuiltinType Int}}};
      { desc = Field { name = "array"; qual_type = { desc =
        IncompleteArray { desc = BuiltinType Char_S }}}}] }}] -> ()
  | _ -> assert false
    ]}*)
  | VariableArray of {
      element : qual_type;
      size : expr;
    }
  (** Variable array.
    {[
let example = "void f(int i, char array[i]);"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function { name = IdentifierName "f"; function_type =
      { result = { desc = BuiltinType Void };
        parameters = Some {
          non_variadic = [
            { desc = { name = "i"; qual_type = { desc = BuiltinType Int }}};
            { desc = { name = "array"; qual_type = { desc = VariableArray {
               element = { desc = BuiltinType Char_S };
               size = { desc = DeclRef ({ name = IdentifierName "i" }) }}}}}];
          variadic = false }}}}] -> ()
  | _ -> assert false
    ]}*)
  | Elaborated of {
      keyword : elaborated_type_keyword;
      nested_name_specifier : nested_name_specifier option; (** C++ *)
      named_type : qual_type;
    }
(** Elaborated type.
    {[
let example = "enum example { A, B, C }; enum example e;"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = EnumDecl _ };
     { desc = Var { var_name = "e"; var_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { desc = Enum ({ name = IdentifierName "example" }) }}}}}]]
    ]}*)
  | Enum of ident_ref
(** Enum type.
    {[
let example = "enum { A, B, C } e;"

let test quoter (decl : 'a) (matcher : ('a, 'b) Pattern.matcher) =
  matcher ~quoted:(quoter decl) decl

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = EnumDecl _ };
     { desc = Var { var_name = "e"; var_type = { desc = Elaborated {
      keyword = Enum;
      named_type = { desc = Enum ({ name = IdentifierName "" }) }
        as named_type }}}}]]
  ~result:begin fun bindings ->
      let decl = Clang.Type.get_declaration bindings#named_type in
      let values =
        match
          test quote_decl decl [%pattern? { desc = EnumDecl { constants }}]
        with
        | Ok bindings ->
            bindings#constants |> List.map @@
            fun (constant : Clang.Ast.enum_constant) ->
              constant.desc.constant_name,
              Clang.Enum_constant.get_value constant
        | Error failure ->
            Format.printf "@[failed:@ %a@]@." Pattern.format_failure
              failure;
            assert false in
      assert (values = ["A", 0; "B", 1; "C", 2])
  end
    ]}*)
  | FunctionType of function_type
(** Function type.

    {[
let example = "int (*p)(void);"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Var { var_name = "p"; var_type = { desc =
      Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some { non_variadic = []; variadic = false}}}}}}]]

let example = "int (*p)(int x);"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Var { var_name = "p"; var_type = { desc =
      Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some { non_variadic = [{ desc = {
          name = "x"; qual_type = { desc = BuiltinType Int }}}] }}}}}}]]
    ]}*)
  | Record of ident_ref
(** Record type (either struct or union).

    The argument is the name and is the empty string for anonymous struct or
    union.
    {[
let example = "struct { int i; float f; } s;"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = RecordDecl { keyword = Struct }};
     { desc = Var { var_name = "s"; var_type = { desc = Elaborated {
      keyword = Struct;
      named_type = { desc = Record ({ name = IdentifierName "" }) }
        as named_type }}}}]]
  ~result:begin fun bindings ->
    let fields = bindings#named_type |> Clang.Type.list_of_fields in
    match test quote_decl_list fields [%pattern?
      [ { desc = Field {
            name = "i";
            qual_type = { desc = BuiltinType Int }}};
        { desc = Field {
            name = "f";
            qual_type = { desc = BuiltinType Float }}}]] with
    | Ok () -> ()
    | Error failure ->
        Format.printf "@[failed:@ %a@]@." Pattern.format_failure
          failure;
        assert false
  end

let example = "union { int i; float f; } u;"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = RecordDecl { keyword = Union }};
     { desc = Var { var_name = "u"; var_type = { desc = Elaborated {
      keyword = Union;
      named_type = { desc = Record ({ name = IdentifierName "" })}
        as named_type }}}}]]
  ~result:begin fun bindings ->
    let fields = bindings#named_type |> Clang.Type.list_of_fields in
    match test quote_decl_list fields [%pattern?
      [ { desc = Field {
            name = "i";
            qual_type = { desc = BuiltinType Int }}};
        { desc = Field {
            name = "f";
            qual_type = { desc = BuiltinType Float }}}]] with
    | Ok () -> ()
    | Error failure ->
        Format.printf "@[failed:@ %a@]@." Pattern.format_failure
          failure;
        assert false
  end
    ]}*)
  | Typedef of ident_ref
(** Typedef type.
    {[
let example = "typedef struct { int i; float f; } struct_t; struct_t s;"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = RecordDecl { keyword = Struct }}; { desc = TypedefDecl _ };
     { desc = Var { var_name = "s";
       var_type = { desc =
         Typedef ({ name = IdentifierName "struct_t" }) } as var_type }}]]
  ~result:begin fun bindings ->
    let fields = bindings#var_type |>
      Clang.Type.get_typedef_underlying_type |>
      Clang.Type.list_of_fields in
    match test quote_decl_list fields [%pattern?
      [ { desc = Field {
            name = "i";
            qual_type = { desc = BuiltinType Int }}};
        { desc = Field {
            name = "f";
            qual_type = { desc = BuiltinType Float }}}]] with
    | Ok () -> ()
    | Error failure ->
        Format.printf "@[failed:@ %a@]@." Pattern.format_failure
          failure;
        assert false
  end
    ]}*)
  | Complex of qual_type
(** Complex number type (C99).

    {[
let example = "double _Complex c;"

let () =
  check pp_decl parse_declaration_list example @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { var_name = "c";
      var_type = { desc = Complex { desc = BuiltinType Double }}}} -> ()
  | _ -> assert false

let example = "float _Complex c;"

let () =
  check pp_decl parse_declaration_list example @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { var_name = "c";
      var_type = { desc = Complex { desc = BuiltinType Float }}}} -> ()
  | _ -> assert false
    ]} *)
  | Attributed of {
      modified_type : qual_type;
      attribute : attribute;
    }
(** Attributed type.

    {[
let example = "int * _Nonnull ptr;"

[%%meta if Clangml_config.version.major >= 8 then [%stri
    let () =
      check_pattern quote_decl_list parse_declaration_list
        example [%pattern?
        [{ desc = Var { var_name = "ptr";
           var_type = { desc = Attributed {
             modified_type = { desc = Pointer { desc = BuiltinType Int }};
             attribute = { desc = Other TypeNonNull }}}}}]]]
  else Metapp.Stri.of_list []]

let example = "void * __ptr32 p;"

[%%meta if Clangml_config.version.major >= 8 then [%stri
    let () =
      check_pattern quote_decl_list
        (parse_declaration_list ~command_line_args:["-fms-extensions"])
        example [%pattern?
        [{ desc = Var { var_name = "p";
           var_type = { desc = Attributed {
             modified_type = { desc = Pointer { desc = BuiltinType Void }};
             attribute = { desc = Other Ptr32 }}}}}]]]
  else Metapp.Stri.of_list []]
    ]} *)
  | ParenType of qual_type
(** Parenthesized type.

    Parenthesized type only occurs when [ignore_paren_in_types = false] option
    is passed to the AST converting function.

    {[
let example = "int f(char(*)[]);"

let () =
  check_pattern quote_decl_list (parse_declaration_list
    ~options:{ Clang.Ast.Options.default with ignore_paren_in_types = false })
    example
  [%pattern?
    [{ desc = Function {
      function_type = {
        result = { desc = BuiltinType Int };
        parameters = Some {
          non_variadic = [
            { desc = { qual_type = { desc = Pointer { desc = ParenType
              { desc = IncompleteArray { desc = BuiltinType Char_S }}}}}}] }};
      name = IdentifierName "f";
      body = None }}]]

let example = "int (*p)(void);"

let () =
  check_pattern quote_decl_list (parse_declaration_list
    ~options:{ Clang.Ast.Options.default with ignore_paren_in_types = false })
    example
  [%pattern?
    [{ desc = Var { var_name = "p"; var_type = { desc =
      Pointer { desc = ParenType { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some { non_variadic = []; variadic = false}}}}}}}]]
    ]}*)
  | TemplateTypeParm of string
  | SubstTemplateTypeParm of string
  | TemplateSpecialization of {
      name : template_name;
      args : template_argument list;
    }
  | BuiltinType of builtin_type
(** Built-in type.
    {[
let example = "_Bool s;"

let () =
  check pp_decl parse_declaration_list example @@ fun ast ->
  match ast |> List.rev |> List.hd with
  | { desc = Var { var_name = "s";
      var_type = { desc = BuiltinType Bool}}} -> ()
  | _ -> assert false
    ]}*)
  | Auto
(** Auto type. (C++11)
    {[
let example = "auto i = 1;"

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Var {
      var_name = "i";
      var_type = { desc = Auto };
      var_init = Some { desc = IntegerLiteral (Int 1)}}}]]
    ]}*)
  | PackExpansion of qual_type
  | MemberPointer of {
      pointee : qual_type;
      class_ : qual_type;
    }
  | Decltype of expr
(** decltype(expression). (C++11)

    {[
let example = "decltype(1) i = 1;"

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11)
    example
  [%pattern?
    [{ desc = Var {
      var_name = "i";
      var_type = { desc = Decltype { desc = IntegerLiteral (Int 1)}};
      var_init = Some { desc = IntegerLiteral (Int 1)}}}]]
    ]}*)
  | InjectedClassName of qual_type
  | UnexposedType of clang_ext_typekind
  | InvalidType

and template_name =
  | NameTemplate of string
  | OverloadedTemplate
  | QualifiedTemplate
  | DependentTemplate
  | SubstTemplateTemplateParm
  | SubstTemplateTemplateParmPack
  | InvalidNameKind

and template_argument =
  | Type of qual_type
  | ArgumentDecl of decl
  | NullPtr of qual_type
  | TemplateTemplateArgument of template_name
  | TemplateExpansion of template_name
  | Integral of { value : integer_literal; qual_type : qual_type }
  | NonTypeTemplateArgument of qual_type
  | ExprTemplateArgument of expr
  | Pack of template_argument list

and requirement =
  | Type of qual_type
  | Simple of expr_requirement
  | Compound of expr_requirement
  | Nested of expr

and expr_requirement = {
  expr : expr;
  return_type_type_constraint : type_constraint option;
}

and type_constraint = {
  parameters : template_parameter_list;
  type_constraint : expr;
}

and nested_name_specifier = nested_name_specifier_component list

and nested_name_specifier_component =
  | Global
  | NestedIdentifier of string
  | NamespaceName of string
  | NamespaceAliasName of string
  | TypeSpec of qual_type
  | TypeSpecWithTemplate of qual_type

and declaration_name =
  | IdentifierName of string
  | ConstructorName of qual_type
  | DestructorName of qual_type
  | ConversionFunctionName of qual_type
  | DeductionGuideName of decl
  | OperatorName of overloaded_operator_kind
  | LiteralOperatorName of string
  | UsingDirectiveName

and record_decl = {
    keyword : elaborated_type_keyword;
    attributes : attribute list;
    nested_name_specifier : nested_name_specifier option; (** C++ *)
    name : string;
    bases : base_specifier list; (** C++ *)
    fields : decl list;
    final : bool; (** C++11 *)
    complete_definition : bool;
  }

and function_decl = {
    linkage : linkage_kind;
    function_type : function_type;
    nested_name_specifier : nested_name_specifier option; (** C++ *)
    name : declaration_name;
    body : stmt option;
    deleted : bool; (** C++ *)
    constexpr : bool; (** C++ *)
    inline_specified : bool;
(** Determine whether the [inline] keyword was specified for this function.

    {[
let example = {|inline void f(void);|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~standard:C11) example
  [%pattern?
    [{ desc = Function {
      name = IdentifierName "f";
      inline_specified = true;
      inlined = true; }}]]
    ]}*)
    inlined : bool;
(** Determine whether this function should be inlined, because it is
    either marked [inline] or [constexpr] or is a member function of a class
    that was defined in the class body.
    {[
let example = {|constexpr void f(void);|}

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11) example
  [%pattern?
    [{ desc = Function {
      name = IdentifierName "f";
      constexpr = true;
      inline_specified = false;
      inlined = true; }}]]
    ]}*)
    attributes : attribute list;
  }

(** Function type. *)
and function_type = {
  calling_conv : calling_conv;
(** Calling convention.
    {[
let example = "void f(void);"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { calling_conv = C }}}] -> ()
  | _ -> assert false

let example = {| __attribute((pcs("aapcs"))) void f(void); |}

let () =
  check pp_decl
    (parse_declaration_list
       ~command_line_args:["--target=arm"; "-mfloat-abi=soft"])
    example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { calling_conv = AAPCS };
      attributes = []; }}] ->
      assert (
        Clang.version () < { major = 3; minor = 8; subminor = 0 } ||
        Clang.version () >= { major = 3; minor = 9; subminor = 0 })
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { calling_conv = C };
      attributes = []; }}] ->
      assert (
        Clang.version () >= { major = 3; minor = 8; subminor = 0 } &&
        Clang.version () < { major = 3; minor = 9; subminor = 0 })
  | _ -> assert false
    ]}
 *)
  result : qual_type;
(** Result type.
    {[
let example = "void f(void);"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { result = { desc = BuiltinType Void }}}}] -> ()
  | _ -> assert false

let example = "f(void);"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { result = { desc = BuiltinType Int }}}}] -> ()
  | _ -> assert false
    ]}*)

  parameters : parameters option;
(** Parameter types. [None] for K&R-style 'int foo()' function.
    {[
let example = "void f(void);"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { parameters = Some {
        non_variadic = [];
        variadic = false }}}}] -> ()
  | _ -> assert false

let example = "void f();"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { parameters = None }}}] -> ()
  | _ -> assert false

    ]}*)

  exception_spec : exception_spec option; (** C++ *)
(**
  Exception specification (C++).

    {[
let example = "void f() noexcept;"

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11)
    example
  [%pattern?
    [{ desc = Function {
      name = IdentifierName "f";
      function_type = { exception_spec = Some (Noexcept {
        expr = None;
        evaluated = None; })}}}]]

let example = "void f() noexcept(true);"

let () =
  if Clang.version () >= { major = 7; minor = 0; subminor = 0 } then
    begin
      check_pattern quote_decl_list
        (parse_declaration_list ~language:CXX ~standard:Cxx11)
        example
      [%pattern?
        [{ desc = Function {
          name = IdentifierName "f";
          function_type = { exception_spec = Some (Noexcept {
            expr = Some { desc = BoolLiteral true };
            evaluated = Some true; })}}}]]
    end
  else
    begin
      check_pattern quote_decl_list
        (parse_declaration_list ~language:CXX ~standard:Cxx11)
        example
      [%pattern?
        [{ desc = Function {
          name = IdentifierName "f";
          function_type = { exception_spec = Some (Noexcept {
            expr = Some { desc = BoolLiteral true };
            evaluated = None; })}}}]]
    end

let example = "void f() noexcept(false);"

let () =
  if Clang.version () >= { major = 7; minor = 0; subminor = 0 } then
    begin
      check_pattern quote_decl_list
        (parse_declaration_list ~language:CXX ~standard:Cxx11)
        example
      [%pattern?
        [{ desc = Function {
          name = IdentifierName "f";
          function_type = { exception_spec = Some (Noexcept {
            expr = Some { desc = BoolLiteral false };
            evaluated = Some false; })}}}]]
    end
  else
    begin
      check_pattern quote_decl_list
        (parse_declaration_list ~language:CXX ~standard:Cxx11)
        example
      [%pattern?
        [{ desc = Function {
          name = IdentifierName "f";
          function_type = { exception_spec = Some (Noexcept {
            expr = Some { desc = BoolLiteral false };
            evaluated = None; })}}}]]
    end

let example = {|
  template <bool b> void f() noexcept(b);
|}

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11)
    example
  [%pattern? [
    { desc = TemplateDecl {
      parameters = { list = [{ desc = {
        parameter_name = "b";
        parameter_kind = NonType { parameter_type = {
          desc = BuiltinType Bool }}}}] };
      decl = { desc = Function {
        name = IdentifierName "f";
        function_type = { exception_spec = Some (Noexcept {
          expr = Some { desc = DeclRef ({ name = IdentifierName "b" })};
          evaluated = None; })}}}}}]]

let example = "void f() throw();"

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11)
    example
  [%pattern?
    [{ desc = Function {
      name = IdentifierName "f";
      function_type = { exception_spec = Some (Throw [])}}}]]

let example = "void f() throw(int);"

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11)
    example
  [%pattern?
    [{ desc = Function {
      name = IdentifierName "f";
      function_type = { exception_spec = Some (Throw [
        { desc = BuiltinType Int }])}}}]]

let example = "void f() throw(...);"

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX)
    example
  [%pattern?
    [{ desc = Function {
      name = IdentifierName "f";
      function_type = { exception_spec = Some (Other MSAny)}}}]]
    ]}*)

    ref_qualifier : cxrefqualifierkind; (** C++ *)
(** Method ref-qualifier.
    {[
let example = {|
template <typename T>
class C
{
  T        no_ref_qualifier();
  T&       value() &;
  T&&      value() &&;
  T const& value() const&;
};
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = TemplateDecl {
         parameters = { list = [{ desc = {
           parameter_name = "T";
           parameter_kind = Class { default = _ }}}] };
         decl = { desc = RecordDecl {
           keyword = Class;
           name = "C";
           fields = [
             { desc = CXXMethod {
                 function_decl = {
                   function_type = {
                     result = { desc = TemplateTypeParm "T" };
                     parameters = Some { non_variadic = [] };
                     ref_qualifier = None };
                   name = IdentifierName "no_ref_qualifier";
                   body = None }}};
             { desc = CXXMethod {
                 const = false;
                 function_decl = {
                   function_type = {
                     result = { desc =
                       LValueReference { desc = TemplateTypeParm "T" };
                       const = false };
                     parameters = Some { non_variadic = [] };
                     ref_qualifier = LValue };
                   name = IdentifierName "value";
                   body = None }}};
             { desc = CXXMethod {
                 function_decl = {
                   function_type = {
                     result = { desc =
                       RValueReference { desc = TemplateTypeParm "T" }};
                     parameters = Some { non_variadic = [] };
                     ref_qualifier = RValue };
                   name = IdentifierName "value";
                   body = None }}};
             { desc = CXXMethod {
                 const = true;
                 function_decl = {
                   function_type = {
                     result = { desc = LValueReference {
                       desc = TemplateTypeParm "T";
                       const = true; }};
                     parameters = Some { non_variadic = [] };
                     ref_qualifier = LValue };
                   name = IdentifierName "value";
                   body = None }}}] }}}}]]
    ]}*)
}

and exception_spec =
  | Noexcept of {
      expr : expr option;
      evaluated : bool option; (** Clang >=7.0.0 only *)
    }
  | Throw of qual_type list
  | Other of exception_specification_type

(** Function parameters. *)
and parameters = {
  non_variadic : parameter list;
(** Non-variadic parameters: the list gives for each argument its name and its
    type.

    For a function type which is not attached to an actual function declaration,
    all arguments have the empty name ([""]) and no default value ([None]),
    since Clang does not keep argument names in function types.
    {[
let example = "void f(int i);"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { parameters = Some {
        non_variadic = [{ desc = {
          name = "i";
          qual_type = { desc = BuiltinType Int };
          default = None }}];
        variadic = false }}}}] -> ()
  | _ -> assert false

let example = "void f(int);"

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { parameters = Some {
        non_variadic = [{ desc = {
          name = "";
          qual_type = { desc = BuiltinType Int };
          default = None }}];
        variadic = false }}}}] -> ()
  | _ -> assert false

let example = "typedef void (*f)(int x);"

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = TypedefDecl {
      name = "f";
      underlying_type = { desc =
        Pointer { desc = FunctionType { parameters = Some {
          non_variadic = [{ desc = {
            name = "";
            qual_type = { desc = BuiltinType Int };
            default = None }}];
          variadic = false }}}}}}] -> ()
  | _ -> assert false

let example = {| void f(int i = 1) {} |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { parameters = Some {
        non_variadic = [{ desc = {
          name = "i";
          qual_type = { desc = BuiltinType Int };
          default = Some { desc = IntegerLiteral (Int 1) }}}];
        variadic = false }}}}] -> ()
  | _ -> assert false
    ]}
 *)
  variadic : bool;
(** True if the function type is variadic.
    {[
let example = "void f(int i, ...);"

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Function {
      name = IdentifierName "f";
      function_type = { parameters = Some {
        non_variadic = [
          { desc = { name = "i"; qual_type = { desc = BuiltinType Int }}}];
        variadic = true }}}}] -> ()
  | decls ->
      List.iter (fun decl -> Format.eprintf "%a@." pp_decl decl)
        decls;
      assert false
    ]}
 *)
}

and parameter = parameter_desc node

and parameter_desc = {
  qual_type : qual_type;
  name : string;
  default : expr option; (** C++ *)
}
(** Function or method parameter. *)

(** {3 Statements}

The following example declares the function [parse_statement_list]
that returns the AST obtained from the parsing of [source] string as a
statement list (by putting it in the context of a function):
this function is used in the following examples to check pp_decl
the AST of various types.
    {[
let parse_statement_list ?(return_type = "void") ?filename ?command_line_args
    ?language ?standard ?options source =
  match
    Printf.sprintf "%s f(void) { %s }" return_type source |>
    parse_declaration_list ?filename ?command_line_args ?language ?standard
      ?options
  with
  | [{ desc = Function { body = Some { desc = Compound items }}}] -> items
  | decls ->
      List.iter (fun decl ->
        Format.eprintf "%a@." pp_decl decl) decls;
      assert false
    ]}*)

and stmt = stmt_desc node

and stmt_desc =
  | Null
(** Null statement.
    {[
let example = ";"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Null }] -> ()
  | _ -> assert false
    ]}
    *)
  | Compound of stmt list
(** Compound statement.
    {[
let example = "{}"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Compound [] }] -> ()
  | _ -> assert false

let example = "{;;}"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Compound [{ desc = Null }; { desc = Null }] }] -> ()
  | _ -> assert false
    ]}
    *)
  | For of {
      init : stmt option;
      condition_variable : var_decl option; (** C++ *)
      cond : expr option;
      inc : stmt option;
      body : stmt;
    }
(** For statement.
    {[
let example = "for (;;) {}"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = For {
      init = None;
      condition_variable = None;
      cond = None;
      inc = None;
      body = { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = "int i; for (i = 0; i < 4; i++) { }"

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = For {
      init = Some { desc = Expr { desc = BinaryOperator {
        lhs = { desc = DeclRef ({ name = IdentifierName "i" })};
        kind = Assign;
        rhs = { desc = IntegerLiteral (Int 0)}}}};
      condition_variable = None;
      cond = Some { desc = BinaryOperator {
        lhs = { desc = DeclRef ({ name = IdentifierName "i" })};
        kind = LT;
        rhs = { desc = IntegerLiteral (Int 4)}}};
      inc = Some { desc = Expr { desc = UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef ({ name = IdentifierName "i" })}}}};
      body = { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = "for (int i = 0; i < 4; i++) { }"

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        var_name = "i";
        var_type = { desc = BuiltinType Int};
        var_init = Some { desc = IntegerLiteral (Int 0)}}}] };
      condition_variable = None;
      cond = Some { desc = BinaryOperator {
        lhs = { desc = DeclRef ({ name = IdentifierName "i" })};
        kind = LT;
        rhs = { desc = IntegerLiteral (Int 4)}}};
      inc = Some { desc = Expr { desc = UnaryOperator {
        kind = PostInc;
        operand = { desc = DeclRef ({ name = IdentifierName "i" })}}}};
      body = { desc = Compound [] }}}] -> ()
  | _ -> assert false

let example = "for (int i = 0; int j = i - 1; i--) { }"

let () =
  check pp_stmt (parse_statement_list
    ~language:CXX) example @@ fun ast -> match ast with
  | [{ desc = For {
      init = Some { desc = Decl [{ desc = Var {
        var_name = "i";
        var_type = { desc = BuiltinType Int};
        var_init = Some { desc = IntegerLiteral (Int 0)}}}] };
      condition_variable = Some { desc = {
        var_name = "j";
        var_type = { desc = BuiltinType Int};
        var_init = Some { desc = BinaryOperator {
          lhs = { desc = DeclRef ({ name = IdentifierName "i" })};
          kind = Sub;
          rhs = { desc = IntegerLiteral (Int 1)}}}}};
      cond = Some { desc = DeclRef ({ name = IdentifierName "j" }) };
      inc = Some { desc = Expr { desc = UnaryOperator {
        kind = PostDec;
        operand = { desc = DeclRef ({ name = IdentifierName "i" })}}}};
      body = { desc = Compound [] }}}] -> ()
  | _ -> assert false
    ]}*)
  | ForRange of {
      var : var_decl;
      range : expr;
      body : stmt;
    }
(** For-range statement.
    {[
let example = {|
  int a[] = {1, 2};
  for (int i : a) {}
|}

let () =
  check_pattern quote_stmt_list (parse_statement_list ~language:CXX) example
  [%pattern?
    [{ desc = Decl [
       { desc = Var {
           var_name = "a";
           var_type = { desc = IncompleteArray { desc = BuiltinType Int }};
           var_init = Some { desc = InitList [
             { desc = IntegerLiteral (Int 1)};
             { desc = IntegerLiteral (Int 2)}] }}}] };
     { desc = ForRange {
         var = { desc = {
           var_name = "i";
           var_type = { desc = BuiltinType Int };
           var_init = Some { desc = UnaryOperator {
             kind = Deref;
             operand = { desc = DeclRef {
               name = IdentifierName _ }}}}}};
         range = { desc = DeclRef { name = IdentifierName "a" }};
         body = { desc = Compound [] }}}]]
    ]}*)
  | If of {
      init : stmt option; (** C++17 *)
      condition_variable : var_decl option; (** C++ *)
      cond : expr;
      then_branch : stmt;
      else_branch : stmt option;
    }
(** If statement.
    {[
let example = "if (1) { 2; } else { 3; }"

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral (Int 1)};
       then_branch = { desc = Compound [{
         desc = Expr { desc = IntegerLiteral (Int 2)}}] };
       else_branch = Some { desc = Compound [{
         desc = Expr { desc = IntegerLiteral (Int 3)}}] }}}] -> ()
  | _ -> assert false

let example = "if (1) { 2; }"

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = None;
       cond = { desc = IntegerLiteral (Int 1)};
       then_branch = { desc = Compound [{
         desc = Expr { desc = IntegerLiteral (Int 2)}}] };
       else_branch = None }}] -> ()
  | _ -> assert false

let example = "if (int i = 1) { i; }"

let () =
  check pp_stmt
    (parse_statement_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = If {
       init = None;
       condition_variable = Some ({ desc = {
         var_type = { desc = BuiltinType Int};
         var_name = "i";
         var_init = Some { desc = IntegerLiteral (Int 1) }}});
       cond = { desc = DeclRef ({ name = IdentifierName "i" })};
       then_branch = { desc = Compound [{
         desc = Expr { desc = DeclRef ({ name = IdentifierName "i" }) }}] };
       else_branch = None }}] -> ()
  | _ -> assert false

    ]}

    Init statements in [if] (C++17) are available since 3.9.0.

    {[
let example = "if (int i = 1; i) { i; }"

let () =
  if Clang.version () >= { major = 3; minor = 9; subminor = 0 } then
    check pp_stmt (parse_statement_list ~language:CXX) example
    @@ fun ast -> match ast with
    | [{ desc = If {
         init = Some { desc = Decl [{ desc = Var {
           var_name = "i";
           var_type = { desc = BuiltinType Int };
           var_init = Some { desc = IntegerLiteral (Int 1) }}}] };
         condition_variable = None;
         then_branch = { desc = Compound [{
           desc = Expr { desc = DeclRef ({ name = IdentifierName "i" }) }}] };
         else_branch = None }}] -> ()
    | _ -> assert false
   ]}*)
  | Switch of {
      init : stmt option; (** C++17 *)
      condition_variable : var_decl option; (** C++ *)
      cond : expr;
      body : stmt;
    }
(** Switch statement.
    {[
let example = "switch (1) { case 1: f(); break; case 2: break; default:;}"

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern?
    [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          body = { desc =
            Expr { desc = Call {
              callee = { desc = DeclRef ({ name = IdentifierName "f" }) };
              args = [] }}}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 2)};
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}]]

let example =
  "switch (int i = 1) { case 1: f(); break; case 2: break; default:;}"

let () =
  check_pattern quote_stmt_list (parse_statement_list ~language:CXX) example
  [%pattern?
    [{ desc = Switch {
      init = None;
      condition_variable = Some ({ desc = {
         var_type = { desc = BuiltinType Int};
         var_name = "i";
         var_init = Some { desc = IntegerLiteral (Int 1)}}});
      cond = { desc = DeclRef ({ name = IdentifierName "i" }) };
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          body = { desc =
            Expr { desc = Call {
              callee = { desc = DeclRef ({ name = IdentifierName "f" }) };
              args = [] }}}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 2)};
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}]]
    ]}


    Init statements in [if] (C++17) are available since 3.9.0.

    {[
let example =
  "switch (int i = 1; i) { case 1: f(); break; case 2: break; default:;}"

let () =
  if Clang.version () >= { major = 3; minor = 9; subminor = 0 } then
    check_pattern quote_stmt_list (parse_statement_list ~language:CXX) example
    [%pattern?
      [{ desc = Switch {
        init = Some { desc = Decl [{ desc = Var {
           var_name = "i";
           var_type = { desc = BuiltinType Int };
           var_init = Some { desc = IntegerLiteral (Int 1)}}}] };
        condition_variable = None;
        cond = { desc = DeclRef ({ name = IdentifierName "i" }) };
        body = { desc = Compound [
          { desc = Case {
            lhs = { desc = IntegerLiteral (Int 1)};
            body = { desc =
              Expr { desc = Call {
                callee = { desc = DeclRef ({ name = IdentifierName "f" }) };
                args = [] }}}}};
          { desc = Break };
          { desc = Case {
            lhs = { desc = IntegerLiteral (Int 2)};
            body = { desc = Break }}};
          { desc = Default { desc = Null }}] }}}]]
    ]}*)
  | Case of {
      lhs : expr;
      rhs : expr option; (** GNU extension: case ranges "case low ... high:" *)
      body : stmt;
    }
(** Case statement.

    Note that [body] only covers the first statement that follows. Other
    statements are attached to the parent.
    {[
let example = "switch (1) { case 1: f(); break; case 2 ... 3: break; default:;}"

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern?
    [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          rhs = None;
          body = { desc =
            Expr { desc = Call {
              callee = { desc = DeclRef ({ name = IdentifierName "f" }) };
              args = [] }}}}};
        { desc = Break };
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 2)};
          rhs = Some { desc = IntegerLiteral (Int 3)};
          body = { desc = Break }}};
        { desc = Default { desc = Null }}] }}}]]

let example = "switch (1) { case 1: case 2: case 3: default: ;}"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Switch {
      init = None;
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Compound [
        { desc = Case {
          lhs = { desc = IntegerLiteral (Int 1)};
          rhs = None;
          body = { desc = Case {
            lhs = { desc = IntegerLiteral (Int 2)};
            rhs = None;
            body = { desc = Case {
              lhs = { desc = IntegerLiteral (Int 3)};
              rhs = None;
              body = { desc = Default { desc = Null }}}}}}}}] }}}] -> ()
  | _ -> assert false

    ]}*)
  | Default of stmt
(** Default statement.

    Note that the parameter only covers the first statement that follows. Other
    statements are attached to the parent.
 *)
  | While of {
      condition_variable : var_decl option; (** C++ *)
      cond : expr;
      body : stmt;
    }
(** While statement.
    {[
let example = "while (1);"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = While {
      condition_variable = None;
      cond = { desc = IntegerLiteral (Int 1)};
      body = { desc = Null }}}] -> ()
  | _ -> assert false

let example = "while (int i = 1) { i; }"

let () =
  check pp_stmt
    (parse_statement_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = While {
      condition_variable = Some ({ desc = {
         var_type = { desc = BuiltinType Int};
         var_name = "i";
         var_init = Some { desc = IntegerLiteral (Int 1)}}});
      cond = { desc = DeclRef ({ name = IdentifierName "i" }) };
      body = { desc = Compound [{ desc =
        Expr { desc = DeclRef ({ name = IdentifierName "i" }) }}] }}}] -> ()
  | _ -> assert false
    ]}*)
  | Do of {
      body : stmt;
      cond : expr;
    }
(** Do statement.
    {[
let example = "do; while (1);"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Do {
      body = { desc = Null };
      cond = { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = "do { f(); } while (1);"

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern?
    [{ desc = Do {
      body = { desc = Compound [{ desc =
        Expr { desc = Call {
          callee = { desc = DeclRef ({ name = IdentifierName "f" }) };
          args = [] }}}] };
      cond = { desc = IntegerLiteral (Int 1)}}}]]
    ]}*)
  | Label of {
      label : label_ref;
      body : stmt;
    }
(** Label statement.
    {[
let example = "label: 1; 2;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = IntegerLiteral (Int 1)}}}};
      { desc = Expr { desc = IntegerLiteral (Int 2)}}] -> ()
  | _ -> assert false
    ]}*)
  | Goto of label_ref
(** Goto statement.
    {[
let example = "label: 1; goto label;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = IntegerLiteral (Int 1)}}}};
      { desc = Goto "label" }] -> ()
  | _ -> assert false
    ]}*)
  | IndirectGoto of expr
(** Indirect goto statement (Labels as Values GNU extension).
    {[
let example = "label: 1; void *ptr = &&label; goto *ptr;"

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern?
    [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = IntegerLiteral (Int 1)}}}};
      { desc = Decl [{ desc = Var {
        var_name = "ptr";
        var_type = { desc = Pointer { desc = BuiltinType Void }};
        var_init = Some { desc = AddrLabel "label" }}}] };
      { desc = IndirectGoto { desc =
          DeclRef ({ name = IdentifierName "ptr" })}}]]
    ]}*)
  | Continue
(** Continue statement.
    {[
let example = "for (;;) continue;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = For { body = { desc = Continue } }}] -> ()
  | _ -> assert false
   ]}*)
  | Break
(** Break statement.
    {[
let example = "for (;;) break;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = For { body = { desc = Break } }}] -> ()
  | _ -> assert false
   ]}*)
  | Asm of asm
(** GCC assembler statement.
    {[
let example = {|
  int src = 1;
  int dst;

  asm ("mov %1, %0\n\t"
    "add $1, %0"
    : "=r" (dst)
    : "r" (src));
|}

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern?
    [{ desc = Decl _ }; { desc = Decl _ };
     { desc = Asm {
       asm_compiler_extension = GCC;
       asm_string = "mov %1, %0\n\tadd $1, %0";
       asm_inputs =
         [{ asm_constraint = "r";
            asm_expr = { desc = DeclRef { name = IdentifierName "src"}}}];
       asm_outputs =
         [{ asm_constraint = "=r";
            asm_expr = { desc = DeclRef { name = IdentifierName "dst"}}}] }}]]
   ]}*)
  | Return of expr option
(** Return statement.
    {[
let example = "return;"

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Return None }] -> ()
  | _ -> assert false

let example = "return 1;"

let () =
  check pp_stmt (parse_statement_list ~return_type:"int")
    example @@
  fun ast -> match ast with
  | [{ desc = Return (Some { desc = IntegerLiteral (Int 1)})}] -> ()
  | _ -> assert false
   ]}*)
  | Decl of decl list
  | Expr of expr
  | Try of {
      try_block : stmt;
      handlers : catch list;
    }
  | AttributedStmt of {
      attributes : attribute list;
      sub_stmts : stmt list;
    }
  | UnknownStmt of cxcursorkind * clang_ext_stmtkind

and catch = {
  parameter : (string * qual_type) option;
  block : stmt
}

and asm = {
  asm_compiler_extension : asm_compiler_extension;
  asm_string : string;
  asm_inputs : asm_operand list;
  asm_outputs : asm_operand list;
}

and asm_operand = {
  asm_constraint : string;
  asm_expr : expr;
}

and attribute = attribute_desc node

and attribute_desc = (expr, qual_type, declaration_name) Attributes.t
(**
   [AbiTag]: ABI tags.

    {[
let example = {|
__attribute__((abi_tag("a", "b"))) float f;
|}

let () =
  if Clang.version () >= { major = 3; minor = 9; subminor = 0 } then
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Var {
         attributes = [{ desc = AbiTag ["a"; "b"] }];
         var_type = { desc = BuiltinType Float };
         var_name = "f"; }}]]
    ]}

    [| AcquireCapability of expr list]
    Marks a function as acquiring a capability.

    {[
let example = {|
int mu;

void lockAndInit() __attribute__((acquire_capability(mu)));
|}


let () =
  if Clang.version () >= { major = 3; minor = 5; subminor = 0 } then
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Var { var_name = "mu" }};
      { desc = Function {
         name = IdentifierName "lockAndInit";
         attributes = [
           { desc = AcquireCapability
               { args = [{
                   desc = DeclRef { name = IdentifierName "mu" }}] }}] }}]]
    ]}

    [Aligned]:
    Field alignment.

    {[
let example = {|
struct {
  _Alignas(32) float f;
};
|}

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = RecordDecl {
      keyword = Struct;
      fields = [
        { desc = Field
          { name = "f";
            qual_type = { desc = BuiltinType Float };
            attributes = [
              { desc = Aligned {
                alignment_expr = { desc = IntegerLiteral (Int 32) }}}] }}] }}]]

let example = {|
struct alignas(32) s {
  float f;
};
|}

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11) example
  [%pattern?
    [{ desc = RecordDecl {
      keyword = Struct;
      attributes = [
        { desc = Aligned {
          alignment_expr = {desc = IntegerLiteral (Int 32)}}}];
      fields = [
        { desc = Field
          { name = "f";
            qual_type = { desc = BuiltinType Float }}}] }}]]

    ]}

Note that whereas clang AST allows [_Alignas] attribute to carry a type
as argument, clang parser transforms [_Alignas(type)] into
[_Alignas(alignof(type))].

    {[
let example = {|
struct {
  _Alignas(double) float f;
};
|}

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = RecordDecl {
      keyword = Struct;
      fields = [
        { desc = Field
          { name = "f";
            qual_type = { desc = BuiltinType Float };
            attributes = [
              { desc = Aligned {
                alignment_expr = { desc = UnaryExpr {
                    kind = AlignOf;
                    argument = ArgumentType
                      { desc = BuiltinType Double }}}}}] }}] }}]]

let example = {|
struct alignas(double) s {
  float f;
};
|}

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11) example
  [%pattern?
    [{ desc = RecordDecl {
      keyword = Struct;
      attributes = [
        { desc = Aligned {
          alignment_expr = { desc = UnaryExpr {
                    kind = AlignOf;
                    argument = ArgumentType
                      { desc = BuiltinType Double }}}}}];
      fields = [
        { desc = Field
          { name = "f";
            qual_type = { desc = BuiltinType Float }}}] }}]]
    ]}

  [AllocAlign]:
Specify that the return value of the function (which must be a pointer type) is
at least as aligned as the value of the indicated parameter.

    {[
let example = {|
void *a(int align) __attribute__((alloc_align(1)));
|}

let () =
  if Clang.version () >= { major = 5; minor = 0; subminor = 0 } then
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11) example
  [%pattern?
    [{ desc = Function {
         attributes = [{ desc = AllocAlign 1 }];
         name = IdentifierName "a";
         function_type = {
           result = { desc = Pointer { desc = BuiltinType Void }};
           parameters = Some {
             non_variadic = [{ desc = {
               name = "align"; qual_type = { desc = BuiltinType Int }}}];
             variadic = false }}}}]]
    ]}

  [AllocSize]:
    Hint to the compiler how many bytes of memory will be available at the
    returned pointer.

    {[
let example = {|
void *my_malloc(int a) __attribute__((alloc_size(1)));
void *my_calloc(int a, int b) __attribute__((alloc_size(1, 2)));
|}

let () =
  if Clang.version () >= { major = 4; minor = 0; subminor = 0 } then
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX ~standard:Cxx11) example
  [%pattern?
    [{ desc = Function {
         attributes = [{ desc = AllocSize { elem_size = 1; num_elems = 0 }}];
         name = IdentifierName "my_malloc";
         function_type = {
           result = { desc = Pointer { desc = BuiltinType Void }};
           parameters = Some {
             non_variadic = [{ desc = {
               name = "a"; qual_type = { desc = BuiltinType Int }}}];
             variadic = false }}}};
      { desc = Function {
         attributes = [
           { desc = AllocSize { elem_size = 1; num_elems = 2 }}];
         name = IdentifierName "my_calloc";
         function_type = {
           result = { desc = Pointer { desc = BuiltinType Void }};
           parameters = Some {
             non_variadic = [
               { desc = {
                   name = "a"; qual_type = { desc = BuiltinType Int }}};
               { desc = {
                   name = "b"; qual_type = { desc = BuiltinType Int }}}];
             variadic = false }}}}]]
    ]}

  [ReleaseCapability]: Marks a function as releasing a capability.

    {[
let example = {|
int mu;

void cleanupAndUnlock()  __attribute__((release_capability(mu)));
|}


let () =
  if Clang.version () >= { major = 3; minor = 5; subminor = 0 } then
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Var { var_name = "mu" }};
      { desc = Function {
         name = IdentifierName "cleanupAndUnlock";
         attributes = [
           { desc = ReleaseCapability {
             args = [{ desc = DeclRef { name = IdentifierName "mu" }}] }}] }}]]
    ]}

  [Other]: Attributes without argument.

    {[
let example = {|
_Noreturn void f(void);
|}

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~standard:C11) example
  [%pattern?
    [{ desc = Function {
         attributes = [{ desc = Other C11NoReturn }];
         name = IdentifierName "f";
         function_type = {
           result = { desc = BuiltinType Void };
           parameters = Some { non_variadic = []; variadic = false }}}}]]
    ]}*)

(** {3 Expressions} *)

and expr = expr_desc node

and expr_desc =
  | IntegerLiteral of integer_literal
(** Integer literal.
    By default, integer literals are converted if possible into {!constr:Int}
    and integers too large to be represented as [int] are not converted.
    Integer literals can be preserved as {!constr:CXInt}
    by turning {!recfield:Clang.convert_integer_literals} option false.
    {[
let example = "0;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr ({ desc = IntegerLiteral (Int 0) } as expr)}] ->
      assert (Clang.Expr.radix_of_integer_literal expr = Some Octal)
  | _ -> assert false

let () =
  check pp_stmt (parse_statement_list
    ~options:({ Clang.Ast.Options.default with
       convert_integer_literals = false }))
    example @@ fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (CXInt _ as zero) }}] ->
      assert (Clang.Ast.int_of_literal zero = 0)
  | _ -> assert false

let example = Printf.sprintf "%d;" Int.max_int

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (Int max_int) }}] ->
      assert (max_int = Int.max_int)
  | _ -> assert false

let large_int = Int64.add (Int64.of_int max_int) 1L
let example = Printf.sprintf "%Ld;" large_int

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (CXInt _ as large_int') }}] ->
      assert (Clang.Ast.int64_of_literal large_int' = large_int)
  | _ -> assert false

let example = "42;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr ({ desc = IntegerLiteral (Int 42) } as expr)}] ->
      assert (Clang.Expr.radix_of_integer_literal expr = Some Decimal)
  | _ -> assert false

let example = "0xFF;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr ({ desc = IntegerLiteral (Int 255) } as expr)}] ->
      assert (Clang.Expr.radix_of_integer_literal expr = Some Hexadecimal)
  | _ -> assert false

    ]}*)
  | FloatingLiteral of floating_literal
(** Floating literal.

    By default, floating literals are converted into {!constr:Float}.
    Floating literals can be preserved as {!constr:CXFloat}
    by turning {!recfield:Clang.convert_floating_literals} option false.
    {[
let example = "0.5;"

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = FloatingLiteral (Float 0.5) }}] -> ()
  | _ -> assert false

let () =
  check pp_stmt (parse_statement_list
    ~options:({ Clang.Ast.Options.default with
      convert_floating_literals = true }))
    example @@ fun ast -> match ast with
  | [{ desc = Expr { desc = FloatingLiteral f }}] ->
    assert (Clang.Ast.float_of_literal f = 0.5)
  | _ -> assert false
    ]}*)
  | StringLiteral of string_literal
(** String literal.
    {[
let example = "\"Hello!\";"

let () =
  check pp_stmt parse_statement_list example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc =
      StringLiteral { byte_width = 1; bytes = "Hello!" }}}] -> ()
  | _ -> assert false
    ]}*)
  | CharacterLiteral of {
      kind : character_kind;
      value : int;
    }
(** Character literal.
    {[
let example = "'a';"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
        CharacterLiteral { kind = Ascii; value = 0x61 } }}] -> ()
  | _ -> assert false

let example = "L'a';"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
        CharacterLiteral { kind = Wide; value = 0x61 } }}] -> ()
  | _ -> assert false


let example = "u8'a';"

let () =
  if Clang.version () >= { major = 3; minor = 6; subminor = 0 } then
    check pp_stmt (parse_statement_list ~language:CXX
        ~command_line_args:["-std=c++1z"]) example @@
    fun ast -> match ast with
    | [{ desc = Expr { desc = CharacterLiteral { kind = UTF8; value = 0x61 } }}]
      -> assert (Clang.version () >= { major = 3; minor = 8; subminor = 0 })
    | [{ desc = Expr { desc =
          CharacterLiteral { kind = Ascii; value = 0x61 } }}]
      -> assert (Clang.version () < { major = 3; minor = 8; subminor = 0 })
    | _ -> assert false

let example = "u'a';"

let () =
  if Clang.version () >= { major = 3; minor = 6; subminor = 0 } then
    check pp_stmt parse_statement_list example
    @@ fun ast -> match ast with
    | [{ desc = Expr { desc =
          CharacterLiteral { kind = UTF16; value = 0x61 } }}]
      -> assert (Clang.version () >= { major = 3; minor = 6; subminor = 0 })
    | [{ desc = Expr { desc = CharacterLiteral { kind = UTF8; value = 0x61 } }}]
      -> assert (Clang.version () < { major = 3; minor = 6; subminor = 0 })
    | _ -> assert false
    ]}*)
  | ImaginaryLiteral of expr
(** Imaginary literal.
    {[
let example = "1i;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
      ImaginaryLiteral { desc = IntegerLiteral (Int 1)} }}] -> ()
  | _ -> assert false

let example = "2.5i;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc =
      ImaginaryLiteral { desc = FloatingLiteral (Float 2.5)}}}] -> ()
  | _ -> assert false
    ]}*)
  | BoolLiteral of bool
(** Boolean literal (C++).
    {[
let example = "true;"

let () =
  check pp_stmt (parse_statement_list ~language:CXX) example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = BoolLiteral true }}] -> ()
  | _ -> assert false

let example = "false;"

let () =
  check pp_stmt (parse_statement_list ~language:CXX) example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = BoolLiteral false }}] -> ()
  | _ -> assert false
    ]}*)
  | NullPtrLiteral
(** Null pointer literal (C++11).
    {[
let example = "nullptr;"

let () =
  check pp_stmt
    (parse_statement_list ~language:CXX ~standard:Cxx11) example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = NullPtrLiteral }}] -> ()
  | _ -> assert false
    ]}*)
  | UnaryOperator of {
      kind : unary_operator_kind;
      operand : expr;
    }
(** Unary operator.
    {[
let example = "+1;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = UnaryOperator {
      kind = Plus;
      operand = { desc = IntegerLiteral (Int 1)}}}}] -> ()
  | _ -> assert false

let example = "int x; &x;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = UnaryOperator {
      kind = AddrOf;
      operand = { desc = DeclRef ({ name = IdentifierName "x" }) }} }}] -> ()
  | _ -> assert false
    ]}*)
  | BinaryOperator of {
      lhs : expr;
      kind : binary_operator_kind;
      rhs : expr;
    }
(** Binary operator.
    {[
let example = "1 + 2;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = BinaryOperator {
      lhs = { desc = IntegerLiteral (Int 1)};
      kind = Add;
      rhs = { desc = IntegerLiteral (Int 2)}}}}] -> ()
  | _ -> assert false

let example = "int i = 2; i *= 3;"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = DeclRef ({ name = IdentifierName "i" })};
      kind = MulAssign;
      rhs = { desc = IntegerLiteral (Int 3)}}}}] -> ()
  | _ -> assert false
    ]} *)
  | DeclRef of ident_ref
(** Declaration reference.

    Note: you may use {!val:Clang.Expr.get_definition} to get a cursor
    to the declaration site. Cursors can be hashed with {!val:Clang.hash_cursor}
    and compared with {!val:Clang.compare_cursors}
    and {!val:Clang.equal_cursors}.

    {[
let example = "int i; i;"

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern? [
    { desc = Decl _ };
    { desc = Expr { desc = DeclRef ({ name = IdentifierName "i" }) }}]]

let example = "{ int i; i; } { int i; i; }"

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern? [
    { desc = Compound [
      { desc = Decl [d1] };
      { desc = Expr ({ desc = DeclRef ({ name = IdentifierName "i" })} as e1)}]
    };
    { desc = Compound [
      { desc = Decl [d2] };
      { desc = Expr ({ desc = DeclRef ({ name = IdentifierName "i" })} as e2)}]
    }]]
  ~result:begin fun b ->
    assert (Clang.equal_cursors
      (Clang.Expr.get_definition b#e1)
      (Clang.get_cursor_definition (Clang.Ast.cursor_of_node b#d1)));
    assert (Clang.equal_cursors
      (Clang.Expr.get_definition b#e2) (Clang.Ast.cursor_of_node b#d2));
    assert (not (Clang.equal_cursors
      (Clang.Expr.get_definition b#e1) (Clang.Ast.cursor_of_node b#d2)));
    assert (not (Clang.equal_cursors
      (Clang.Expr.get_definition b#e2) (Clang.Ast.cursor_of_node b#d1)));
  end ]} *)
  | Call of {
      callee : expr;
      args : expr list;
    }
(** Function call.
    {[
let example = "void g(int); g(1);"

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = Call {
      callee = { desc = DeclRef ({ name = IdentifierName "g" }) };
      args = [{ desc = IntegerLiteral (Int 1)}] }}}] -> ()
  | _ -> assert false
    ]} *)
  | Cast of {
      kind : cast_kind;
      qual_type : qual_type;
      operand : expr;
    }
(** Cast.

    {[
let example = {| (void * ) "Hello"; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = Cast {
      kind = CStyle;
      qual_type = { desc = Pointer _ };
      operand = { desc = StringLiteral { bytes = "Hello" }}}}}] -> ()
  | _ -> assert false
    ]}

    Implicit casts are removed in the AST unless [ignore_implicit_cast = false]
    is passed to the converting function.

    {[
let example = {| int i; i; |}

let () =
  check pp_stmt (parse_statement_list
    ~options:({ Clang.Ast.Options.default with ignore_implicit_cast = false }))
    example @@
  fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = Cast {
      kind = Implicit;
      qual_type = { desc = BuiltinType Int };
      operand = { desc = DeclRef ({ name = IdentifierName "i" }) }} }}] -> ()
  | _ -> assert false
    ]}
*)
  | Member of {
      base : expr option;
      arrow : bool;
      field : field;
    }
(** Member dot or arrow
    {[
let example = {| struct s { int i; } s; s.i = 0; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = Member {
        base = Some { desc = DeclRef ({ name = IdentifierName "s" }) };
        arrow = false;
        field = FieldName { desc = { name = IdentifierName "i" } }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral (Int 0)}}}}] -> ()
  | _ -> assert false

let example = {| struct s { int i; } *p; p->i = 0; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = Member {
        base = Some { desc = DeclRef ({ name = IdentifierName "p" }) };
        arrow = true;
        field = FieldName { desc = { name = IdentifierName "i" } }}};
      kind = Assign;
      rhs = { desc = IntegerLiteral (Int 0)}}}}] -> ()
  | _ -> assert false
    ]}*)
  | ArraySubscript of {
      base : expr;
      index : expr;
    }
(** Array subscript
    {[
let example = {| int a[1]; a[0] = 1; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Decl _ }; { desc = Expr { desc = BinaryOperator {
      lhs = { desc = ArraySubscript {
        base = { desc = DeclRef ({ name = IdentifierName "a" }) };
        index = { desc = IntegerLiteral (Int 0)}}};
      kind = Assign;
      rhs = { desc = IntegerLiteral (Int 1)}}}}] -> ()
  | _ -> assert false
    ]}*)
  | ConditionalOperator of {
      cond : expr;
      then_branch : expr option;
      else_branch : expr;
    }
(** Conditional operator.
    [None] in [else_branch] captures GNU "missing middle" extension.
    {[
let example = {| 1 ? 2 : 3; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = ConditionalOperator {
      cond = { desc = IntegerLiteral (Int 1)};
      then_branch = Some { desc = IntegerLiteral (Int 2)};
      else_branch = { desc = IntegerLiteral (Int 3)}}}}] -> ()
  | _ -> assert false

let example = {| 1 ? : 3; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = ConditionalOperator {
      cond = { desc = IntegerLiteral (Int 1)};
      then_branch = None;
      else_branch = { desc = IntegerLiteral (Int 3)}}}}] -> ()
  | _ -> assert false
    ]}*)
  | Paren of expr
(** Parenthesed expression.

    Parenthesed expression are removed in the AST unless ~ignore_paren:false
    is passed to the converting function.
    {[
let example = {| (1); |}

let () =
  check pp_stmt (parse_statement_list
    ~options:{ Clang.Ast.Options.default with ignore_paren = false })
    example @@
  fun ast -> match ast with
  | [{ desc = Expr { desc = Paren { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = IntegerLiteral (Int 1)}}] -> ()
  | _ -> assert false

let example = {| int i; sizeof(i); |}

let () =
  check pp_stmt (parse_statement_list
    ~options:{ Clang.Ast.Options.default with ignore_paren = false })
    example @@
  fun ast -> match ast with
  | [ { desc = Decl _ };
      { desc = Expr { desc = UnaryExpr {
          kind = SizeOf;
          argument = ArgumentExpr { desc = Paren { desc = DeclRef ({ name = IdentifierName "i" }) }}} }}] ->
      ()
  | _ -> assert false
    ]}*)
  | AddrLabel of string
(** Label address (Labels as Values GNU extension).
    {[
let example = {| label: &&label; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Label {
        label = "label";
        body = { desc = Expr { desc = AddrLabel "label" }}}}] ->
      ()
  | _ -> assert false
    ]}*)
  | InitList of expr list
(** Initialization list.
    {[
let example = {| int a[2] = { 1, 2 }; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Var { var_name = "a"; var_type = {
      desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 2;
        size_as_expr = Some { desc = IntegerLiteral (Int 2) }}};
      var_init = Some { desc = InitList [
        { desc = IntegerLiteral (Int 1)};
        { desc = IntegerLiteral (Int 2)}] }}}] -> ()
  | _ -> assert false
    ]}*)
  | CompoundLiteral of {
      qual_type : qual_type;
      init : expr
    }
(** Compound literal [C99 6.5.2.5].
    {[
let example = {| (int []) { 1, 2 }; |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [{ desc = Expr { desc = CompoundLiteral {
      qual_type = { desc = ConstantArray {
        element = { desc = BuiltinType Int };
        size = 2 }};
      init = { desc = InitList [
        { desc = IntegerLiteral (Int 1)};
        { desc = IntegerLiteral (Int 2)}] }} }}] -> ()
  | _ -> assert false
    ]}*)
  | UnaryExpr of {
      kind : unary_expr_kind;
      argument : expr_or_type;
    }
(** Unary expr: sizeof, alignof (C++11), ...
    {[

let example = {| int i; sizeof(i); |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [ { desc = Decl _ };
      { desc = Expr { desc = UnaryExpr {
          kind = SizeOf;
          argument = ArgumentExpr { desc = DeclRef ({ name = IdentifierName "i" }) }} }}] -> ()
  | _ -> assert false

let example = {| sizeof(int); |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [ { desc = Expr { desc = UnaryExpr {
          kind = SizeOf;
          argument = ArgumentType { desc = BuiltinType Int }} }}] -> ()
  | _ -> assert false

let example = {| alignof(int); |}

let () =
  check pp_stmt
    (parse_statement_list ~language:CXX
      ~command_line_args:["-std=c++11"])
    example @@
  fun ast -> match ast with
  | [ { desc = Expr { desc = UnaryExpr {
          kind = AlignOf;
          argument = ArgumentType { desc = BuiltinType Int }} }}] -> ()
  | _ -> assert false
    ]}

    From Clang>=6.0.0, [alignof] is available by default with C++.

    {[
let () =
  if Clang.version () >= { major = 6; minor = 0; subminor = 0 } then
    check pp_stmt (parse_statement_list ~language:CXX)
    example @@ fun ast -> match ast with
    | [ { desc = Expr { desc = UnaryExpr {
            kind = AlignOf;
            argument = ArgumentType { desc = BuiltinType Int }} }}] -> ()
    | _ -> assert false
    ]}
*)
  | GenericSelection of {
      controlling_expr : expr;
      assocs : (qual_type option * expr) list;
    }
(**
   Generic selection (C11).

   {[
let example = {|
   _Generic("expr", double: 1, float: 2, default: 3);
   |}

let () =
  check pp_stmt parse_statement_list example
  @@ fun ast -> match ast with
  | [ { desc = Expr { desc = GenericSelection {
          controlling_expr = { desc = StringLiteral { bytes = "expr" }};
          assocs = [
            (Some { desc = BuiltinType Double},
              { desc = IntegerLiteral (Int 1)});
            (Some { desc = BuiltinType Float},
              { desc = IntegerLiteral (Int 2)});
            (None, { desc = IntegerLiteral (Int 3)})] }}}] -> ()
  | _ -> assert false
   ]}
 *)
  | Predefined of {
      kind : predefined_identifier_kind;
      function_name : string;
    }
(**
   Predefined identifiers.

   {[
let example = {|
  void
  myfunc(void)
  {
    const char *_s = __func__;
  }
|}

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Void};
        parameters = Some { non_variadic = []; variadic = false }};
      name = IdentifierName "myfunc";
      body = Some { desc = Compound [{
        desc = Decl [{ desc = Var {
          var_type = { desc = Pointer { desc = BuiltinType Char_S }};
          var_name = "_s";
          var_init = Some { desc = Predefined {
            kind = Func;
            function_name = "myfunc"; }}}}] }] }}}]]
   ]}*)
  | ExprWithCleanups of expr
  | TemporaryObject of {
      qual_type : qual_type;
      args : expr list;
    }
  | MaterializeTemporaryExpr of expr
  | BindTemporaryExpr of expr
  | Lambda of {
      capture_default : lambda_capture_default;
      captures : lambda_capture list;
      is_mutable : bool;
      parameters : parameter list option;
      result_type : qual_type option;
      body : stmt;
    }
  | This
  | New of {
      placement_args : expr list;
      qual_type : qual_type;
      array_size : expr option;
      init : expr option;
    }
  | Delete of {
      global_delete : bool;
      array_form : bool;
      argument : expr;
    }
(**
   Delete operator.

   {[
let example = {|
  struct T {};
  void
  f(void)
  {
    T* t = new T();
    delete t;
  }
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl { keyword = Struct; name = "T"; fields = [] }};
     { desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Void};
        parameters = Some { non_variadic = []; variadic = false }};
      name = IdentifierName "f";
      body = Some { desc = Compound [
        { desc = Decl [{ desc = Var {
          var_type = { desc = Pointer { desc =
            Record ({ name = IdentifierName "T" })}};
          var_name = "t";
          var_init = Some { desc = New {
            placement_args = [];
            qual_type = { desc = Record ({ name = IdentifierName "T" })};
            array_size = None;
            init = None; }}}}] };
        { desc = Expr { desc = Delete {
          global_delete = false;
          array_form = false;
          argument = { desc = DeclRef ({ name = IdentifierName "t" })}}}}] }}}]]

let example = {|
  void
  f(void)
  {
    int* t = new int[10];
    delete[] t;
  }
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Void};
        parameters = Some { non_variadic = []; variadic = false }};
      name = IdentifierName "f";
      body = Some { desc = Compound [
        { desc = Decl [{ desc = Var {
          var_type = { desc = Pointer { desc = BuiltinType Int }};
          var_name = "t";
          var_init = Some { desc = New {
            placement_args = [];
            qual_type = { desc = BuiltinType Int };
            array_size = Some { desc = IntegerLiteral (Int 10)};
            init = None; }}}}] };
        { desc = Expr { desc = Delete {
          global_delete = false;
          array_form = true;
          argument = { desc = DeclRef ({ name = IdentifierName "t" })}}}}] }}}]]

let example = {|
  struct T {};
  void
  f(void)
  {
    T* t = new T();
    ::delete t;
  }
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl { keyword = Struct; name = "T"; fields = [] }};
     { desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Void};
        parameters = Some { non_variadic = []; variadic = false }};
      name = IdentifierName "f";
      body = Some { desc = Compound [
        { desc = Decl [{ desc = Var {
          var_type = { desc = Pointer { desc =
            Record ({ name = IdentifierName "T" })}};
          var_name = "t";
          var_init = Some { desc = New {
            placement_args = [];
            qual_type = { desc = Record ({ name = IdentifierName "T" })};
            array_size = None;
            init = None; }}}}] };
        { desc = Expr { desc = Delete {
          global_delete = true;
          array_form = false;
          argument = { desc = DeclRef ({ name = IdentifierName "t" })}}}}] }}}]]
   ]}*)
  | Typeid of expr_or_type
  | PackExpansionExpr of expr
  | Fold of {
      lhs : expr option;
      operator : binary_operator_kind;
      rhs : expr option;
    }
  | SizeOfPack of ident_ref
  | Construct of {
      qual_type : qual_type;
      args : expr list;
    }
  | UnresolvedConstruct of {
      qual_type : qual_type;
      args : expr list;
    }
  | ThrowExpr of expr option
  | TemplateRef of ident_ref
  | OverloadedDeclRef of ident_ref
  | StdInitializerList of expr list
  | DefaultArg
  | UnexposedExpr of clang_ext_stmtkind
  | SubstNonTypeTemplateParm of expr
  | ArrayInitLoop of {
      common_expr : expr;
      sub_expr : expr;
    }
  | ArrayInitIndex
  | NoexceptExpr of expr
(** noexcept unary operator (C++11).
  {[
let example =
  "noexcept(1);"

let () =
  check_pattern quote_stmt_list
    (parse_statement_list ~language:CXX ~standard:Cxx11) example
  [%pattern?
    [{ desc = Expr { desc = NoexceptExpr { desc = IntegerLiteral (Int 1)}}}]]
    ]}*)
  | ImplicitValueInit of qual_type
(** Implicitly generated initialization value.
  {[
let example = {|
struct { int a; int b } x = { 1 };
|}

let () =
  if Clang.version () >= { major = 3; minor = 7; subminor = 0 } then
    check_pattern quote_stmt_list (parse_statement_list
      ~options:{ Clang.Ast.Options.default with init_list_form = Semantic })
      example
    [%pattern?
      [{ desc = Decl [
        { desc = RecordDecl _; _ };
        { desc = Var {
            var_name = "x";
            var_init = Some { desc = InitList [
              { desc = IntegerLiteral (Int 1); _ };
              { desc = ImplicitValueInit {
                  desc = (BuiltinType Int) }}] }}}] }]]
 ]}*)
  | DesignatedInit of {
      designators : designator list;
      init : expr;
    }
  | Requires of {
      local_parameters : parameter list;
      requirements : requirement list;
    }
(** Requires expression (C++ 20).
    {[
let example = {|
  template<typename T> concept Addable = requires (T x) { x + x; };
|}

let () =
  if Clang.version () >= { major = 10; minor = 0; subminor = 0 } then
    check_pattern quote_decl_list (parse_declaration_list ~language:CXX
      ~command_line_args:[Clang.Command_line.standard Cxx20]) example
    [%pattern?
      [{ desc = Concept {
         parameters = { list = [{ desc = { parameter_name = "T" }}] };
         name = IdentifierName ("Addable");
         constraint_expr = {
           desc = Requires {
             local_parameters =[ { desc = {
               qual_type = { desc = TemplateTypeParm "T" };
               name = "x" }}];
             requirements = [Simple { expr = { desc = BinaryOperator {
               lhs = { desc = DeclRef { name = IdentifierName "x" }};
               kind = Add;
               rhs = { desc = DeclRef { name = IdentifierName "x" }}}}}] }}}}]]
    ]} *)
  | UnknownExpr of cxcursorkind * clang_ext_stmtkind

and field =
  | FieldName of ident_ref node
  | PseudoDestructor of {
      nested_name_specifier : nested_name_specifier option;
      qual_type : qual_type;
    }
  | DependentScopeMember of {
      ident_ref : ident_ref;
      template_arguments : template_argument list;
    }
  | UnresolvedMember of ident_ref

and lambda_capture = {
   capture_kind : lambda_capture_kind;
   implicit : bool;
   captured_var_name : string option;
   pack_expansion : bool;
 }

and cast_kind =
  | CStyle
  | Implicit
  | Functional
  | Static
  | Dynamic
  | Const

and expr_or_type =
  | ArgumentExpr of expr
  | ArgumentType of qual_type

and string_literal = {
  byte_width : int;
  bytes : string;
  string_kind : string_kind;
}

and designator =
  | FieldDesignator of string
(** Designator for a field of a structure or an union in a designated
    initialization list.
  {[
let example = {|
  struct {
    int a;
    int b;
  } s = { .a = 1, .b = 2 };
|}

let () =
  check_pattern quote_stmt_list parse_statement_list example
  [%pattern?
    [{ desc = Decl [
      { desc = RecordDecl _; _ };
      { desc = Var {
          var_name = "s";
          var_init = Some { desc = InitList [
             { desc = DesignatedInit {
                designators = [FieldDesignator "a"];
                init = { desc = IntegerLiteral (Int 1) }}};
             { desc = DesignatedInit {
                designators = [FieldDesignator "b"];
                init = { desc = IntegerLiteral (Int 2) }}}] }}}] }]]
    ]}*)
  | ArrayDesignator of expr
  | ArrayRangeDesignator of expr * expr

(** {3 Declarations} *)

and decl = decl_desc node

and decl_desc =
  | TemplateDecl of {
      parameters : template_parameter_list;
      decl : decl;
    }
(** Template declaration.

    {[
let example = {| template <class X, int i> int f(X); |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = { list = [
        { desc = {
            parameter_name = "X";
            parameter_kind = Class { default = None }}};
        { desc = { parameter_name = "i"; parameter_kind = NonType {
            parameter_type = { desc = BuiltinType Int };
            default = None }}}] };
      decl = { desc = Function {
        function_type = {
          calling_conv = C;
          result = { desc = BuiltinType Int};
          parameters = Some {
            non_variadic = [
              { desc = {
                  name = "";
                  qual_type = { desc = TemplateTypeParm "X"}}}];
            variadic = false }};
        name = IdentifierName "f";
        body = None }}}}] -> ()
  | _ -> assert false

let example = {|
  template <class X = bool, int i = 4>
  class C { X x; int v = i; };
|}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = { list = [
        { desc = {
            parameter_name = "X";
            parameter_kind =
              Class { default = Some { desc = BuiltinType Bool }}}};
        { desc = { parameter_name = "i"; parameter_kind = NonType {
            parameter_type = { desc = BuiltinType Int };
            default = Some { desc = IntegerLiteral (Int 4)}}}}] };
      decl = { desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field {
              name = "x";
              qual_type = { desc = TemplateTypeParm "X" }}};
          { desc = Field {
              name = "v";
              qual_type = { desc = BuiltinType Int }}}] }}}}] -> ()
  | _ -> assert false

let example = {|
  class C {
    template <class X>
    int f(X x);
  };

  template <class X>
  int C::f(X x)
  {
    return 0;
  } |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl {
      keyword = Class;
      name = "C";
      fields = [
        { desc = TemplateDecl {
            parameters = { list = [
              { desc = { parameter_name = "X";
                parameter_kind = Class { default = None };
                parameter_pack = false; }}] };
            decl = { desc = CXXMethod {
              type_ref = { desc = Record ({ name = IdentifierName "C" }) };
              function_decl = {
                function_type = {
                  result = { desc = BuiltinType Int };
                  parameters = Some { non_variadic = [
                    { desc = {
                        name = "x";
                        qual_type = { desc = TemplateTypeParm "X" }}}] }};
                name = IdentifierName "f";
                body = None; }}}}}] }};
     { desc = TemplateDecl {
         parameters = { list = [{ desc = {
           parameter_name = "X";
           parameter_kind = Class { default = None };
           parameter_pack = false; }}] };
         decl = { desc = CXXMethod {
           type_ref = { desc = Record ({ name = IdentifierName "C" }) };
           function_decl = {
             function_type = {
               result = { desc = BuiltinType Int };
               parameters = Some { non_variadic = [
                 { desc = {
                     name = "x";
                     qual_type = { desc = TemplateTypeParm "X" }}}] }};
             name = IdentifierName "f";
             body = Some { desc = Compound [
               { desc =
                   Return (Some { desc = IntegerLiteral (Int 0)})}] }; }}}}}]]

let example = {| template<class ... Types> struct Tuple {}; |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
        parameters = { list = [
          { desc = {
            parameter_name = "Types";
            parameter_kind = Class { default = None };
            parameter_pack = true; }}] };
        decl = { desc = RecordDecl {
          keyword = Struct;
          name = "Tuple"; }}}}] -> ()
  | _ -> assert false


let example = {|
  template<typename T> requires (sizeof(T) > 1)
  void f(T);
|}

let () =
  if Clang.version () >= { major = 9; minor = 0; subminor = 0 } then
    check_pattern quote_decl_list (parse_declaration_list ~language:CXX
      ~command_line_args:[Clang.Command_line.standard Cxx20]) example
    [%pattern?
      [{ desc = TemplateDecl {
         parameters = {
           list = [{ desc = { parameter_name = "T" }}];
           requires_clause = Some ({ desc = BinaryOperator {
             lhs = { desc = UnaryExpr {
               kind = SizeOf;
               argument = ArgumentType { desc = TemplateTypeParm "T" }}};
             kind = GT;
             rhs =  { desc = IntegerLiteral (Int 1) }}})};
         decl = { desc = Function {
           function_type = {
             result = { desc = BuiltinType Void };
             parameters = Some {
               non_variadic = [
                 { desc = { qual_type = { desc = TemplateTypeParm "T" }}}] }};
           name = IdentifierName "f";
           body = None; }}}}]]
    ]}*)
  | Function of function_decl
(** Function definition or forward declaration.
    In case of function definition, we should have
    [body = Some { desc = Compound list; _ }] for some [list].
    In case of forward declaration, [body = None].

    {[
let example = {| int f(void) { return 0; } |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = Function {
      linkage = External;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Int};
        parameters = Some { non_variadic = []; variadic = false }};
      name = IdentifierName "f";
      body = Some { desc = Compound [
        { desc = Return (Some {desc = IntegerLiteral (Int 0)})}] }}}] -> ()
  | _ -> assert false

let example = {| static int f(int x); |}

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = Function {
      linkage = Internal;
      function_type = {
        calling_conv = C;
        result = { desc = BuiltinType Int};
        parameters = Some {
          non_variadic = [
            { desc = { name = "x"; qual_type = { desc = BuiltinType Int }}}];
          variadic = false }};
      name = IdentifierName "f";
      body = None }}] -> ()
  | _ -> assert false
    ]}*)
  | TemplatePartialSpecialization of {
      parameters : template_parameter_list;
      arguments : template_argument list;
      decl : decl;
    }
(** Template partial specialization.
    {[
let example = {|
template<long n> struct A { };
template<typename T> struct C;
template<typename T, T n> struct C<A<n>> {
  using Q = T;
};
|}
let () =
  if Clang.version () >= { major = 3; minor = 5; subminor = 0 } then
    check_pattern quote_decl_list (parse_declaration_list ~language:CXX
      ~command_line_args:[Clang.Command_line.standard Cxx17]) example
    [%pattern? [
      { desc = TemplateDecl {
        parameters = { list = [
          { desc = {
            parameter_name = "n";
            parameter_kind = NonType {
              parameter_type = { desc = BuiltinType Long }}}}] };
        decl = { desc = RecordDecl {
          keyword = Struct; name = "A"; fields = [] }}}};
      { desc = TemplateDecl {
        parameters = { list = [
          { desc = { parameter_name = "T"; parameter_kind = Class _ }}] };
        decl = { desc = RecordDecl {
          keyword = Struct; name = "C"; fields = [] }}}};
      { desc = TemplatePartialSpecialization {
        parameters = { list = [
          { desc = { parameter_name = "T"; parameter_kind = Class _ }};
          { desc = {
            parameter_name = "n";
            parameter_kind = NonType {
              parameter_type = { desc = TemplateTypeParm "T" }}}}] };
        arguments = [
          Type { desc = TemplateSpecialization {
            name = NameTemplate "A";
            args = [
              ExprTemplateArgument { desc = DeclRef {
                name = IdentifierName "n" }}] }}];
        decl = { desc = RecordDecl {
          keyword = Struct; name = "C"; fields = [
            { desc = TypeAlias {
              ident_ref = { name = IdentifierName "Q" };
              qual_type = { desc = TemplateTypeParm "T" }}}] }}}}]]
    ]}*)
  | CXXMethod of {
      function_decl : function_decl;
      type_ref : qual_type;
      defaulted : bool;
      static : bool;
      binding : cxx_method_binding_kind;
      const : bool;
    }
(** C++ method.

    {[
let example = {|
    class C {
      int f(char);
      void const_method() const {
      }
      virtual void virtual_method() {
      }
      virtual void pure_virtual_method() = 0;
      static void static_method() {
      }
      void deleted_method() =delete;
      C &operator+(C &rhs) {
        return *this;
      }
    };

    int C::f(char c) {
      return 0;
    }
 |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl {
      keyword = Class;
      name = "C";
      fields = [
        { desc = CXXMethod {
          type_ref = { desc = Record ({ name = IdentifierName "C" }) };
          function_decl = {
            name = IdentifierName "f";
            function_type = {
              result = { desc = BuiltinType Int };
              parameters = Some { non_variadic = [
                { desc = {
                    name = "";
                    qual_type = { desc = BuiltinType Char_S }}}] }};
            body = None; }}};
        { desc = CXXMethod {
          type_ref = { desc = Record ({ name = IdentifierName "C" }) };
          function_decl = {
            name = IdentifierName "const_method";
            function_type = {
              result = { desc = BuiltinType Void };
              parameters = Some { non_variadic = [] }};
            body = Some { desc = Compound [] }};
          const = true; }};
        { desc = CXXMethod {
          type_ref = { desc = Record ({ name = IdentifierName "C" }) };
          binding = Virtual;
          function_decl = {
            name = IdentifierName "virtual_method";
            function_type = {
              result = { desc = BuiltinType Void };
              parameters = Some { non_variadic = [] }};
            body = Some { desc = Compound [] }; }}};
        { desc = CXXMethod {
          type_ref = { desc = Record ({ name = IdentifierName "C" }) };
          binding = PureVirtual;
          function_decl = {
            name = IdentifierName "pure_virtual_method";
            function_type = {
              result = { desc = BuiltinType Void };
              parameters = Some { non_variadic = [] }};
            body = None; }}};
        { desc = CXXMethod {
          type_ref = { desc = Record ({ name = IdentifierName "C" }) };
          function_decl = {
            name = IdentifierName "static_method";
            function_type = {
              result = { desc = BuiltinType Void };
              parameters = Some { non_variadic = [] }};
            body = Some { desc = Compound [] }};
          static = true; }};
        { desc = CXXMethod {
          type_ref = { desc = Record ({ name = IdentifierName "C" }) };
          function_decl = {
            name = IdentifierName "deleted_method";
            function_type = {
              result = { desc = BuiltinType Void };
              parameters = Some { non_variadic = [] }};
            deleted = true;
            body = None; }; }};
        { desc = CXXMethod {
          type_ref = { desc = Record ({ name = IdentifierName "C" }) };
          function_decl = {
            name = OperatorName Plus;
            function_type = {
              result = { desc = LValueReference { desc =
                Record ({ name = IdentifierName "C" }) }};
              parameters = Some {
                non_variadic = [{ desc = {
                  name = "rhs";
                  qual_type = { desc =
                    LValueReference { desc =
                      Record ({ name = IdentifierName "C" }) }}}}] }};
            body = Some { desc = Compound [{
              desc = Return (Some {
                desc = UnaryOperator {
                  kind = Deref;
                  operand = { desc = This }}})}] }}}}] }};
      { desc = CXXMethod {
        type_ref = { desc = Record ({ name = IdentifierName "C" }) };
        function_decl = {
          name = IdentifierName "f";
          function_type = {
            result = { desc = BuiltinType Int };
            parameters = Some { non_variadic = [{ desc = {
              name = "c";
              qual_type = { desc = BuiltinType Char_S }}}] }};
          body = Some { desc = Compound [
            { desc = Return (Some { desc = IntegerLiteral (Int 0) })}] }}}}]]
    ]}*)
  | Var of var_decl_desc
(** Variable declaration.

    {[
let example = {| int x = 1; |}

let () =
    check pp_decl parse_declaration_list example @@
    fun ast -> match ast with
  | [{ desc = Var {
      linkage = External;
      var_type = { const = false; desc = BuiltinType Int };
      var_name = "x";
      var_init = Some { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = {| const int x = 1; |}

let () =
    check pp_decl parse_declaration_list example @@
    fun ast -> match ast with
  | [{ desc = Var {
      linkage = External;
      var_type = { const = true; desc = BuiltinType Int };
      var_name = "x";
      var_init = Some { desc = IntegerLiteral (Int 1)}}}] -> ()
  | _ -> assert false

let example = {| static int x = 1; |}

let () =
    check pp_decl parse_declaration_list example @@
    fun ast -> match ast with
  | [{ desc = Var {
      linkage = Internal;
      var_type = { const = false; desc = BuiltinType Int };
      var_name = "x";
      var_init = Some ({ desc = IntegerLiteral (Int 1)})}}] -> ()
  | _ -> assert false

let example = {| int var1 asm("altvar") = 1; |}

[%%meta if Clangml_config.version.major >= 10 then [%stri
let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Var {
      var_type = { const = false; desc = BuiltinType Int };
      var_name = "var1";
      var_init = Some ({ desc = IntegerLiteral (Int 1)});
      attributes = [{ desc = AsmLabel { label = "altvar" }}] }}]]]
else Metapp.Stri.of_list []]
    ]}*)
  | EnumDecl of {
      name : string;
      constants : enum_constant list;
      complete_definition : bool;
      attributes : attribute list;
    }
(** Enum declaration.
    {[
let example = {| enum e { A, B = 2, C }; |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = EnumDecl {
      name = "e";
      constants = [
        { desc = { constant_name = "A"; constant_init = None }} as a;
        { desc = {
          constant_name = "B";
          constant_init = Some { desc = IntegerLiteral (Int 2)}}} as b;
        { desc = { constant_name = "C"; constant_init = None }} as c] }}]]
  ~result:(fun bindings ->
    assert (Clang.Enum_constant.get_value bindings#a = 0);
    assert (Clang.Enum_constant.get_value bindings#b = 2);
    assert (Clang.Enum_constant.get_value bindings#c = 3))

let example = {|
enum __attribute__((enum_extensibility(closed))) ClosedEnum {
  A0, A1
};

enum __attribute__((enum_extensibility(open))) OpenEnum {
  B0, B1
};

enum __attribute__((enum_extensibility(closed),flag_enum)) ClosedFlagEnum {
  C0 = 1 << 0, C1 = 1 << 1
};

enum __attribute__((enum_extensibility(open),flag_enum)) OpenFlagEnum {
  D0 = 1 << 0, D1 = 1 << 1
};
|}

[%%meta if Clangml_config.version.major >= 5 then [%stri
let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = EnumDecl {
          name = "ClosedEnum";
          constants = [
            { desc = { constant_name = "A0"; constant_init = None }};
            { desc = { constant_name = "A1"; constant_init = None }}];
          complete_definition = true;
          attributes = [{ desc = EnumExtensibility Closed }] }};
      { desc = EnumDecl {
          name = "OpenEnum";
          constants = [
            { desc = { constant_name = "B0"; constant_init = None }};
            { desc = { constant_name = "B1"; constant_init = None }}];
          complete_definition = true;
          attributes = [{ desc = EnumExtensibility Open }] }};
      { desc = EnumDecl {
          name = "ClosedFlagEnum";
          constants = [
            { desc = { constant_name = "C0";
                constant_init = Some { desc = BinaryOperator {
                  lhs = { desc = IntegerLiteral (Int 1) };
                  kind = Shl;
                  rhs = { desc = IntegerLiteral (Int 0) }}}}};
            { desc = { constant_name = "C1";
                constant_init = Some { desc = BinaryOperator {
                  lhs = { desc = IntegerLiteral (Int 1) };
                  kind = Shl;
                  rhs = { desc = IntegerLiteral (Int 1) }}}}}];
          complete_definition = true;
          attributes = ([
            { desc = EnumExtensibility Closed };
            { desc = Other FlagEnum }] | [
            { desc = Other FlagEnum };
            { desc = EnumExtensibility Closed }]) }};
      { desc = EnumDecl {
          name = "OpenFlagEnum";
          constants = [
            { desc = { constant_name = "D0";
                constant_init = Some { desc = BinaryOperator {
                  lhs = { desc = IntegerLiteral (Int 1) };
                  kind = Shl;
                  rhs = { desc = IntegerLiteral (Int 0) }}}}};
            { desc = { constant_name = "D1";
                constant_init = Some { desc = BinaryOperator {
                  lhs = { desc = IntegerLiteral (Int 1) };
                  kind = Shl;
                  rhs = { desc = IntegerLiteral (Int 1) }}}}}];
          complete_definition = true;
          attributes = ([
            { desc = EnumExtensibility Open };
            { desc = Other FlagEnum }] | [
            { desc = Other FlagEnum };
            { desc = EnumExtensibility Open }]) }}]]]
else Metapp.Stri.of_list []]
    ]}*)
  | RecordDecl of record_decl
(** Record declaration ([struct], [union] or [class]).
    {[
let example = {| struct s { int i; float f; }; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "i";
          qual_type = { desc = BuiltinType Int}}};
        { desc = Field { name = "f";
          qual_type = { desc = BuiltinType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| struct s { int a:1; int b:2; int c; }; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "a";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 1)}}} as a;
        { desc = Field { name = "b";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 2)}}};
        { desc = Field { name = "c";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None}}] }}] ->
      assert (Clang.Decl.get_field_bit_width a = 1)
  | _ -> assert false

let example = {| union u { int i; float f; }; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Union;
      name = "u";
      fields = [
        { desc = Field { name = "i";
          qual_type = { desc = BuiltinType Int}}};
        { desc = Field { name = "f";
          qual_type = { desc = BuiltinType Float}}}] }}] -> ()
  | _ -> assert false

let example = {| union u { int a:1; int b:2; int c; }; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Union;
      name = "u";
      fields = [
        { desc = Field { name = "a";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 1)}}} as a;
        { desc = Field { name = "b";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 2)}}};
        { desc = Field { name = "c";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None}}] }}] ->
      assert (Clang.Decl.get_field_bit_width a = 1)
  | _ -> assert false

let example = {| struct s { int label; union { int i; float f; };}; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "label";
          qual_type = { desc = BuiltinType Int}}};
        { desc = RecordDecl { keyword = Union; name = ""; fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }}] }}] -> ()
  | _ -> assert false

let example = {| union s { int single; struct { int i; float f; };}; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Union;
      name = "s";
      fields = [
        { desc = Field { name = "single";
          qual_type = { desc = BuiltinType Int}}};
        { desc = RecordDecl { keyword = Struct; name = ""; fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }}] }}] -> ()
  | _ -> assert false
    ]}*)
  | TypedefDecl of {
      name : string;
      underlying_type : qual_type;
    }
(** Typedef declaration.

    Note that if the typedef declares a new underlying type,
    the declaration of the underlying type precedes the typedef
    declaration in the AST.
    {[
let example = {| typedef int int_t; |}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = TypedefDecl {
      name = "int_t";
      underlying_type = { desc = BuiltinType Int }}}] -> ()
  | _ -> assert false

let example = {| typedef union u { int i; float f; } u_t; |}

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = RecordDecl {
        keyword = Union;
        name = "u";
        fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int}}};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float}}}] }};
      { desc = TypedefDecl {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record ({ name = IdentifierName "u" }) }}}}}]]

let example = {| typedef union { int i; float f; } u_t; |}

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = RecordDecl {
        keyword = Union;
        name = "";
        fields = [
          { desc = Field {
              name = "i";
              qual_type = { desc = BuiltinType Int}}};
          { desc = Field {
              name = "f";
              qual_type = { desc = BuiltinType Float}}}] }};
      { desc = TypedefDecl {
        name = "u_t";
        underlying_type = { desc = Elaborated {
          keyword = Union;
          named_type = { desc = Record ({ name = IdentifierName "" }) }}}
            as underlying_type }}]]
  ~result:begin fun bindings ->
    let fields = bindings#underlying_type |> Clang.Type.list_of_fields in
    match test quote_decl_list fields [%pattern?
        [{ desc = Field {
            name = "i";
            qual_type = { desc = BuiltinType Int}}};
        { desc = Field {
            name = "f";
            qual_type = { desc = BuiltinType Float}}}]] with
    | Ok () -> ()
    | Error failure ->
        Format.printf "@[failed:@ %a@]@." Pattern.format_failure
          failure;
        assert false
  end
    ]}*)
  | Field of {
      name : string;
      qual_type : qual_type;
      bitwidth : expr option;
      init : expr option; (* C++11 *)
      attributes : attribute list;
    }
(** Record (struct, union or class) field.

    {[
let example = {|
  struct s { int label : 1; union u { int i; float f; } data;};
|}

let () =
  check pp_decl parse_declaration_list example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Struct;
      name = "s";
      fields = [
        { desc = Field { name = "label";
          qual_type = { desc = BuiltinType Int};
          bitwidth = Some { desc = IntegerLiteral (Int 1)};
          init = None; }};
        { desc = RecordDecl { keyword = Union; name = "u"; fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int};
            bitwidth = None;
            init = None; }};
          { desc = Field { name = "f";
            qual_type = { desc = BuiltinType Float};
            bitwidth = None;
            init = None; }}] }};
        { desc = Field { name = "data";
          qual_type = { desc = Elaborated {
            keyword = Union;
            named_type = { desc = Record ({ name = IdentifierName "u" }) }}};
          bitwidth = None;
          init = None; }}] }}] -> ()
  | _ -> assert false

let example = {| class C { int i = 1; }; |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example
  @@ fun ast -> match ast with
  | [{ desc = RecordDecl {
      keyword = Class;
      name = "C";
      fields = [
        { desc = Field { name = "i";
          qual_type = { desc = BuiltinType Int};
          bitwidth = None;
          init = Some { desc = IntegerLiteral (Int 1)}}}] }}] -> ()
  | _ -> assert false
    ]}

    Default member initializer for bit-field is a C++2a extension
    (supported from Clang >6.0.0).

    {[

let example = {| class C { int i : 3 = 2; }; |}

let () =
  if Clang.version () >= { major = 6; minor = 0; subminor = 0 } then
    check pp_decl (parse_declaration_list ~language:CXX
      ~command_line_args:["-std=c++2a"]) example
    @@ fun ast -> match ast with
    | [{ desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field { name = "i";
            qual_type = { desc = BuiltinType Int};
            bitwidth = Some { desc = IntegerLiteral (Int 3)};
            init = Some { desc = IntegerLiteral (Int 2)}}}] }}] -> ()
    | _ -> assert false
      ]}

    Fields can have a [no_unique_address] attribute.

    {[
let example = {|
template<typename T, typename Alloc> struct my_vector {
  T *p;
  [[no_unique_address]] Alloc alloc;
  // ...
};
|}

[%%meta if Clangml_config.version.major >= 9 then [%stri
    let () =
      check_pattern quote_decl_list (parse_declaration_list ~language:CXX)
        example
      [%pattern?
        [{ desc = TemplateDecl {
          parameters = { list = [
            { desc = { parameter_name = "T" }};
            { desc = { parameter_name = "Alloc" }}] };
          decl = { desc = RecordDecl {
            keyword = Struct;
            fields = [
              { desc = Field {
                  name = "p";
                  qual_type = { desc =
                    Pointer { desc = TemplateTypeParm "T" }}}};
              { desc = Field {
                  name = "alloc";
                  qual_type = { desc = TemplateTypeParm "Alloc" };
                  attributes = [{ desc = Other NoUniqueAddress }] }}] }}}}]]]
  else Metapp.Stri.of_list []]
    ]}*)
  | AccessSpecifier of cxx_access_specifier
(** C++ access specifier.

    {[
let example = {|
    class c {
      private: int private_field;
      protected: int protected_field;
      public: int public_field;
    };
    |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl {
      keyword = Class;
      name = "c";
      fields = [
        { desc = AccessSpecifier CXXPrivate };
        { desc = Field { name = "private_field";
          qual_type = { desc = BuiltinType Int}}};
        { desc = AccessSpecifier CXXProtected };
        { desc = Field { name = "protected_field";
          qual_type = { desc = BuiltinType Int}}};
        { desc = AccessSpecifier CXXPublic };
        { desc = Field { name = "public_field";
          qual_type = { desc = BuiltinType Int}}}] }}]]
    ]}*)
  | Namespace of {
      name : string;
      declarations : decl list;
      inline : bool; (* C++11 *)
    }
(** C++ namespace.

    {[
let example = {|
    namespace example {
      int i;
    }
    |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = Namespace {
      name = "example";
      declarations = [
        { desc = Var { var_name = "i";
          var_type = { desc = BuiltinType Int}}}];
      inline = false; }}] -> ()
  | _ -> assert false
    ]}

let example = {|
    inline namespace example {
      int i;
    }
    |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = Namespace {
      name = "example";
      declarations = [
        { desc = Var { var_name = "i";
          var_type = { desc = BuiltinType Int}}}];
      inline = true; }}] -> ()
  | _ -> assert false
    ]}
*)
  | UsingDirective of {
      nested_name_specifier : nested_name_specifier option;
      namespace : decl;
    }
(** C++ "using" directive.

    {[
let example = {|
    namespace std {}
    using namespace std;
    |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = Namespace { name = "std" }};
     { desc = UsingDirective {
      nested_name_specifier = None;
      namespace = { desc = Namespace { name = "std" }}}}]]
    ]}*)
  | UsingDeclaration of ident_ref
(** C++ "using" declaration.

    {[
let example = {|
    namespace std {
      void cout() {}
    }
    using std::cout;
    |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [_; { desc = UsingDeclaration {
      nested_name_specifier = Some [NamespaceName "std"];
      name = IdentifierName "cout" }}]]
    ]}*)
  | Constructor of {
      class_name : string;
      parameters : parameters;
      initializer_list : (string * expr) list;
      body : stmt option;
      explicit : bool;
      defaulted : bool;
      deleted : bool;
      constexpr : bool;
    }
(**
  C++ class constructor.

    {[
let example = {|
    class C {
      int i;
      C() = delete;
      explicit C(int v) : i(v) {
      }
    };
    |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Field {
               name = "i";
               qual_type = { desc = BuiltinType Int }}};
           { desc = Constructor {
               class_name = "C";
               parameters = {
                 non_variadic = [];
                 variadic = false;
               };
               initializer_list = [];
               body = None;
               explicit = false;
               defaulted = false;
               deleted = true; }};
           { desc = Constructor {
               parameters = {
                 non_variadic = [{ desc = {
                   name = "v";
                   qual_type = { desc = BuiltinType Int}}}];
                 variadic = false;
               };
               initializer_list =
                 ["i", { desc = DeclRef ({ name = IdentifierName "v" }) }];
               body = Some { desc = Compound [] };
               explicit = true;
               defaulted = false;
               deleted = false; }}; ] }}] -> ()
  | _ -> assert false

let example = {|
    class C {
      C() =default;
    };
    |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Constructor {
               class_name = "C";
               parameters = {
                 non_variadic = [];
                 variadic = false;
               };
               initializer_list = [];
               body = None;
               explicit = false;
               defaulted = true;
               deleted = false; }}] }}] -> ()
  | _ -> assert false
   ]}*)
  | Destructor of {
      class_name : string;
      body : stmt option;
      defaulted : bool;
      deleted : bool;
      exception_spec : exception_spec option;
    }
(**
  C++ class destructor.

    {[
let example = {|
    class C {
      ~C() {
      }
    };
    |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Destructor {
               class_name = "C";
               body = Some { desc = Compound [] };
               defaulted = false;
               deleted = false; }}] }}] -> ()
  | _ -> assert false

let example = {|
    class C {
      ~C() =default;
    };
    |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Destructor {
               class_name = "C";
               body = None;
               defaulted = true;
               deleted = false; }}] }}] -> ()
  | _ -> assert false
   ]}*)
  | LinkageSpec of {
      language : Clang__types.language;
      decls : decl list;
    }
(**
  C++ language linkage.

    {[
let example = {|
    extern "C" {
      int i;

      void f() {
      }
    }
   |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = LinkageSpec {
         language = C;
         decls = [
           { desc = Var {
               var_type = { desc = BuiltinType Int };
               var_name = "i"; }};
           { desc = Function {
               function_type = {
                 result = { desc = BuiltinType Void };
                 parameters = Some { non_variadic = []; variadic = false }};
               name = IdentifierName "f";
               body = Some { desc = Compound [] }}}] }}] -> ()
  | _ -> assert false

let example = {|
    extern "C++" {
      int i;

      void f() {
      }
    }
   |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = LinkageSpec {
         language = CXX;
         decls = [
           { desc = Var {
               var_type = { desc = BuiltinType Int };
               var_name = "i"; }};
           { desc = Function {
               function_type = {
                 result = { desc = BuiltinType Void };
                 parameters = Some { non_variadic = []; variadic = false }};
               name = IdentifierName "f";
               body = Some { desc = Compound [] }}}] }}] -> ()
  | _ -> assert false
   ]}*)
  | TemplateTemplateParameter of string
  | Friend of friend_decl
(**
  C++ friend declaration.

   {[
let example = {|
     class C {
       friend void f();
       friend class B;
     };
   |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Friend (FriendDecl { desc = Function {
               function_type = {
                 result = { desc = BuiltinType Void };
                 parameters = Some { non_variadic = []; variadic = false }};
               name = IdentifierName "f";
               body = None }})};
           { desc = Friend (FriendType { desc = Elaborated {
               keyword = Class;
               named_type = {
                 desc = Record ({ name = IdentifierName "B" }) }}})}] }}]]
   ]}

   {[
let example = {|
     template <typename T> class C {
       friend T; // only in C++0x
     };
   |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = TemplateDecl {
         parameters = { list = [{ desc = {
           parameter_name = "T";
           parameter_kind = Class { default = _ }}}] };
         decl = { desc = RecordDecl {
           keyword = Class;
           name = "C";
           fields = [
             { desc = Friend (FriendType { desc =
                 TemplateTypeParm "T" })}] }}}}]]
   ]}

   {[
let example = {|
     class C {
       template <typename T> friend class B;
     };
   |}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [{ desc = Friend (FriendDecl { desc = TemplateDecl {
           parameters = { list = [{ desc = {
             parameter_name = "T";
             parameter_kind = Class { default = _ }}}] };
           decl = { desc = RecordDecl {
             keyword = Class;
             name = "B";
             fields = [] }}}})}] }}]]
   ]}*)
  | NamespaceAlias of { alias : ident_ref; original : ident_ref }
(** C++ namespace alias.

    {[
let example = {|
  namespace N { int d; }
  namespace N1 = N;
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX)
    example [%pattern?
    [{ desc = _ };
     { desc = NamespaceAlias {
         alias = { name = IdentifierName "N1" };
         original = { name = IdentifierName "N" } }}]]
    ]}*)
  | EmptyDecl
(**
  Empty declaration.

    {[
let example = ";"

let () =
  check pp_decl parse_declaration_list example @@
  fun ast -> match ast with
  | [{ desc = EmptyDecl }] -> ()
  | _ -> assert false
    ]}*)
  | Directive of directive
  | StaticAssert of {
      constexpr : expr;
      message : expr option; (** Optional since C++ 17 *)
    }
(** Static assertion. (C++ 11) *)
  | TypeAlias of {
      ident_ref : ident_ref;
      qual_type : qual_type;
    }
  | Decomposition of {
      bindings : declaration_name list;
      init : expr option;
    }
(** Structure decomposition. (C++ 17) *)
  | Concept of {
      parameters : template_parameter_list;
      name : declaration_name;
      constraint_expr : expr;
    }
(** Concept declaration. (C++ 20)
    {[
let example = "template<class T> concept C1 = true;"

let () =
  if Clang.version () >= { major = 9; minor = 0; subminor = 0 } then
    check_pattern quote_decl_list (parse_declaration_list ~language:CXX
      ~command_line_args:[Clang.Command_line.standard Cxx20]) example
    [%pattern?
      [{ desc = Concept {
         parameters = { list = [{ desc = { parameter_name = "T" }}] };
         name = IdentifierName ("C1");
         constraint_expr = { desc = BoolLiteral true }}}]]

let example = "template<typename T> concept A = T::value || true;"

let () =
  if Clang.version () >= { major = 9; minor = 0; subminor = 0 } then
    check_pattern quote_decl_list (parse_declaration_list ~language:CXX
      ~command_line_args:[Clang.Command_line.standard Cxx20]) example
    [%pattern?
      [{ desc = Concept {
         parameters = { list = [{ desc = { parameter_name = "T" }}] };
         name = IdentifierName ("A");
         constraint_expr = {
           desc = BinaryOperator {
             lhs = { desc = DeclRef {
               nested_name_specifier = Some [
                 TypeSpec { desc = TemplateTypeParm "T" }];
               name = IdentifierName "value" }};
             kind = LOr;
             rhs = { desc = BoolLiteral true }}}}}]]
    ]}*)
  | Export of decl list
(** Export declaration context. (C++ 20)
    {[
let example = {|
  export module M;
  export namespace NS {}
|}

let () =
  if Clang.version () >= { major = 9; minor = 0; subminor = 0 } then
    check_pattern quote_decl_list (parse_declaration_list ~language:CXX
      ~command_line_args:[Clang.Command_line.standard Cxx20]) example
    [%pattern?
      [{ desc = Export [
        { desc = Namespace {
          name = "NS";
          declarations = [] }}] }]]
    ]}*)

  | UnknownDecl of cxcursorkind * clang_ext_declkind

and directive =
  | Include of {
      program_context : bool;
      filename : string;
    }
  | Ifdef of string
  | Ifndef of string
  | Endif

and base_specifier = {
  qual_type : qual_type;
(** Type of the base class.
    {[
let example = {|
class A {};
class B : A {};
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl { keyword = Class; name = "A" }};
      { desc = RecordDecl {
          keyword = Class;
          name = "B";
          bases = [{
            qual_type = { desc = Record { name = IdentifierName "A" }};
            virtual_base = false;
            access_specifier = CXXPrivate; }] }}]]
    ]}*)
  virtual_base : bool;
(** Determines whether the base is virtual.
    {[
let example = {|
class A {};
class B : virtual A {};
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl { keyword = Class; name = "A" }};
      { desc = RecordDecl {
          keyword = Class;
          name = "B";
          bases = [{
            qual_type = { desc = Record { name = IdentifierName "A" }};
            virtual_base = true;
            access_specifier = CXXPrivate; }] }}]]
    ]}*)
  access_specifier : cxx_access_specifier;
(** Determines whether the base is private (default), protected or public.
    {[
let example = {|
class A {};
class B : private A {};
class C : protected A {};
class D : public A {};
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
    [{ desc = RecordDecl { keyword = Class; name = "A" }};
      { desc = RecordDecl {
          keyword = Class;
          name = "B";
          bases = [{
            qual_type = { desc = Record { name = IdentifierName "A" }};
            virtual_base = false;
            access_specifier = CXXPrivate; }] }};
      { desc = RecordDecl {
          keyword = Class;
          name = "C";
          bases = [{
            qual_type = { desc = Record { name = IdentifierName "A" }};
            virtual_base = false;
            access_specifier = CXXProtected; }] }};
      { desc = RecordDecl {
          keyword = Class;
          name = "D";
          bases = [{
            qual_type = { desc = Record { name = IdentifierName "A" }};
            virtual_base = false;
            access_specifier = CXXPublic; }] }}]]
    ]}*)
}

and ident_ref = {
  nested_name_specifier : nested_name_specifier option;
  name : declaration_name;
  template_arguments : template_argument list;
}
(**
    {[
let example = {|
  namespace ns1 {
    namespace ns2 {
      int f() {
        return 0;
      }
    }
  }

  int g() {
    return ns1::ns2::f();
  }
|}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [_; { desc =
      Function {
        name = IdentifierName "g";
        body = Some { desc = Compound [
          { desc = Return (Some { desc = Call {
              callee = { desc = DeclRef ({
                nested_name_specifier = Some [
                  NamespaceName "ns1";
                  NamespaceName "ns2";];
                name = IdentifierName "f"})};
              args = []; }})}] }}}] -> ()
  | _ -> assert false
    ]}

    {[
let example = {|
  class C {
  public:
    enum E {
      A, B
    };
  };

  C::E g() {
    return C::E::A;
  }
|}

let () =
  if Clang.version () >= { major = 3; minor = 7; subminor = 0 } then
    check_pattern quote_decl_list
      (parse_declaration_list ~command_line_args:[
         Clang.Command_line.language CXX; "-std=c++11"]) example
    [%pattern?
      [_; { desc =
        Function {
          function_type = {
            result = { desc = Elaborated {
              nested_name_specifier = Some [
                TypeSpec { desc = Record { name = IdentifierName "C" }}];
              named_type = { desc = Enum { name = IdentifierName "E" }}}}};
          name = IdentifierName "g";
          body = Some { desc = Compound [
            { desc = Return (Some { desc = DeclRef {
                nested_name_specifier = Some [
                  TypeSpec { desc = Record { name = IdentifierName "C" }};
                  TypeSpec { desc = Enum { name = IdentifierName "E" }}];
                name = IdentifierName "A"}})}] }}}]]
    ]}*)

and friend_decl =
  | FriendDecl of decl
  | FriendType of qual_type

and label_ref = string

and enum_constant = enum_constant_desc node

and enum_constant_desc = {
    constant_name : string;
    constant_init : expr option;
  }

and var_decl = var_decl_desc node

and var_decl_desc = {
    linkage : linkage_kind;
    var_name : string;
    var_type : qual_type;
    var_init : expr option;
    constexpr : bool;
    attributes : attribute list;
  }

and cxx_method_binding_kind = NonVirtual | Virtual | PureVirtual
(** C++ method binding kind *)

and template_parameter_list = {
    list : template_parameter list;
    requires_clause : expr option;
  }

and template_parameter = template_parameter_desc node
(** C++ template parameter *)

and template_parameter_desc = {
    parameter_name : string;
    parameter_kind : template_parameter_kind;
    parameter_pack : bool; (** C++11 *)
  }

and template_parameter_kind =
  | Class of {
      default : qual_type option;
    }
(** Class (or typename) template parameter.

    {[
let example = {|
  template <class X, typename Y = bool>
  class C { X x; Y y; };
|}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = { list = [
        { desc = {
            parameter_name = "X";
            parameter_kind = Class { default = None }}};
        { desc = {
            parameter_name = "Y";
            parameter_kind =
              Class { default = Some { desc = BuiltinType Bool }}}}] };
      decl = { desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field {
              name = "x";
              qual_type = { desc = TemplateTypeParm "X" }}};
          { desc = Field {
              name = "y";
              qual_type = { desc = TemplateTypeParm "Y" }}}] }}}}] -> ()
  | _ -> assert false
    ]}*)
  | NonType of {
      parameter_type : qual_type;
      default : expr option;
    }
(** Non type template parameter.

    {[
let example = {|
  template <int i = 4>
  class C { int v = i; };
|}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
      parameters = { list = [
        { desc = { parameter_name = "i"; parameter_kind = NonType {
            parameter_type = { desc = BuiltinType Int };
            default = Some { desc = IntegerLiteral (Int 4)}}}}] };
      decl = { desc = RecordDecl {
        keyword = Class;
        name = "C";
        fields = [
          { desc = Field {
              name = "v";
              qual_type = { desc = BuiltinType Int }}}] }}}}] -> ()
  | _ -> assert false
    ]}*)
  | Template of {
      parameters : template_parameter_list;
      default : string option;
    }
(** Template template parameter.

    {[
let example = {|
  template<typename T> class Default {};

  template <template<typename> class T = Default>
  class C {
    T<bool> x;
    T<int> y;
  };
    |}

let () =
  check pp_decl (parse_declaration_list ~language:CXX) example @@
  fun ast -> match ast with
  | [{ desc = TemplateDecl {
       parameters = { list = [{ desc = {
         parameter_name = "T";
         parameter_kind = Class { default = None };
         parameter_pack = false; }}] };
       decl = { desc = RecordDecl {
         keyword = Class;
         name = "Default";
         fields = [] }}}};
     { desc = TemplateDecl {
       parameters = { list = [{ desc = {
         parameter_name = "T";
         parameter_kind = Template {
           parameters = { list = [{ desc = {
             parameter_name = "";
             parameter_kind = Class { default = None };
             parameter_pack = false; }}] };
         default = Some "Default" };
         parameter_pack = false; }}] };
       decl = { desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Field {
               name = "x";
               qual_type = { desc = TemplateSpecialization {
                 name = NameTemplate "T";
                 args = [Type { desc = BuiltinType Bool }] }}}};
           { desc = Field {
               name = "y";
               qual_type = { desc = TemplateSpecialization {
                 name = NameTemplate "T";
                 args = [Type { desc = BuiltinType Int }] }}}}] }}}}] -> ()
  | _ -> assert false

let example = {|
  template <template<int... > class T>
    class C {
      T<1, 2, 3> x;
    };
|}

let () =
  check_pattern quote_decl_list (parse_declaration_list ~language:CXX) example
  [%pattern?
     [{ desc = TemplateDecl {
       parameters = { list = [{ desc = {
         parameter_name = "T";
         parameter_kind = Template {
           parameters = { list = [{ desc = {
             parameter_name = "";
             parameter_kind = NonType {
               parameter_type = { desc = BuiltinType Int };
               default = None };
             parameter_pack = true }}] };
           default = None };
         parameter_pack = false }}] };
       decl = { desc = RecordDecl {
         keyword = Class;
         name = "C";
         fields = [
           { desc = Field {
               name = "x";
               qual_type = { desc = TemplateSpecialization {
                 name = NameTemplate "T";
                 args = [
                   ExprTemplateArgument { desc = IntegerLiteral (Int 1) };
                   ExprTemplateArgument { desc = IntegerLiteral (Int 2) };
                   ExprTemplateArgument { desc = IntegerLiteral (Int 3) }]
            }}}}] }}}}]]
    ]}*)

(** {3 Translation units} *)

and translation_unit = translation_unit_desc node

and translation_unit_desc = {
    filename : string; items : decl list
  } [@@deriving refl]

(** {3 Type loc: source representation of types}

    [type_loc] API is now deprecated and is kept for compatibility only.
    *)

type type_loc = {
    typeloc : (clang_ext_typeloc option [@opaque]);
    desc : type_loc_desc;
  }
and type_loc_desc =
  | BuiltinTypeLoc of builtin_type
(** Built-in type. *)
  | TypedefTypeLoc of ident_ref
(** Typedef. *)
  | ConstantArrayTypeLoc of {
      size : expr;
      element : type_loc;
    }
(** Constant array type.
    [size] contains the original expression.
    In the type  {!type:qual_type}, the [size] of [ConstantArray]
    is computed but [size_as_expr] contains the original expression.

    {[
let example = {| const int i = 1; int a[i + 1]; |}

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [_; { desc = Var { var_name = "a"; var_type = { desc = ConstantArray {
      element = { desc = BuiltinType Int };
      size = 2 }}}} as decl]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Decl.get_type_loc bindings#decl)
      [%pattern? { desc = ConstantArrayTypeLoc {
        size = { desc = BinaryOperator {
          lhs = { desc = DeclRef { name = IdentifierName "i" }};
          kind = Add;
          rhs = { desc = IntegerLiteral (Int 1)}}};
        element = { desc = BuiltinTypeLoc Int }}}])
  end
    ]} *)
  | VariableArrayTypeLoc of {
      size : expr;
      element : type_loc;
    }
(** Variable array type.

    {[
let example = "void f(int i, int a[i + 1]);"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Function { name = IdentifierName "f"; function_type =
      { result = { desc = BuiltinType Void };
        parameters = Some {
          non_variadic = [
            { desc = { name = "i"; qual_type = { desc = BuiltinType Int }}};
            array];
          variadic = false }}}}]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Parameter.get_type_loc bindings#array)
      [%pattern? { desc = VariableArrayTypeLoc {
        size = { desc = BinaryOperator {
          lhs = { desc = DeclRef { name = IdentifierName "i" }};
          kind = Add;
          rhs = { desc = IntegerLiteral (Int 1)}}};
        element = { desc = BuiltinTypeLoc Int }}}])
  end
    ]}*)
  | IncompleteArrayTypeLoc of {
      element : type_loc;
    }
(** Incomplete array type.

    {[
let example = "const int i = 1; void f(int a[][i + 1]);"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [_; { desc = Function { name = IdentifierName "f"; function_type =
      { result = { desc = BuiltinType Void };
        parameters = Some {
          non_variadic = [array];
          variadic = false }}}}]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Parameter.get_type_loc bindings#array)
      [%pattern? { desc = IncompleteArrayTypeLoc {
        element = { desc = ConstantArrayTypeLoc {
          size = { desc = BinaryOperator {
            lhs = { desc = DeclRef { name = IdentifierName "i" }};
            kind = Add;
            rhs = { desc = IntegerLiteral (Int 1)}}};
          element = { desc = BuiltinTypeLoc Int }}}}}])
  end
    ]}*)
  | PointerTypeLoc of {
      pointee : type_loc;
    }
(** Pointer type.

    {[
let example = "void f(int **a);"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Function { name = IdentifierName "f"; function_type =
      { result = { desc = BuiltinType Void };
        parameters = Some {
          non_variadic = [array];
          variadic = false }}}}]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Parameter.get_type_loc bindings#array)
      [%pattern? { desc = PointerTypeLoc
        { pointee = { desc = PointerTypeLoc
          { pointee = { desc = BuiltinTypeLoc Int }}}}}])
  end
    ]}*)
  | BlockPointerTypeLoc of {
      pointee : type_loc;
    }
(** Block pointer type (Clang's C language extension).

    {[
let example = {|
  typedef int (^IntBlock)();
|}

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~command_line_args:["-fblocks"]) example
  [%pattern?
    [{ desc = TypedefDecl { name = "IntBlock"; _ }} as decl]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Decl.get_type_loc bindings#decl)
      [%pattern? { desc = BlockPointerTypeLoc {
        pointee = { desc = FunctionTypeLoc {
          result = { desc = BuiltinTypeLoc Int };
          parameters = [] }}}}])
  end
    ]}*)
  | MemberPointerTypeLoc of {
      class_ : type_loc;
      pointee : type_loc;
    }
(** Member pointer type (C++).

    {[
let example = {|
  struct S {
    int i;
  };
  int S::*p;
|}

let () =
  check_pattern quote_decl_list
    (parse_declaration_list ~language:CXX) example
  [%pattern?
    [_; { desc = Var { var_name = "p"; var_type = { desc = MemberPointer {
      class_ = { desc = Record { name = IdentifierName "S" } };
      pointee = { desc = BuiltinType Int }}}}} as decl]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Decl.get_type_loc bindings#decl)
      [%pattern? { desc = MemberPointerTypeLoc {
        class_ = { desc = RecordTypeLoc { name = IdentifierName "S"}};
        pointee = { desc = BuiltinTypeLoc Int }}}])
  end
    ]}*)
  | FunctionTypeLoc of {
      result : type_loc;
      parameters : parameter list;
    }
(** Function type.

    {[
let example = "int (*p)(int x[1 + 1]);"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Var { var_name = "p"; var_type = { desc =
      Pointer { desc = FunctionType {
        result = { desc = BuiltinType Int };
        parameters = Some { non_variadic = [
          { desc = { name = "x"; qual_type = { desc =
            ConstantArray {
              element = { desc = BuiltinType Int };
              size = 2;
              size_as_expr = Some { desc = BinaryOperator {
              lhs = { desc = IntegerLiteral (Int 1)};
              kind = Add;
              rhs = { desc = IntegerLiteral (Int 1)}}}}}}}] }}}}}} as decl]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Decl.get_type_loc bindings#decl)
      [%pattern? { desc = PointerTypeLoc {
        pointee = { desc = FunctionTypeLoc {
          result = { desc = BuiltinTypeLoc Int };
          parameters = [
            { desc = { name = "x"; qual_type = { desc = ConstantArray {
              element = { desc = BuiltinType Int };
              size = 2; }}}} as parameter] }}}}])
      ~result:begin fun bindings ->
        check_result (Pattern.check quote_type_loc
          (Clang.Parameter.get_type_loc bindings#parameter)
          [%pattern? { desc = ConstantArrayTypeLoc {
            size = { desc = BinaryOperator {
              lhs = { desc = IntegerLiteral (Int 1)};
              kind = Add;
              rhs = { desc = IntegerLiteral (Int 1)}}};
            element = { desc = BuiltinTypeLoc Int }}}])
      end
  end
    ]}*)
  | QualifiedTypeLoc of type_loc
(** Qualified type.

    {[
let example = "const int i = 1;"

let () =
  check_pattern quote_decl_list parse_declaration_list example
  [%pattern?
    [{ desc = Var { var_name = "i";
      var_type = { const = true; desc = BuiltinType Int }}} as decl]]
  ~result:begin fun bindings ->
    check_result (Pattern.check quote_type_loc
      (Clang.Decl.get_type_loc bindings#decl)
      [%pattern? { desc = QualifiedTypeLoc { desc = BuiltinTypeLoc Int }}])
  end
    ]}*)
  | RecordTypeLoc of ident_ref
  | EnumTypeLoc of ident_ref
  | ElaboratedTypeLoc of qual_type
  | UnknownTypeLoc of clang_ext_typeloc_class
        [@@deriving refl]
end

module Id = Custom (IdNode)

module Lazy = Custom (LazyNode)

include Id

(*{[
let () =
  Printf.eprintf "%d success%s and %d failure%s.\n"
    !success_count (if !success_count > 1 then "es" else "")
    !failure_count (if !failure_count > 1 then "s" else "");
  if !failure_count > 0 then
    exit 1
 ]}*)
