(** {2 Low-level interface } *)

(** The module includes {!module:Clang__bindings} which contains the
auto-generated wrappers over [libclang] and some extensions defined in
[libclang_extensions.h]. *)

module Bindings : sig
  include module type of struct
    include Clang__bindings

    include Special_bindings
  end
end

include module type of struct
  include Bindings
end

module Cursor : sig
  include Hashtbl.HashedType with type t = cxcursor

  module Hashtbl : Hashtbl.S with type key = cxcursor
end

module Types = Clang__types

include module type of struct
  include Types
end

include module type of struct
  include Clang__utils
end

module Standard = Standard

module Command_line = Clang__command_line

module Printer = Printer

val version : unit -> cxversion
(** [version ()] is the Clang version. *)

val includedir : string
(** [includedir] is equal to the path to Clang [include/] directory, i.e.,
    [`llvm-config --includedir`/../lib/clang/`llvm-config --version`/include],
    where [llvm-config] commands has been evaluated when [clangml] has been
    configured.
    This directory contains [stddef.h] and other compiler-specific headers,
    and it is common to pass
    [Clang.Command_line.include_directory Clang.includedir]
    to Clang command-line. *)

val default_include_directories : unit -> string list
(** [default_include_directories ()] is a list of include directories that are
    common to pass to Clang command-line. The list contains {!val:includedir}.
 *)

val compare_cursors : cxcursor -> cxcursor -> int
(** [compare_cursors c1 c2] provides a total order over cursors. *)

val get_typedef_underlying_type : ?recursive:bool -> cxtype -> cxtype
(** [get_typedef_underlying_type t] returns the underlying type of [t] if [t]
    is a typedef, and [t] otherwise. If [recursive] is [true]
    (default: [false]), typedefs are followed until the underlying type is not a
    typedef. *)

(** {2 Abstract syntax tree} *)

module type S = Ast_sig.S

module Id : Ast_sig.CustomS with module Node := Clang__ast.IdNode

module Lazy : Ast_sig.CustomS with module Node := Clang__ast.LazyNode

include Ast_sig.CustomS with module Node := Clang__ast.IdNode
