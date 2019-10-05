(* This module contains type definitions.

   This module is defined by this clang__types.ml file only, without
   interface file ( *.mli ) such that these type definitions do not have
   to be written twice.

   This module should only depend on Clang__bindings and Clang__compat.
   The modules Clang__utils and Clang__ast depend on it. *)

(* From FrontendOptions.h:InputKind:Language *)
type language = C | CXX | ObjC | ObjCXX | OpenCL | CUDA | RenderScript | HIP

type standard = Clang__bindings.clang_ext_langstandards

type location_kind = Presumed | Expansion
