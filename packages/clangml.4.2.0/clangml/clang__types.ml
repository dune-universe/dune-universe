(* This module contains type definitions.

   This module is defined by this clang__types.ml file only, without
   interface file ( *.mli ) such that these type definitions do not have
   to be written twice.

   This module should only depend on Clang__bindings and Clang__compat.
   The modules Clang__utils and Clang__ast depend on it. *)

(* From FrontendOptions.h:InputKind:Language *)
type language = C | CXX | ObjC | ObjCXX | OpenCL | CUDA | RenderScript | HIP
  [@@deriving refl]

type location_kind = Expansion | Presumed | Instantiation | Spelling | File
  [@@deriving refl]

type concrete_location = {
    filename : string;
    line : int;
    column : int
  }

(** Use by {!val:Clang__utils.pp_diagnostic} *)
module Display_source_location = struct
  type t = {
      kind : location_kind;
      column : bool;
      ranges : bool;
    }

  let default = {
    kind = Presumed;
    column = false;
    ranges = false;
  }
end

module Diagnostic_display_options = struct
  type t = {
      source_location : Display_source_location.t option;
      option : bool;
      category_id : bool;
      category_name : bool;
    }

  let default = {
    source_location = Some Display_source_location.default;
    option = false;
    category_id = false;
    category_name = false;
  }
end
