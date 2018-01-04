open! Core_kernel
open! Import

include Table_intf.Table

module Default_sort_spec : sig
  (** A sortable value. The sorting order of a row is specified by specifying a conversion
      to this type.

      Note that typically, all values will be injected into just one of these variant
      constructors.  The sorting between different constructors is considered arbitrary. *)
  module Sort_key : sig
    type t =
      | String of string
      | Float of float
      | Integer of Int63.t
      | Null
    [@@deriving compare, sexp]

    include Table_intf.Sort_key with type t := t
  end

  module Sort_dir : sig
    type t = Ascending | Descending [@@deriving sexp, compare]

    include Table_intf.Sort_dir with type t := t
  end

  include Table_intf.Sort_spec
    with module Sort_key := Sort_key
     and module Sort_dir := Sort_dir
end
