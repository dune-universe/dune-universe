module Stable = struct
  open! Core_kernel.Core_kernel_stable

  module V2 = struct
    type t =
      { max_resources : int
      ; idle_cleanup_after : Time_ns.Span.V2.t
      ; max_resources_per_id : int
      ; max_resource_reuse : int
      ; close_idle_resources_when_at_limit : bool
      }
    [@@deriving bin_io, sexp]

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 0b4e2e81e7d6313c18730c4807c9d69f |}]
    ;;
  end

  module V1 = struct
    type t =
      { max_resources : int
      ; idle_cleanup_after : Time_ns.Span.V2.t
      ; max_resources_per_id : int
      ; max_resource_reuse : int
      }
    [@@deriving
      bin_io
    , sexp
    , stable_record ~version:V2.t ~add:[ close_idle_resources_when_at_limit ]]

    let to_v2 = to_V2_t ~close_idle_resources_when_at_limit:false

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 32e8b51732879423c8392f29f3265d27 |}]
    ;;
  end
end

open! Core_kernel
open! Async_kernel
open! Import

type t = Stable.V2.t =
  { max_resources : int
  ; idle_cleanup_after : Time_ns.Span.t
  ; max_resources_per_id : int
  ; max_resource_reuse : int
  ; close_idle_resources_when_at_limit : bool
  }
[@@deriving compare, fields, sexp_of]

let create = Fields.create
