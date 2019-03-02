open! Core_kernel
open! Async_kernel
open! Import
open! Eager_deferred.Use
include Resource_intf

module Make_simple (R : Simple) = struct
  module Key = R.Key
  module Common_args = R.Common_args

  type resource = R.t

  type t =
    { underlying : R.t
    ; close : unit Lazy_deferred.t
    }

  let underlying t = t.underlying

  let open_ key common_args =
    let open Deferred.Or_error.Let_syntax in
    let%map underlying =
      Monitor.try_with_join_or_error
        ~name:(sprintf !"opening resource [%{sexp:Key.t}]" key)
        (fun () -> R.open_ key common_args)
    in
    { underlying; close = Lazy_deferred.create (fun () -> R.close underlying) }
  ;;

  let has_close_started t = Lazy_deferred.is_forced t.close
  let close t = Lazy_deferred.force_exn t.close
  let close_finished t = Lazy_deferred.wait_exn t.close
end
