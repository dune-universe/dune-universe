open! Core
open! Async
open! Import
include Heartbeat_pipe_rpc_intf

let make_serialization (type a) (module M : Stable with type t = a) version =
  (module struct
    open! Core.Core_stable

    module Stable_format = struct
      type t =
        { value : M.t
        ; version : int
        }
      [@@deriving bin_io, sexp]
    end

    include Make_stable.Of_stable_format.V1
        (Stable_format)
        (struct
          include M

          let to_stable_format value = { Stable_format.value; version }

          let of_stable_format
                { Stable_format.value; version = serialized_version }
            =
            assert (Core.Int.( = ) serialized_version version);
            value
          ;;
        end)
  end : Stable
    with type t = a)
;;

module type Register = functor
  (Version_i : sig
     type query = int [@@deriving bin_io]
     type response = unit [@@deriving bin_io]
     type error = Error.t [@@deriving bin_io]

     val version : int
     val model_of_query : query -> query
     val error_of_model : error -> error
     val response_of_model : response -> response
     val client_pushes_back : bool
   end)
  -> sig
    val rpc : (Version_i.query, Version_i.response, Version_i.error) Rpc.Pipe_rpc.t
  end

let register_version (module Register : Register) version =
  let module M =
    Register (struct
      open Core.Core_stable
      module Query = (val make_serialization (module Int.V1) version)
      module Response = (val make_serialization (module Unit.V1) version)
      module Error = (val make_serialization (module Error.V2) version)

      type query = Query.t [@@deriving bin_io, compare, sexp]
      type response = Response.t [@@deriving bin_io, compare, sexp]
      type error = Error.t [@@deriving bin_io, compare, sexp]

      let version = version
      let model_of_query = Fn.id
      let error_of_model = Fn.id
      let client_pushes_back = false
      let response_of_model = Fn.id
    end)
  in
  M.rpc
;;

module type S_make = sig
  type query = int [@@deriving of_sexp]
  type response = unit [@@deriving sexp_of]
  type error = Error.t [@@deriving sexp_of]

  include
    Versioned_rpc.Callee_converts.Pipe_rpc.S
    with type query := query
     and type response := response
     and type error := error

  module Register : Register
end

let make () =
  (module struct
    type query = int [@@deriving of_sexp]
    type response = unit [@@deriving sexp_of]
    type error = Error.t [@@deriving sexp_of]

    include Versioned_rpc.Callee_converts.Pipe_rpc.Make (struct
        let name = "heartbeat"

        type nonrec query = query
        type nonrec response = response
        type nonrec error = error
      end)
  end : S_make)
;;

let server ~min_version ~max_version =
  (module struct
    include (val make ())

    let () =
      assert (Int.( > ) max_version min_version);
      assert (Int.( > ) min_version 0);
      for version = min_version to max_version do
        ignore
          (register_version (module Register) version
           : (query, response, error) Rpc.Pipe_rpc.t)
      done
    ;;

    let implementation _invocation n =
      return (Ok (Pipe.of_list (List.init n ~f:(const ()))))
    ;;
  end : S)
;;

let client ~version =
  let module M = (val make ()) in
  register_version (module M.Register) version
;;
