open! Core
open! Async
open! Import
include Heartbeat_streamable_state_rpc_intf

module type Stable = sig
  type t [@@deriving compare]

  include Binable.S with type t := t
end

let make_serialization (type a) (module M : Stable with type t = a) version =
  (module struct
    open! Core.Core_stable

    module Stable_format = struct
      type t =
        { value : M.t
        ; version : int
        }
      [@@deriving bin_io]
    end

    type t = M.t [@@deriving compare]

    include Binable.Of_binable.V1 [@alert "-legacy"]
        (Stable_format)
        (struct
          include M

          let to_binable value = { Stable_format.value; version }

          let of_binable { Stable_format.value; version = serialized_version } =
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
     type update = unit
     type state = unit

     module State : Streamable.S_rpc with type t = state
     module Update : Streamable.S_rpc with type t = update

     val version : int
     val model_of_query : query -> query
     val update_of_model : update -> update
     val state_of_model : state -> state
     val client_pushes_back : bool
   end)
  -> sig
    val rpc : (Version_i.query, Version_i.state, Version_i.update) Streamable.State_rpc.t
  end

let register_version (module Register : Register) version =
  let module M =
    Register (struct
      open Core.Core_stable
      module Query = (val make_serialization (module Int.V1) version)

      module State =
        Streamable.Stable.Of_atomic_rpc.V1
          ((val make_serialization (module Unit.V1) version))

      module Update =
        Streamable.Stable.Of_atomic_rpc.V1
          ((val make_serialization (module Unit.V1) version))

      type query = Query.t [@@deriving bin_io, compare]
      type state = State.t
      type update = Update.t

      let version = version
      let model_of_query = Fn.id
      let state_of_model = Fn.id
      let update_of_model = Fn.id
      let client_pushes_back = false
    end)
  in
  M.rpc
;;

module type S_make = sig
  type query = int
  type initial_state = unit
  type update = unit

  include
    Streamable.Versioned_state_rpc.Callee_converts.S
    with type query := query
     and type update := update
     and type state := initial_state

  module Register : Register
end

let make () =
  (module struct
    type query = int
    type update = unit
    type initial_state = unit

    include Streamable.Versioned_state_rpc.Callee_converts.Make (struct
        let name = "heartbeat-streamable-state"

        type nonrec query = query
        type nonrec state = initial_state
        type nonrec update = update
      end)
  end : S_make)
;;

let server ~min_version ~max_version =
  let module M = struct
    include (val make ())

    let () =
      assert (Int.( > ) max_version min_version);
      assert (Int.( > ) min_version 0);
      for version = min_version to max_version do
        ignore
          (register_version (module Register) version
           : (query, initial_state, update) Streamable.State_rpc.t)
      done
    ;;

    let implementation _invocation n =
      return (Ok ((), Pipe.of_list (List.init n ~f:(const ()))))
    ;;
  end
  in
  M.implement_multi M.implementation
;;

let client ~version =
  let module M = (val make ()) in
  register_version (module M.Register) version
;;
