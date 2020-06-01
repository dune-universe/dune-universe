open! Core
open! Async
open! Import
include Heartbeat_state_rpc_intf

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
     type update = unit [@@deriving bin_io]
     type state = unit [@@deriving bin_io]
     type error = Error.t [@@deriving bin_io]

     val version : int
     val model_of_query : query -> query
     val error_of_model : error -> error
     val update_of_model : update -> update
     val state_of_model : state -> state
     val client_pushes_back : bool
   end)
  -> sig
    val rpc
      : ( Version_i.query
        , Version_i.state
        , Version_i.update
        , Version_i.error )
          Rpc.State_rpc.t
  end

let register_version (module Register : Register) version =
  let module M =
    Register (struct
      open Core.Core_stable
      module Query = (val make_serialization (module Int.V1) version)
      module State = (val make_serialization (module Unit.V1) version)
      module Update = (val make_serialization (module Unit.V1) version)
      module Error = (val make_serialization (module Error.V2) version)

      type query = Query.t [@@deriving bin_io, compare]
      type state = State.t [@@deriving bin_io, compare]
      type update = Update.t [@@deriving bin_io, compare]
      type error = Error.t [@@deriving bin_io, compare]

      let version = version
      let model_of_query = Fn.id
      let state_of_model = Fn.id
      let error_of_model = Fn.id
      let update_of_model = Fn.id
      let client_pushes_back = false
    end)
  in
  M.rpc
;;

module Make () = struct
  type query = int
  type update = unit
  type initial_state = unit
  type error = Error.t

  include Versioned_rpc.Callee_converts.State_rpc.Make (struct
      let name = "heartbeat-state"

      type nonrec query = query
      type nonrec state = initial_state
      type nonrec update = update
      type nonrec error = error
    end)
end

let server ~min_version ~max_version =
  let module X = Make () in
  let () =
    assert (Int.( > ) max_version min_version);
    assert (Int.( > ) min_version 0);
    for version = min_version to max_version do
      ignore
        (register_version (module X.Register) version
         : (X.query, X.initial_state, X.update, X.error) Rpc.State_rpc.t)
    done
  in
  X.implement_multi (fun _invocation ~version:_ n ->
    return (Ok ((), Pipe.of_list (List.init n ~f:(const ())))))
;;

let client ~version =
  let module M = Make () in
  register_version (module M.Register) version
;;
