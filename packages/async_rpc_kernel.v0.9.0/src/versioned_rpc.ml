open Core_kernel
open Async_kernel

open Rpc

let failed_conversion x =
  Error.create "type conversion failure" x
    [%sexp_of:
      [ `Msg | `Query | `Response | `Error | `State | `Update ]
      * [ `Rpc of string ]
      * [ `Version of int ]
      * exn
    ]

let multiple_registrations x =
  Error.create "multiple rpc registrations" x
    [%sexp_of: [ `Rpc of string ] * [ `Version of int ]]

let unknown_version x =
  Error.create "unknown rpc version" x
    [%sexp_of: string * int]

module Callee_converts = struct

  module Rpc = struct

    module Simple = struct
      type ('query, 'response) adapter =
        { adapt :
            'state . ('state -> 'query -> 'response Deferred.t) -> 'state Implementation.t
        }
      type ('query, 'response) t =
        { name     : string
        ; adapters : ('query, 'response) adapter Int.Map.t
        } [@@deriving fields]

      let create ~name = { name; adapters = Int.Map.empty }

      let wrap_error fn =
        (fun state query ->
           fn state query
           >>| function
           | Ok value -> Ok value
           | Error error -> Error (Error.to_string_hum error))

      let add { name; adapters } rpc adapter =
        if String.(<>) name (Rpc.name rpc)
        then Or_error.error "Rpc names don't agree" (name, Rpc.name rpc)
               [%sexp_of: string * string]
        else
          let version = Rpc.version rpc in
          match Map.find adapters version with
          | Some _ ->
            Or_error.error
              "Version already exists" (name, version) [%sexp_of: string * int]
          | None ->
            let adapters = Map.add adapters ~key:version ~data:adapter in
            Ok { name; adapters }

      let add_rpc_version t old_rpc upgrade downgrade =
        let adapt fn =
          let adapted state old_query =
            fn state (upgrade old_query)
            >>| fun result ->
            downgrade result
          in
          Rpc.implement old_rpc adapted
        in
        add t old_rpc { adapt }

      let add_rpc_version_with_failure t old_rpc upgrade_or_error downgrade_or_error =
        let adapt fn =
          let adapted state old_query =
            let open Deferred.Result.Monad_infix in
            return (upgrade_or_error old_query)
            >>= fun query ->
            fn state query
            >>= fun response ->
            return (downgrade_or_error response)
          in
          Rpc.implement old_rpc (wrap_error adapted)
        in
        add t old_rpc { adapt }

      let add_version t ~version ~bin_query ~bin_response upgrade downgrade =
        let rpc = Rpc.create ~name:t.name ~version ~bin_query ~bin_response in
        add_rpc_version t rpc upgrade downgrade

      let add_version_with_failure t ~version ~bin_query ~bin_response upgrade downgrade =
        let rpc = Rpc.create ~name:t.name ~version ~bin_query ~bin_response in
        add_rpc_version_with_failure t rpc upgrade downgrade

      let implement t fn =
        Map.data t.adapters
        |> List.map ~f:(fun { adapt } -> adapt fn)
    end

    module type S = sig
      type query
      type response
      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state -> version:int -> query -> response Deferred.t)
        -> 'state Implementation.t list
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
        val name : string
        type query
        type response
      end) = struct

      let name = Model.name

      type 's impl = 's -> version:int -> Model.query -> Model.response Deferred.t

      type implementer =
        { implement : 's. log_version:(int -> unit) -> 's impl -> 's Implementation.t }

      let registry : (int, implementer * Any.t) Hashtbl.t =
        Int.Table.create ~size:1 ()

      let implement_multi ?log_not_previously_seen_version f =
        let log_version =
          match log_not_previously_seen_version with
          | None -> ignore
          (* prevent calling [f] more than once per version *)
          | Some f -> Memo.general (f ~name)
        in
        List.map (Hashtbl.data registry) ~f:(fun (i, _rpc) -> i.implement ~log_version f)

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Hashtbl.keys registry)

      module Register (Version_i : sig
          type query [@@deriving bin_io]
          type response [@@deriving bin_io]
          val version : int
          val model_of_query : query -> Model.query
          val response_of_model : Model.response -> response
        end) = struct

        open Version_i

        let rpc = Rpc.create ~name ~version ~bin_query ~bin_response

        let () =
          let implement ~log_version f =
            Rpc.implement rpc (fun s q ->
              log_version version;
              match Result.try_with (fun () -> Version_i.model_of_query q) with
              | Error exn ->
                Error.raise
                  (failed_conversion (`Query, `Rpc name, `Version version, exn))
              | Ok q ->
                f s ~version q
                >>| fun r ->
                match Result.try_with (fun () -> Version_i.response_of_model r) with
                | Ok r -> r
                | Error exn ->
                  Error.raise
                    (failed_conversion (`Response, `Rpc name, `Version version, exn))
            )
          in
          match Hashtbl.find registry version with
          | None ->
            Hashtbl.set registry ~key:version ~data:({ implement }, Any.Rpc rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))
      end

    end

  end

  module Pipe_rpc = struct

    module type S = sig
      type query
      type response
      type error
      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state
            -> version:int
            -> query
            -> (response Pipe.Reader.t, error) Result.t Deferred.t)
        -> 'state Implementation.t list
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
      val name : string
      type query
      type response
      type error
    end) = struct

      let name = Model.name

      type 's impl =
        's
        -> version:int
        -> Model.query
        -> (Model.response Pipe.Reader.t, Model.error) Result.t Deferred.t

      type implementer =
        { implement : 's. log_version:(int -> unit) -> 's impl -> 's Implementation.t }

      let registry = Int.Table.create ~size:1 ()

      let implement_multi ?log_not_previously_seen_version f =
        let log_version =
          match log_not_previously_seen_version with
          | None -> ignore
          (* prevent calling [f] more than once per version *)
          | Some f -> Memo.general (f ~name)
        in
        List.map (Hashtbl.data registry) ~f:(fun (i, _) -> i.implement ~log_version f)

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Int.Table.keys registry)

      module type Version_shared = sig
        type query [@@deriving bin_io]
        type response [@@deriving bin_io]
        type error [@@deriving bin_io]
        val version : int
        val model_of_query : query -> Model.query
        val error_of_model : Model.error -> error
        val client_pushes_back : bool
      end

      module Register_raw (Version_i : sig
        include Version_shared
        val response_of_model : Model.response Pipe.Reader.t -> response Pipe.Reader.t
      end) = struct
        open Version_i

        let rpc =
          Pipe_rpc.create ~name ~version ~bin_query ~bin_response ~bin_error
            ?client_pushes_back:(Option.some_if client_pushes_back ())
            ()

        let () =
          let implement ~log_version f =
            Pipe_rpc.implement rpc (fun s q ->
              log_version version;
              match Version_i.model_of_query q with
              | exception exn ->
                Error.raise
                  (failed_conversion (`Response, `Rpc name, `Version version, exn))
              | q ->
                f s ~version q
                >>= function
                | Ok pipe ->
                  Monitor.handle_errors
                    (fun () -> return (Ok (Version_i.response_of_model pipe)))
                    (fun exn -> Error.raise (
                       failed_conversion (`Response, `Rpc name, `Version version, exn)))
                | Error error ->
                  return (match Version_i.error_of_model error with
                    | error -> Error error
                    | exception exn ->
                      Error.raise
                        (failed_conversion (`Error, `Rpc name, `Version version, exn)))
            )
          in
          match Hashtbl.find registry version with
          | None ->
            Hashtbl.set registry ~key:version ~data:({ implement }, Any.Pipe rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))
      end

      module Register (Version_i : sig
        include Version_shared
        val response_of_model : Model.response -> response
      end) = struct
        include Register_raw (struct
          include Version_i
          let response_of_model pipe = Pipe.map ~f:response_of_model pipe
        end)
      end
    end
  end

  module State_rpc = struct
    module type S = sig
      type query
      type state
      type update
      type error
      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('connection_state
            -> version:int
            -> query
            -> (state * update Pipe.Reader.t, error) Result.t Deferred.t)
        -> 'connection_state Implementation.t list
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
      val name : string
      type query
      type state
      type update
      type error
    end) = struct

      let name = Model.name

      type 's impl =
        's
        -> version:int
        -> Model.query
        -> (Model.state * Model.update Pipe.Reader.t, Model.error) Result.t Deferred.t

      type implementer =
        { implement : 's. log_version:(int -> unit) -> 's impl -> 's Implementation.t }

      let registry = Int.Table.create ~size:1 ()

      let implement_multi ?log_not_previously_seen_version f =
        let log_version =
          match log_not_previously_seen_version with
          | None -> ignore
          (* prevent calling [f] more than once per version *)
          | Some f -> Memo.general (f ~name)
        in
        List.map (Hashtbl.data registry) ~f:(fun (i, _) -> i.implement ~log_version f)

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Int.Table.keys registry)

      module type Version_shared = sig
        type query [@@deriving bin_io]
        type state [@@deriving bin_io]
        type update [@@deriving bin_io]
        type error [@@deriving bin_io]
        val version : int
        val model_of_query : query -> Model.query
        val state_of_model : Model.state -> state
        val error_of_model : Model.error -> error
        val client_pushes_back : bool
      end

      module Register_raw (Version_i : sig
        include Version_shared
        val update_of_model
          :  Model.state
          -> Model.update Pipe.Reader.t
          -> update Pipe.Reader.t
      end) = struct
        open Version_i

        let rpc =
          State_rpc.create ~name ~version ~bin_query ~bin_state ~bin_update ~bin_error
            ?client_pushes_back:(Option.some_if client_pushes_back ())
            ()

        let () =
          let implement ~log_version f =
            State_rpc.implement rpc (fun s q ->
              log_version version;
              match Version_i.model_of_query q with
              | exception exn ->
                Error.raise
                  (failed_conversion (`Response, `Rpc name, `Version version, exn))
              | q ->
                f s ~version q
                >>= function
                | Ok (model_state, pipe) ->
                  let state =
                    match Version_i.state_of_model model_state with
                    | state -> state
                    | exception exn ->
                      Error.raise
                        (failed_conversion (`State, `Rpc name, `Version version, exn))
                  in
                  Monitor.handle_errors
                    (fun () ->
                       return (Ok (state, Version_i.update_of_model model_state pipe)))
                    (fun exn -> Error.raise (
                       failed_conversion (`Update, `Rpc name, `Version version, exn)))
                | Error error ->
                  return (match Version_i.error_of_model error with
                    | error -> Error error
                    | exception exn ->
                      Error.raise
                        (failed_conversion (`Error, `Rpc name, `Version version, exn)))
            )
          in
          match Hashtbl.find registry version with
          | None -> Hashtbl.set registry ~key:version ~data:({ implement }, Any.State rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))
      end

      module Register (Version_i : sig
        include Version_shared
        val update_of_model : Model.update -> update
      end) = struct
        include Register_raw (struct
          include Version_i
          let update_of_model _state pipe = Pipe.map ~f:update_of_model pipe
        end)
      end
    end
  end

  module One_way = struct

    module type S = sig
      type msg
      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state -> version:int -> msg -> unit)
        -> 'state Implementation.t list
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
        val name : string
        type msg
      end) = struct

      let name = Model.name

      type 's impl = 's -> version:int -> Model.msg -> unit

      type implementer = {
        implement : 's. log_version:(int -> unit) -> 's impl -> 's Implementation.t;
      }

      let registry : (int, implementer * Any.t) Hashtbl.t =
        Int.Table.create ~size:1 ()

      let implement_multi ?log_not_previously_seen_version f =
        let log_version =
          match log_not_previously_seen_version with
          | None -> ignore
          (* prevent calling [f] more than once per version *)
          | Some f -> Memo.general (f ~name)
        in
        List.map (Hashtbl.data registry) ~f:(fun (i, _rpc) -> i.implement ~log_version f)

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Hashtbl.keys registry)

      module Register (Version_i : sig
          type msg [@@deriving bin_io]
          val version : int
          val model_of_msg : msg -> Model.msg
        end) = struct

        open Version_i

        let rpc = One_way.create ~name ~version ~bin_msg

        let () =
          let implement ~log_version f =
            One_way.implement rpc (fun s q ->
              log_version version;
              match Result.try_with (fun () -> Version_i.model_of_msg q) with
              | Error exn ->
                Error.raise (failed_conversion (`Msg, `Rpc name, `Version version, exn))
              | Ok q ->
                f s ~version q
            )
          in
          match Hashtbl.find registry version with
          | None -> Hashtbl.set registry ~key:version ~data:({implement}, Any.One_way rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))
      end

    end

  end

end

module Menu = struct

  (***************** some prohibitions for this module ******************

       (1) !!! never prune old versions of this rpc !!!

       It is too fundamental to the workings of various versioning
       schemes and it probably won't change very much anyway.

       (2) !!! only ever say "with bin_io" on built-in ocaml types !!!

       Examples of built-in types are int, list, string, etc.

       This is to protect ourselves against changes to Core data
       structures, for example.

   *********************************************************************)

  type rpc_name = string
  type rpc_version = int

  module Model = struct
    let name = "__Versioned_rpc.Menu"
    type query = unit
    type response = (rpc_name * rpc_version) list
  end

  include Callee_converts.Rpc.Make (Model)

  let rpc_name = Model.name

  module V1 = struct
    module T = struct
      let version = 1
      type query = unit [@@deriving bin_io]
      type response = (string * int) list [@@deriving bin_io]
      let model_of_query q = q
      let response_of_model r = r
    end
    include T
    include Register (T)
  end

  module Current_version = V1

  let add impls =
    let menu =
      List.map impls ~f:(fun i ->
        match Implementation.description i with
          { name; version } -> (name, version))
    in
    let menu_impls = implement_multi (fun _ ~version:_ () -> return menu) in
    impls @ menu_impls

  type t = Int.Set.t String.Table.t

  let supported_rpcs t =
    let open List.Monad_infix in
    String.Table.to_alist t
    >>= fun (name, versions) ->
    Int.Set.to_list versions
    >>| fun version ->
    { Description. name; version }

  let supported_versions t ~rpc_name =
    Option.value ~default:Int.Set.empty (Hashtbl.find t rpc_name)

  let of_entries entries =
    Hashtbl.map ~f:Int.Set.of_list (String.Table.of_alist_multi entries)

  let request conn =
    Rpc.dispatch Current_version.rpc conn ()
    >>| fun result ->
    Result.map result ~f:of_entries

  let create descriptions =
    List.map descriptions ~f:(fun { Description. name; version } ->
      (name, version))
    |> of_entries

end

module Connection_with_menu = struct

  type t =
    { connection : Connection.t
    ; menu       : Menu.t
    }
  [@@deriving fields]

  let create connection =
    let open Deferred.Or_error.Monad_infix in
    Menu.request connection
    >>| fun menu ->
    { connection; menu }

  let create_directly connection menu = {connection; menu}

end

module Caller_converts = struct

  let most_recent_common_version ~rpc_name ~caller_versions ~callee_versions =
    match Set.max_elt (Set.inter callee_versions caller_versions) with
    | Some version -> Ok version
    | None ->
      Or_error.error "caller and callee share no common versions for rpc"
        ( `Rpc rpc_name
        , `Caller_versions caller_versions
        , `Callee_versions callee_versions )
        [%sexp_of: [ `Rpc of string ]
                    * [ `Caller_versions of Int.Set.t ]
                    * [ `Callee_versions of Int.Set.t ]]

  module Dispatch = struct
    module Make (M : Monad) = struct
      open M
      let with_specific_version ~version ~connection ~name ~query ~registry =
        match Hashtbl.find registry version with
        | None -> return (Error (unknown_version (name, version)))
        | Some (dispatch, _rpc) -> dispatch connection query

      let with_version_menu { Connection_with_menu. connection; menu } query
            ~name ~versions ~registry =
        let callee_versions = Menu.supported_versions menu ~rpc_name:name in
        let caller_versions = versions () in
        match
          most_recent_common_version ~rpc_name:name ~caller_versions ~callee_versions with
        | Error e -> return (Error e)
        | Ok version -> with_specific_version ~version ~connection ~name ~query ~registry
    end
    module Async  = Make (Deferred)
    module Direct = Make (Monad.Ident)
  end

  module Rpc = struct

    module type S = sig
      type query
      type response
      val deprecated_dispatch_multi
        : version:int -> Connection.t -> query -> response Or_error.t Deferred.t

      val dispatch_multi
        : Connection_with_menu.t -> query -> response Or_error.t Deferred.t
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
        val name : string
        type query
        type response
      end) = struct

      let name = Model.name

      let registry = Int.Table.create ~size:1 ()

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Int.Table.keys registry)

      let dispatch_multi conn_with_menu query =
        Dispatch.Async.with_version_menu conn_with_menu query ~name ~versions ~registry

      let deprecated_dispatch_multi ~version connection query =
        Dispatch.Async.with_specific_version ~version ~connection ~query ~name ~registry

      module Register (Version_i : sig
          type query [@@deriving bin_io]
          type response [@@deriving bin_io]
          val version : int
          val query_of_model : Model.query -> query
          val model_of_response : response -> Model.response
        end) = struct

        open Version_i

        let rpc = Rpc.create ~name ~version ~bin_query ~bin_response

        let () =
          let dispatch conn q =
            match Result.try_with (fun () -> Version_i.query_of_model q) with
            | Error exn ->
              return
                (Error (failed_conversion (`Query, `Rpc name, `Version version, exn)))
            | Ok q ->
              Rpc.dispatch rpc conn q
              >>| fun result ->
              Result.bind result ~f:(fun r ->
                match Result.try_with (fun () -> Version_i.model_of_response r) with
                | Ok r -> Ok r
                | Error exn ->
                  Error (failed_conversion (`Response, `Rpc name, `Version version, exn)))
          in
          match Hashtbl.find registry version with
          | None -> Hashtbl.set registry ~key:version ~data:(dispatch, Any.Rpc rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))

      end

    end

  end

  module Pipe_rpc = struct

    module type S = sig
      type query
      type response
      type error

      val deprecated_dispatch_multi
        :  version:int
        -> Connection.t
        -> query
        -> ( response Or_error.t Pipe.Reader.t * Pipe_rpc.Metadata.t
           , error
           ) Result.t Or_error.t Deferred.t

      val dispatch_multi
        :  Connection_with_menu.t
        -> query
        -> ( response Or_error.t Pipe.Reader.t * Pipe_rpc.Metadata.t
           , error
           ) Result.t Or_error.t Deferred.t
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
      val name : string
      type query
      type response
      type error
    end) = struct

      let name = Model.name

      let registry = Int.Table.create ~size:1 ()

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Int.Table.keys registry)

      let dispatch_multi conn_with_menu query =
        Dispatch.Async.with_version_menu conn_with_menu query ~name ~versions ~registry

      let deprecated_dispatch_multi ~version connection query =
        Dispatch.Async.with_specific_version ~version ~connection ~query ~name ~registry

      module type Version_shared = sig
        type query [@@deriving bin_io]
        type response [@@deriving bin_io]
        type error [@@deriving bin_io]
        val version : int
        val query_of_model : Model.query -> query
        val model_of_error : error -> Model.error
        val client_pushes_back : bool
      end

      module Register_raw (Version_i : sig
        include Version_shared
        val model_of_response
          :  response Pipe.Reader.t
          -> Model.response Or_error.t Pipe.Reader.t
      end) = struct

        open Version_i

        let rpc =
          Pipe_rpc.create ~name ~version ~bin_query ~bin_response ~bin_error
            ?client_pushes_back:(Option.some_if client_pushes_back ())
            ()

        let () =
          let dispatch conn q =
            match Version_i.query_of_model q with
            | exception exn ->
              return
                (Error (failed_conversion (`Query, `Rpc name, `Version version, exn)))
            | q ->
              Pipe_rpc.dispatch rpc conn q
              >>| fun result ->
              match result with
              | Error exn -> Error exn
              | Ok (Error e) ->
                (match Version_i.model_of_error e with
                 | e' -> Ok (Error e')
                 | exception exn ->
                   Error (failed_conversion (`Error, `Rpc name, `Version version, exn)))
              | Ok (Ok (pipe, id)) ->
                Ok (Ok (Version_i.model_of_response pipe, id))
          in
          match Hashtbl.find registry version with
          | None -> Hashtbl.set registry ~key:version ~data:(dispatch, Any.Pipe rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))

      end

      module Register (Version_i : sig
        include Version_shared
        val model_of_response : response -> Model.response
      end) = struct
        include Register_raw (struct
          include Version_i

          let model_of_response rs =
            Pipe.map rs ~f:(fun r ->
              match Version_i.model_of_response r with
              | r -> Ok r
              | exception exn ->
                Error (failed_conversion (`Response, `Rpc name, `Version version, exn)))
        end)
      end

    end

  end

  module State_rpc = struct
    module type S = sig
      type query
      type state
      type update
      type error

      val dispatch_multi
        :  Connection_with_menu.t
        -> query
        -> ( state * update Or_error.t Pipe.Reader.t * State_rpc.Metadata.t
           , error
           ) Result.t Or_error.t Deferred.t
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
      val name : string
      type query
      type state
      type update
      type error
    end) = struct
      let name = Model.name

      let registry = Int.Table.create ~size:1 ()

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Int.Table.keys registry)

      let dispatch_multi conn_with_menu query =
        Dispatch.Async.with_version_menu conn_with_menu query ~name ~versions ~registry

      module type Version_shared = sig
        type query  [@@deriving bin_io]
        type state  [@@deriving bin_io]
        type update [@@deriving bin_io]
        type error  [@@deriving bin_io]
        val version : int
        val query_of_model : Model.query -> query
        val model_of_state : state -> Model.state
        val model_of_error : error -> Model.error
        val client_pushes_back : bool
      end

      module Register_raw (Version_i : sig
        include Version_shared
        val model_of_update
          :  update Pipe.Reader.t
          -> Model.update Or_error.t Pipe.Reader.t
      end) = struct

        open Version_i

        let rpc =
          State_rpc.create ~name ~version ~bin_query ~bin_state ~bin_update ~bin_error
            ?client_pushes_back:(Option.some_if client_pushes_back ()) ()

        let () =
          let dispatch conn q =
            match Version_i.query_of_model q with
            | exception exn ->
              return
                (Error (failed_conversion (`Query, `Rpc name, `Version version, exn)))
            | q ->
              State_rpc.dispatch rpc conn q
              >>| fun result ->
              match result with
              | Error exn -> Error exn
              | Ok (Error e) ->
                (match Version_i.model_of_error e with
                 | e' -> Ok (Error e')
                 | exception exn ->
                   Error (failed_conversion (`Error, `Rpc name, `Version version, exn)))
              | Ok (Ok (state, pipe, id)) ->
                match Version_i.model_of_state state with
                | exception exn ->
                  Error (failed_conversion (`State, `Rpc name, `Version version, exn))
                | state ->
                  Ok (Ok (state, Version_i.model_of_update pipe, id))
          in
          match Hashtbl.find registry version with
          | None -> Hashtbl.set registry ~key:version ~data:(dispatch, Any.State rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))
      end

      module Register (Version_i : sig
        include Version_shared
        val model_of_update : update -> Model.update
      end) = struct
        include Register_raw (struct
          include Version_i

          let model_of_update rs =
            Pipe.map rs ~f:(fun r ->
              match Version_i.model_of_update r with
              | r -> Ok r
              | exception exn ->
                Error (failed_conversion (`Update, `Rpc name, `Version version, exn)))
        end)
      end

    end

  end

  module One_way = struct

    module type S = sig
      type msg

      val dispatch_multi
        : Connection_with_menu.t -> msg -> unit Or_error.t
      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
        val name : string
        type msg
      end) = struct

      let name = Model.name

      let registry = Int.Table.create ~size:1 ()

      let rpcs () = List.map (Hashtbl.data registry) ~f:(fun (_, rpc) -> rpc)

      let versions () = Int.Set.of_list (Int.Table.keys registry)

      let dispatch_multi conn_with_menu msg =
        Dispatch.Direct.with_version_menu conn_with_menu msg ~name ~versions ~registry

      module Register (Version_i : sig
          type msg [@@deriving bin_io]
          val version : int
          val msg_of_model : Model.msg -> msg
        end) = struct

        open Version_i

        let rpc = One_way.create ~name ~version ~bin_msg

        let () =
          let dispatch conn q =
            match Result.try_with (fun () -> Version_i.msg_of_model q) with
            | Error exn ->
              Error (failed_conversion (`Msg, `Rpc name, `Version version, exn))
            | Ok q ->
              One_way.dispatch rpc conn q
          in
          match Hashtbl.find registry version with
          | None -> Hashtbl.set registry ~key:version ~data:(dispatch, Any.One_way rpc)
          | Some _ -> Error.raise (multiple_registrations (`Rpc name, `Version version))

      end

    end

  end


end

module Both_convert = struct

  module Plain = struct
    module type S = sig
      type caller_query
      type callee_query
      type caller_response
      type callee_response

      val dispatch_multi
        : Connection_with_menu.t -> caller_query -> caller_response Or_error.t Deferred.t

      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state -> version:int -> callee_query -> callee_response Deferred.t)
        -> 'state Implementation.t list

      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
        val name : string
        module Caller : sig type query type response end
        module Callee : sig type query type response end
      end) = struct
      open Model

      let name = name

      module Caller = Caller_converts.Rpc.Make (struct let name = name include Caller end)
      module Callee = Callee_converts.Rpc.Make (struct let name = name include Callee end)

      let%test _ = Int.Set.equal (Caller.versions ()) (Callee.versions ())

      module Register (Version : sig
          open Model
          val version : int
          type query    [@@deriving bin_io]
          type response [@@deriving bin_io]
          val query_of_caller_model : Caller.query -> query
          val callee_model_of_query : query -> Callee.query
          val response_of_callee_model : Callee.response -> response
          val caller_model_of_response : response -> Caller.response
        end) = struct
        include Callee.Register (struct
            include Version
            let model_of_query    = callee_model_of_query
            let response_of_model = response_of_callee_model
          end)
        include Caller.Register (struct
            include Version
            let query_of_model    = query_of_caller_model
            let model_of_response = caller_model_of_response
          end)
        let%test _ = Int.Set.equal (Caller.versions ()) (Callee.versions ())
      end

      let dispatch_multi = Caller.dispatch_multi

      let implement_multi = Callee.implement_multi

      (* Note: Caller.versions is the same as Callee.versions, so it doesn't matter which
         one we call here. Same for [rpcs]. *)
      let versions () = Caller.versions ()

      let rpcs () = Caller.rpcs ()
    end
  end

  module Pipe_rpc = struct
    module type S = sig
      type caller_query
      type callee_query
      type caller_response
      type callee_response
      type caller_error
      type callee_error

      val dispatch_multi
        :  Connection_with_menu.t
        -> caller_query
        -> ( caller_response Or_error.t Pipe.Reader.t * Pipe_rpc.Metadata.t
           , caller_error
           ) Result.t Or_error.t Deferred.t

      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state
            -> version : int
            -> callee_query
            -> (callee_response Pipe.Reader.t, callee_error) Result.t Deferred.t)
        -> 'state Implementation.t list

      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
        val name : string
        module Caller : sig type query type response type error end
        module Callee : sig type query type response type error end
      end) = struct
      open Model

      let name = name

      module Caller = Caller_converts.Pipe_rpc.Make (struct let name = name include Caller end)
      module Callee = Callee_converts.Pipe_rpc.Make (struct let name = name include Callee end)

      let%test _ = Int.Set.equal (Caller.versions ()) (Callee.versions ())

      module type Version_shared = sig
        val version : int
        type query    [@@deriving bin_io]
        type response [@@deriving bin_io]
        type error    [@@deriving bin_io]
        val query_of_caller_model : Model.Caller.query -> query
        val callee_model_of_query : query -> Model.Callee.query
        val error_of_callee_model : Model.Callee.error -> error
        val caller_model_of_error : error -> Model.Caller.error
        val client_pushes_back : bool
      end

      module Register_raw (Version_i : sig
          include Version_shared
          val response_of_callee_model
            : Model.Callee.response Pipe.Reader.t -> response Pipe.Reader.t
          val caller_model_of_response
            : response Pipe.Reader.t -> Model.Caller.response Or_error.t Pipe.Reader.t
        end) = struct
        include Callee.Register_raw (struct
            include Version_i
            let model_of_query    = callee_model_of_query
            let response_of_model = response_of_callee_model
            let error_of_model    = error_of_callee_model
          end)
        include Caller.Register_raw (struct
            include Version_i
            let query_of_model    = query_of_caller_model
            let model_of_response = caller_model_of_response
            let model_of_error    = caller_model_of_error
          end)
      end

      module Register (Version_i : sig
          include Version_shared
          val response_of_callee_model : Model.Callee.response -> response
          val caller_model_of_response : response -> Model.Caller.response
        end) = struct
        include Callee.Register (struct
            include Version_i
            let model_of_query    = callee_model_of_query
            let response_of_model = response_of_callee_model
            let error_of_model    = error_of_callee_model
          end)
        include Caller.Register (struct
            include Version_i
            let query_of_model    = query_of_caller_model
            let model_of_response = caller_model_of_response
            let model_of_error    = caller_model_of_error
          end)
      end

      let dispatch_multi = Caller.dispatch_multi

      let implement_multi = Callee.implement_multi

      let versions () = Caller.versions ()

      let rpcs () = Caller.rpcs ()
    end
  end

  module One_way = struct
    module type S = sig
      type caller_msg
      type callee_msg

      val dispatch_multi : Connection_with_menu.t -> caller_msg -> unit Or_error.t

      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state -> version:int -> callee_msg -> unit)
        -> 'state Implementation.t list

      val rpcs : unit -> Any.t list
      val versions : unit -> Int.Set.t
      val name : string
    end

    module Make (Model : sig
        val name : string
        module Caller : sig type msg end
        module Callee : sig type msg end
      end) =
    struct
      open Model

      let name = name

      module Caller =
        Caller_converts.One_way.Make (struct let name = name include Caller end)

      module Callee =
        Callee_converts.One_way.Make (struct let name = name include Callee end)

      let%test _ = Int.Set.equal (Caller.versions ()) (Callee.versions ())

      module Register (Version : sig
          open Model
          val version : int
          type msg [@@deriving bin_io]
          val msg_of_caller_model : Caller.msg -> msg
          val callee_model_of_msg : msg -> Callee.msg
        end) = struct
        include Callee.Register (struct
            include Version
            let model_of_msg = callee_model_of_msg
          end)
        include Caller.Register (struct
            include Version
            let msg_of_model = msg_of_caller_model
          end)
        let%test _ = Int.Set.equal (Caller.versions ()) (Callee.versions ())
      end

      let dispatch_multi = Caller.dispatch_multi

      let implement_multi = Callee.implement_multi

      (* Note: Caller.versions is the same as Callee.versions, so it doesn't matter which
         one we call here. Same for [rpcs]. *)
      let versions () = Caller.versions ()

      let rpcs () = Caller.rpcs ()
    end
  end
end
