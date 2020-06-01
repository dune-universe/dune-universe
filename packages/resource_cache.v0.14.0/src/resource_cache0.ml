module Stable = struct
  open! Core_kernel.Core_kernel_stable

  module Resource = struct
    module V1 = struct
      type t =
        { state : [ `Busy | `Closing | `Idle ]
        ; since : Time_ns.Span.V2.t
        }
      [@@deriving sexp, bin_io]
    end
  end

  module type Key = sig
    type t [@@deriving sexp, bin_io]
  end

  module Resource_list = struct
    module V1 = struct
      type 'key t =
        { key : 'key
        ; resources : Resource.V1.t list
        ; queue_length : int
        ; max_time_on_queue : Time_ns.Span.V2.t option
        }
      [@@deriving sexp, bin_io]
    end
  end

  module Status = struct
    module V1 = struct
      type 'key t =
        { resource_lists : 'key Resource_list.V1.t list
        ; num_jobs_in_cache : int
        }
      [@@deriving sexp, bin_io]
    end
  end
end

open! Core_kernel
open! Async_kernel
open! Import
include Resource_cache_intf

module Make_wrapped (R : Resource.S_wrapped) () = struct
  module Status = struct
    module Key = R.Key

    module Resource = struct
      type state =
        [ `Busy
        | `Idle
        | `Closing
        ]
      [@@deriving sexp_of, compare]

      type t = Stable.Resource.V1.t =
        { state : state
        ; since : Time_ns.Span.t
        }
      [@@deriving fields, sexp_of, compare]
    end

    module Resource_list = struct
      type 'key t_ = 'key Stable.Resource_list.V1.t =
        { key : 'key
        ; resources : Resource.t list
        ; queue_length : int
        ; max_time_on_queue : Time_ns.Span.t option
        }
      [@@deriving fields, sexp_of, compare]

      type t = Key.t t_ [@@deriving sexp_of, compare]
    end

    type 'key t_ = 'key Stable.Status.V1.t =
      { resource_lists : 'key Resource_list.t_ list
      ; num_jobs_in_cache : int
      }
    [@@deriving fields, sexp_of, compare]

    type t = Key.t t_ [@@deriving sexp_of, compare]

    module Make_stable = struct
      module V1 (Key : Stable.Key with type t = Key.t) = struct
        type t = Key.t Stable.Status.V1.t [@@deriving sexp, bin_io]
      end
    end
  end

  module Delayed_failures = struct
    type t =
      [ `Error_opening_resource of R.Key.t * Error.t
      | `Cache_is_closed
      ]
  end

  module Job : sig
    type 'a t

    val create
      :  ?open_timeout:Time_ns.Span.t
      -> give_up:unit Deferred.t
      -> f:(R.t -> 'a Deferred.t)
      -> 'a t

    (* Use [has_result t] instead of [Deferred.is_determined (result t)] to prevent a race
       condition. It is possible that the result ivar was filled but [result] is not yet
       determined. *)

    val has_result : _ t -> bool

    val result
      :  'a t
      -> [ `Ok of R.Key.t * 'a | `Gave_up_waiting_for_resource | Delayed_failures.t ]
           Deferred.t

    val f : 'a t -> R.t -> 'a Deferred.t
    val open_timeout : 'a t -> Time_ns.Span.t option
    val created_at : 'a t -> Time_ns.t

    val mark_result_from_available_resource
      :  'a t
      -> R.Key.t
      -> result:'a Deferred.t
      -> unit

    val mark_result_from_resource_creation
      :  'a t
      -> result:[ `Ok of R.Key.t * 'a
                | Delayed_failures.t
                | (* This case is not possible, but the compiler gets mad otherwise *)
                  `Gave_up_waiting_for_resource
                ]
                  Deferred.t
      -> unit

    val mark_cache_closed : 'a t -> unit
  end = struct
    type 'a t =
      { f : R.t -> 'a Deferred.t
      ; result_ivar :
          [ `Ok of R.Key.t * 'a | `Gave_up_waiting_for_resource | Delayed_failures.t ]
            Deferred.t
            Ivar.t
      ; open_timeout : Time_ns.Span.t option
      ; created_at : Time_ns.t
      }
    [@@deriving fields]

    let create ?open_timeout ~give_up ~f =
      let result_ivar = Ivar.create () in
      (* This [Job.t] is placed into a queue and then executed later. The execution of [f]
         occurs inside a different async execution context. We need to preserve the async
         context from the original call. *)
      let f = Monitor.Exported_for_scheduler.preserve_execution_context' f |> unstage in
      upon give_up (fun () ->
        Ivar.fill_if_empty result_ivar (return `Gave_up_waiting_for_resource));
      { f; result_ivar; open_timeout; created_at = Time_ns.now () }
    ;;

    let mark_result_from_available_resource t args ~result =
      Ivar.fill
        t.result_ivar
        (let%map res = result in
         `Ok (args, res))
    ;;

    let mark_result_from_resource_creation t ~result = Ivar.fill t.result_ivar result
    let mark_cache_closed t = Ivar.fill_if_empty t.result_ivar (return `Cache_is_closed)
    let has_result t = Ivar.is_full t.result_ivar

    let result t =
      let%bind result = Ivar.read t.result_ivar in
      result
    ;;
  end

  (* [Resource] wraps [R] taking care that uses of [with_] don't cross paths, and that
     [close] and [close_finished] are well behaved.

     It will trigger [close] once the [max_resource_reuse] or [idle_cleanup_after] are
     exceeded. *)
  module Resource : sig
    module Id : Unique_id.Id

    module State : sig
      type t =
        [ `Idle
        | `In_use_until of unit Ivar.t
        | `Closing
        ]
    end

    type t

    val id : t -> Id.t

    (* [create] will immediately produce a [Resource.t] that is initially
       busy with:
       - calling [R.open_]
       - calling [immediate ~f:with_] with the resource created (if successful)

       If [R.open_] fails, this resource is immediately closed
       otherwise the resource will become idle after the initial use.

       @see [immediate]. *)

    val create
      :  ?open_timeout:Time_ns.Span.t
      -> ?on_state_update:(t -> State.t -> unit)
      -> Config.t
      -> R.Key.t
      -> R.Common_args.t
      -> with_:(R.t -> 'a Deferred.t)
      -> log_error:(string -> unit)
      -> t * [> `Ok of R.Key.t * 'a | Delayed_failures.t ] Deferred.t

    val status : t -> Status.Resource.t

    (* [close_when_idle] forces the resource to shutdown either now or when the currently
       running [f] completes *)

    val close_when_idle : t -> unit Deferred.t

    (* [close_finished] becomes determined when this [Resource] has been closed.
       We guarantee that this will become determined, even if the underlying
       resource implementation is not well behaved. *)

    val close_finished : t -> unit Deferred.t

    (* Aquire an exclusive lock on this resource and call [f]. If [f] fails, or if the
       number of calls exceeds [max_resource_reuse] this resource will be closed.
       Otherwise this resource will be marked as idle and will close if not used again
       within a predefined timeout. *)

    val immediate
      :  t
      -> f:(R.t -> 'a Deferred.t)
      -> [ `Ok of 'a Deferred.t
         | `Resource_unavailable_until of unit Deferred.t
         | `Resource_closed
         ]
  end = struct
    module Id = Unique_id.Int ()

    module State = struct
      type t =
        [ `Idle
        | `In_use_until of unit Ivar.t
        | `Closing
        ]
    end

    type t =
      { id : Id.t
      ; key : R.Key.t
      ; args : R.Common_args.t
      ; resource : R.t Set_once.t
      ; mutable state : State.t
      ; mutable in_state_since : Time_ns.t
      ; config : Config.t
      ; mutable remaining_uses : int
      ; close_finished : unit Ivar.t
      ; on_state_update : (t -> State.t -> unit) option
      ; log_error : string -> unit
      }

    let id t = t.id

    let status t =
      let state =
        match t.state with
        | `Idle -> `Idle
        | `In_use_until _ -> `Busy
        | `Closing -> `Closing
      in
      { Status.Resource.state; since = Time_ns.diff (Time_ns.now ()) t.in_state_since }
    ;;

    let set_state t state =
      Option.iter t.on_state_update ~f:(fun f -> f t state);
      t.state <- state;
      t.in_state_since <- Time_ns.now ()
    ;;

    let close_finished t = Ivar.read t.close_finished

    let close t =
      let really_close () =
        set_state t `Closing;
        let closed =
          match Set_once.get t.resource with
          | None -> Deferred.unit
          | Some r ->
            (match%map
               Monitor.try_with (fun () ->
                 if R.has_close_started r then Deferred.unit else R.close r)
             with
             | Ok () -> ()
             | Error exn ->
               t.log_error (sprintf !"Exception closing resource: %{Exn}" exn))
        in
        match%map Clock_ns.with_timeout (Time_ns.Span.of_sec 10.) closed with
        | `Result () | `Timeout -> Ivar.fill t.close_finished ()
      in
      match t.state with
      | `Closing -> close_finished t
      | `Idle -> really_close ()
      | `In_use_until done_ ->
        assert (not (Ivar.is_full done_));
        close_finished t >>> Ivar.fill done_;
        really_close ()
    ;;

    let close_when_idle t =
      match t.state with
      | `Closing -> close_finished t
      | `Idle -> close t
      | `In_use_until _ ->
        (* This will trigger a [close] when the current task completes. *)
        t.remaining_uses <- 0;
        close_finished t
    ;;

    let set_idle t =
      match t.state with
      | `Closing -> failwith "Impossible, can't set a closed resource to idle"
      | `Idle -> failwith "Impossible, already marked as idle"
      | `In_use_until done_ ->
        assert (Ivar.is_empty done_);
        if t.remaining_uses <= 0
        then don't_wait_for (close t)
        else (
          set_state t `Idle;
          Ivar.fill done_ ();
          Clock_ns.after t.config.idle_cleanup_after
          >>> fun () ->
          match t.state with
          | `Closing | `In_use_until _ -> ()
          | `Idle ->
            let idle_time = Time_ns.diff (Time_ns.now ()) t.in_state_since in
            if Time_ns.Span.( >= ) idle_time t.config.idle_cleanup_after
            then don't_wait_for (close t))
    ;;

    let unsafe_immediate t ~f =
      match t.state with
      | `Closing -> failwith "Can't [unsafe_immediate] a closed resource"
      | `Idle -> failwith "Can't [unsafe_immediate] an idle resource"
      | `In_use_until done_ ->
        assert (Ivar.is_empty done_);
        assert (t.remaining_uses > 0);
        t.remaining_uses <- t.remaining_uses - 1;
        (* deliberately not filling [done_] here.
           It is filled in [set_idle] or [close]. *)
        (match%map
           Monitor.try_with (fun () -> f (Set_once.get_exn t.resource [%here]))
         with
         | Ok res ->
           set_idle t;
           res
         | Error exn ->
           don't_wait_for (Deferred.ignore_m (close t));
           raise exn)
    ;;

    let immediate t ~f =
      match t.state with
      | `Closing -> `Resource_closed
      | `In_use_until done_ -> `Resource_unavailable_until (Ivar.read done_)
      | `Idle ->
        (* It is possible that [R.close] was called but [R.close_finished] is not
           determined yet. Use [R.is_closed] to prevent this race. *)
        if R.has_close_started (Set_once.get_exn t.resource [%here])
        then `Resource_closed
        else (
          set_state t (`In_use_until (Ivar.create ()));
          `Ok (unsafe_immediate t ~f))
    ;;

    let create ?open_timeout ?on_state_update config key args ~with_ ~log_error =
      let t =
        { id = Id.create ()
        ; key
        ; args
        ; resource = Set_once.create ()
        ; state = `In_use_until (Ivar.create ())
        ; in_state_since = Time_ns.now ()
        ; config
        ; remaining_uses = config.Config.max_resource_reuse
        ; close_finished = Ivar.create ()
        ; on_state_update
        ; log_error
        }
      in
      let res =
        match%bind
          Deferred.Or_error.try_with_join (fun () ->
            match open_timeout with
            | None -> R.open_ key args
            | Some timeout ->
              let resource_ivar = Ivar.create () in
              (match%map
                 Clock_ns.with_timeout
                   timeout
                   (let%map r = R.open_ key args in
                    Ivar.fill resource_ivar r;
                    r)
               with
               | `Result r -> r
               | `Timeout ->
                 (* In case we timeout, make sure we cleanup after ourselves *)
                 (Ivar.read resource_ivar
                  >>> function
                  | Error _ -> ()
                  | Ok r -> don't_wait_for (R.close r));
                 Or_error.error_string "Exceeded open timeout while creating resource"))
        with
        | Ok res ->
          (* A call to [close_and_flush] might have occurred *)
          if t.remaining_uses > 0
          then (
            don't_wait_for
              (let%bind () = R.close_finished res in
               close_when_idle t);
            Set_once.set_exn t.resource [%here] res;
            let%map r = unsafe_immediate t ~f:with_ in
            `Ok (key, r))
          else return `Cache_is_closed
        | Error err ->
          (* Ensure [close_finished] gets filled *)
          don't_wait_for (close t);
          return (`Error_opening_resource (key, err))
      in
      t, res
    ;;
  end

  module Idle_resource_tracker : sig
    type t

    val create : unit -> t
    val close_least_recently_used : t -> unit

    val on_resource_state_update
      :  t
      -> (Resource.t -> [ `Idle | `In_use_until of unit Ivar.t | `Closing ] -> unit)
           Staged.t
  end = struct
    module Lru = Hash_queue.Make (Resource.Id)

    type t = Resource.t Lru.t

    let create () = Lru.create ()

    let enqueue t resource =
      let id = Resource.id resource in
      match Lru.enqueue_back t id resource with
      | `Ok -> ()
      | `Key_already_present ->
        (* This shouldn't happen, but this is the logical thing to do. *)
        ignore (Lru.lookup_and_move_to_back_exn t id : Resource.t)
    ;;

    let remove t resource =
      match Lru.remove t (Resource.id resource) with
      | `Ok -> ()
      | `No_such_key ->
        (* This can occur because [close_least_recently_used] removes from the queue and
           a subsequent state update comes later. *)
        ()
    ;;

    let close_least_recently_used t =
      (* Explicitly dequeue so another call to [close_least_recently_used] before the
         resource's [on_state_update] fires will choose a different resource. *)
      match Lru.dequeue_front t with
      | Some resource ->
        don't_wait_for (Resource.close_when_idle resource);
        ()
      | None -> ()
    ;;

    let on_resource_state_update t =
      stage (fun resource ->
        function
        | `Idle -> enqueue t resource
        | `In_use_until _ | `Closing -> remove t resource)
    ;;
  end

  (* Limit the number of concurrent [Resource.t]s globally *)
  module Global_resource_limiter : sig
    type t

    val create : Config.t -> t

    (* create a single resource, and block a slot until the resource has been cleaned
       up *)

    val create_resource
      :  ?open_timeout:Time_ns.Span.t
      -> t
      -> R.Key.t
      -> R.Common_args.t
      -> with_:(R.t -> 'a Deferred.t)
      -> log_error:(string -> unit)
      -> [ `Ok of Resource.t * [> `Ok of R.Key.t * 'a | Delayed_failures.t ] Deferred.t
         | `Cache_is_closed
         | `No_resource_available_until of unit Deferred.t
         ]

    val maybe_close_least_recently_used : t -> unit
    val close_and_flush : t -> unit Deferred.t
  end = struct
    type t =
      { config : Config.t
      ; idle_resource_tracker : Idle_resource_tracker.t option
      ; throttle : unit Throttle.t
      }

    let create config =
      { config
      ; idle_resource_tracker =
          (if config.close_idle_resources_when_at_limit
           then Some (Idle_resource_tracker.create ())
           else None)
      ; throttle =
          Throttle.create
            ~continue_on_error:true
            ~max_concurrent_jobs:config.max_resources
      }
    ;;

    let capacity_available_now t =
      Throttle.num_jobs_running t.throttle < Throttle.max_concurrent_jobs t.throttle
    ;;

    let create_resource ?open_timeout t key args ~with_ ~log_error =
      if Throttle.is_dead t.throttle
      then `Cache_is_closed
      else if capacity_available_now t
      then (
        assert (Throttle.num_jobs_waiting_to_start t.throttle = 0);
        let on_state_update =
          Option.map t.idle_resource_tracker ~f:(fun tracker ->
            unstage (Idle_resource_tracker.on_resource_state_update tracker))
        in
        let r, v =
          Resource.create
            ?open_timeout
            ?on_state_update
            t.config
            key
            args
            ~with_
            ~log_error
        in
        don't_wait_for
          (Throttle.enqueue t.throttle (fun () -> Resource.close_finished r));
        `Ok (r, v))
      else
        `No_resource_available_until
          (Deferred.any
             [ Throttle.capacity_available t.throttle; Throttle.cleaned t.throttle ])
    ;;

    let maybe_close_least_recently_used t =
      match t.idle_resource_tracker with
      | None -> ()
      | Some tracker ->
        if not (capacity_available_now t)
        then Idle_resource_tracker.close_least_recently_used tracker
    ;;

    let close_and_flush t =
      Throttle.kill t.throttle;
      Throttle.cleaned t.throttle
    ;;
  end

  (* Limit the number of concurrent [Resource.t]s locally *)
  module Resource_list : sig
    type t

    val create
      :  Config.t
      -> Global_resource_limiter.t
      -> R.Key.t
      -> R.Common_args.t
      -> log_error:(string -> unit)
      -> t

    val status : t -> Status.Resource_list.t

    (* [is_empty] is true iff there are no currently connected/connecting resources. *)

    val is_empty : t -> bool

    (* [close_and_flush'] will mark this resource list for removal and start tearing down
       all its resources. *)

    val close_and_flush' : t -> unit

    (* [close_finished] becomes determined after [close_and_flush'] was called and all
       resources have been closed. *)

    val close_finished : t -> unit Deferred.t

    (* [find_available_resource] and [create_resource] can be used to bypass [enqueue] in
       the case where there is an idle resource or an available slot. *)

    val find_available_resource
      :  t
      -> f:(R.t -> 'a Deferred.t)
      -> [ `Immediate of 'a Deferred.t | `None_until of unit Deferred.t ]

    val create_resource
      :  ?open_timeout:Time_ns.Span.t
      -> t
      -> f:(R.t -> 'a Deferred.t)
      -> [> `Ok of R.Key.t * 'a | Delayed_failures.t ] Deferred.t option

    val enqueue : t -> 'a Job.t -> unit
    val num_open : t -> int
  end = struct
    type job = T : 'a Job.t -> job

    type t =
      { config : Config.t
      ; key : R.Key.t
      ; args : R.Common_args.t
      ; global_resource_limiter : Global_resource_limiter.t
      ; resources : Resource.t Resource.Id.Table.t
      ; waiting_jobs : job Queue.t
      ; trigger_queue_manager : unit Mvar.Read_write.t
      ; mutable close_started : bool
      ; close_finished : unit Ivar.t
      ; log_error : string -> unit
      }

    let num_open t = Hashtbl.length t.resources

    let status t =
      let max_time_on_queue =
        Queue.peek t.waiting_jobs
        |> Option.map ~f:(fun (T job) ->
          Time_ns.diff (Time_ns.now ()) (Job.created_at job))
      in
      { Status.Resource_list.key = t.key
      ; resources = Hashtbl.data t.resources |> List.map ~f:Resource.status
      ; queue_length = Queue.length t.waiting_jobs
      ; max_time_on_queue
      }
    ;;

    let find_available_resource t ~f =
      With_return.with_return (fun { return } ->
        let until =
          Hashtbl.fold t.resources ~init:[] ~f:(fun ~key:_ ~data:r until ->
            match Resource.immediate r ~f with
            | `Ok r -> return (`Immediate r)
            | `Resource_unavailable_until u -> u :: until
            | `Resource_closed -> until)
        in
        `None_until (Deferred.any until))
    ;;

    let create_resource ?open_timeout t ~f =
      if Hashtbl.length t.resources >= t.config.max_resources_per_id
      then None
      else (
        match
          Global_resource_limiter.create_resource
            ?open_timeout
            t.global_resource_limiter
            t.key
            t.args
            ~with_:f
            ~log_error:t.log_error
        with
        | `Cache_is_closed -> None
        | `No_resource_available_until u ->
          (* Trigger when there is global capacity available *)
          upon u (Mvar.set t.trigger_queue_manager);
          None
        | `Ok (resource, response) ->
          Hashtbl.add_exn t.resources ~key:(Resource.id resource) ~data:resource;
          (Resource.close_finished resource
           >>> fun () ->
           Hashtbl.remove t.resources (Resource.id resource);
           (* Trigger that capacity is now available *)
           Mvar.set t.trigger_queue_manager ();
           if t.close_started && Hashtbl.is_empty t.resources
           then Ivar.fill t.close_finished ());
          (* Trigger when this resource is now available. This is needed because
             [create_resource] is called from outside this module *)
          upon response (fun _ -> Mvar.set t.trigger_queue_manager ());
          Some response)
    ;;

    let allocate_resources t =
      let rec loop () =
        match Queue.peek t.waiting_jobs with
        | None -> ()
        | Some (T job) ->
          (* Skip if this job has a result already *)
          if Job.has_result job
          then (
            let (_ : _) = Queue.dequeue_exn t.waiting_jobs in
            loop ())
          else (
            match find_available_resource t ~f:(Job.f job) with
            | `Immediate result ->
              Job.mark_result_from_available_resource job t.key ~result;
              let (_ : _) = Queue.dequeue_exn t.waiting_jobs in
              loop ()
            | `None_until until ->
              (* Trigger when a resource is available *)
              upon until (Mvar.set t.trigger_queue_manager);
              (match
                 create_resource ?open_timeout:(Job.open_timeout job) t ~f:(Job.f job)
               with
               | Some result ->
                 Job.mark_result_from_resource_creation job ~result;
                 let (_ : _) = Queue.dequeue_exn t.waiting_jobs in
                 loop ()
               | None -> ()))
      in
      loop ()
    ;;

    let start_background_resource_allocator t =
      let rec loop () =
        let%bind () = Mvar.take t.trigger_queue_manager in
        if t.close_started
        then (
          Queue.iter t.waiting_jobs ~f:(fun (T job) -> Job.mark_cache_closed job);
          Queue.clear t.waiting_jobs;
          Deferred.unit)
        else (
          allocate_resources t;
          loop ())
      in
      loop ()
    ;;

    let create config global_resource_limiter key args ~log_error =
      let t =
        { config
        ; key
        ; args
        ; global_resource_limiter
        ; resources = Resource.Id.Table.create ()
        ; waiting_jobs = Queue.create ()
        ; trigger_queue_manager = Mvar.create ()
        ; close_started = false
        ; close_finished = Ivar.create ()
        ; log_error
        }
      in
      don't_wait_for (start_background_resource_allocator t);
      t
    ;;

    let enqueue t job =
      Queue.enqueue t.waiting_jobs (T job);
      (* Trigger that a new job is on the queue *)
      Mvar.set t.trigger_queue_manager ();
      upon (Job.result job) (fun _ ->
        Queue.filter_inplace t.waiting_jobs ~f:(fun (T job') ->
          not (phys_same job job'));
        (* Trigger that a resource is now available *)
        Mvar.set t.trigger_queue_manager ())
    ;;

    let is_empty t = Hashtbl.is_empty t.resources && Queue.is_empty t.waiting_jobs
    let close_finished t = Ivar.read t.close_finished

    let close_and_flush' t =
      if not t.close_started
      then (
        t.close_started <- true;
        if Hashtbl.is_empty t.resources
        then Ivar.fill t.close_finished ()
        else (
          Mvar.set t.trigger_queue_manager ();
          Hashtbl.iter t.resources ~f:(fun r ->
            don't_wait_for (Resource.close_when_idle r))))
    ;;
  end

  type t =
    { config : Config.t
    ; global_resource_limiter : Global_resource_limiter.t
    ; cache : Resource_list.t R.Key.Table.t
    ; args : R.Common_args.t
    ; mutable num_jobs_in_cache : int
    ; mutable close_started : bool
    ; close_finished : unit Ivar.t
    ; log_error : string -> unit
    }

  let status t =
    let resource_lists = List.map (Hashtbl.data t.cache) ~f:Resource_list.status in
    { Status.resource_lists; num_jobs_in_cache = t.num_jobs_in_cache }
  ;;

  let get_resource_list t key =
    Hashtbl.find_or_add t.cache key ~default:(fun () ->
      Resource_list.create
        t.config
        t.global_resource_limiter
        key
        t.args
        ~log_error:t.log_error)
  ;;

  let find_any_available_resource t keys ~f =
    List.find_map keys ~f:(fun key ->
      let res_list = get_resource_list t key in
      match Resource_list.find_available_resource res_list ~f with
      | `Immediate res -> Some (key, res)
      | `None_until _ -> None)
  ;;

  let create_any_resource ?open_timeout ~load_balance t keys ~f =
    let keys =
      if load_balance
      then (
        let num_open key =
          let res_list = get_resource_list t key in
          Resource_list.num_open res_list
        in
        List.stable_sort keys ~compare:(fun key1 key2 ->
          Int.compare (num_open key1) (num_open key2)))
      else keys
    in
    List.find_map keys ~f:(fun key ->
      let res_list = get_resource_list t key in
      Resource_list.create_resource ?open_timeout res_list ~f)
  ;;

  let enqueue_all ?open_timeout t ~give_up keys ~f =
    let job = Job.create ?open_timeout ~give_up ~f in
    List.iter keys ~f:(fun key ->
      let res_list = get_resource_list t key in
      Resource_list.enqueue res_list job);
    Job.result job
  ;;

  let with_any'
        ?open_timeout
        ?(give_up = Deferred.never ())
        ?(load_balance = false)
        t
        keys
        ~f
    =
    let f resource = f (R.underlying resource) in
    t.num_jobs_in_cache <- t.num_jobs_in_cache + 1;
    let result =
      if t.close_started
      then return `Cache_is_closed
      else (
        match find_any_available_resource t keys ~f with
        | Some (args, res) ->
          let%map res = res in
          `Ok (args, res)
        | None ->
          (match create_any_resource ?open_timeout ~load_balance t keys ~f with
           | Some res -> res
           | None ->
             Global_resource_limiter.maybe_close_least_recently_used
               t.global_resource_limiter;
             if Deferred.is_determined give_up
             then return `Gave_up_waiting_for_resource
             else enqueue_all ?open_timeout ~give_up t keys ~f))
    in
    upon result (fun _ -> t.num_jobs_in_cache <- t.num_jobs_in_cache - 1);
    result
  ;;

  let with_any ?open_timeout ?give_up ?load_balance t keys ~f =
    match%map with_any' ?open_timeout ?give_up ?load_balance t keys ~f with
    | `Ok args_and_res -> Ok args_and_res
    | `Error_opening_resource (key, err) ->
      let tag = sprintf !"Error creating required resource: %{sexp:R.Key.t}" key in
      Error (Error.tag ~tag err)
    | `Cache_is_closed -> Or_error.error_string "Cache is closed"
    | `Gave_up_waiting_for_resource ->
      Or_error.error_string "Gave up waiting for resource"
  ;;

  let with_ ?open_timeout ?give_up t key ~f =
    match%map with_any ?open_timeout ?give_up t [ key ] ~f with
    | Ok (_args, res) -> Ok res
    | Error e -> Error e
  ;;

  let with_' ?open_timeout ?give_up t key ~f =
    match%map with_any' ?open_timeout ?give_up t [ key ] ~f with
    | `Ok (_args, res) -> `Ok res
    | `Error_opening_resource (_args, err) -> `Error_opening_resource err
    | `Cache_is_closed -> `Cache_is_closed
    | `Gave_up_waiting_for_resource -> `Gave_up_waiting_for_resource
  ;;

  let with_any_loop ?open_timeout ?give_up ?load_balance t keys ~f =
    let rec loop ~failed = function
      | [] -> return (`Error_opening_all_resources (List.rev failed))
      | keys ->
        (match%bind with_any' ?open_timeout ?give_up ?load_balance t keys ~f with
         | (`Ok _ | `Gave_up_waiting_for_resource | `Cache_is_closed) as res -> return res
         | `Error_opening_resource (failed_key, e) ->
           let remaining =
             List.filter keys ~f:(fun key -> not (R.Key.equal key failed_key))
           in
           loop ~failed:((failed_key, e) :: failed) remaining)
    in
    loop ~failed:[] keys
  ;;

  let init ~config ~log_error args =
    let t =
      { config
      ; global_resource_limiter = Global_resource_limiter.create config
      ; cache = R.Key.Table.create ()
      ; args
      ; num_jobs_in_cache = 0
      ; close_started = false
      ; close_finished = Ivar.create ()
      ; log_error
      }
    in
    Clock_ns.every
      ~stop:(Ivar.read t.close_finished)
      config.idle_cleanup_after
      (fun () ->
         Hashtbl.filter_inplace t.cache ~f:(fun d ->
           if Resource_list.is_empty d
           then (
             Resource_list.close_and_flush' d;
             false)
           else true));
    t
  ;;

  let close_and_flush t =
    if not t.close_started
    then (
      t.close_started <- true;
      let%map () =
        Deferred.all_unit
          (Global_resource_limiter.close_and_flush t.global_resource_limiter
           :: List.map (Hashtbl.data t.cache) ~f:(fun r ->
             Resource_list.close_and_flush' r;
             Resource_list.close_finished r))
      in
      Ivar.fill t.close_finished ())
    else Ivar.read t.close_finished
  ;;

  let config t = t.config
  let close_started t = t.close_started
  let close_finished t = Ivar.read t.close_finished
end

module Make (R : Resource.S) () = struct
  include Make_wrapped
      (struct
        include R

        type resource = t

        let underlying t = t
      end)
      ()
end

module Make_simple (R : Resource.Simple) () = struct
  include Make_wrapped
      (struct
        include Resource.Make_simple (R)
      end)
      ()
end
