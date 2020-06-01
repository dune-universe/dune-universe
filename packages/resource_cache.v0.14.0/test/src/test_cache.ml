open! Core
open! Async
open Expect_test_helpers_core

module Resource = struct
  module Key = Int
  module Common_args = Unit

  type t =
    { mutable status : [ `Open | `Closed ]
    ; close_finished : unit Ivar.t
    ; key : int
    ; id : int
    }
  [@@deriving fields]

  let counter = ref 0

  let open_ key () =
    let id = !counter in
    counter := !counter + 1;
    printf "Opening %d,%d\n" key id;
    Deferred.Or_error.return { status = `Open; close_finished = Ivar.create (); key; id }
  ;;

  let has_close_started t =
    match t.status with
    | `Open -> false
    | `Closed -> true
  ;;

  let close t =
    if has_close_started t
    then Deferred.unit
    else (
      printf "Closing %d,%d\n" t.key t.id;
      t.status <- `Closed;
      Ivar.fill t.close_finished ();
      Deferred.unit)
  ;;

  let close_finished t = Ivar.read t.close_finished
end

module Test_cache = struct
  include Resource_cache.Make (Resource) ()

  let init ~config k =
    Resource.counter := 0;
    init ~config ~log_error:(Log.Global.string ~level:`Error) k
  ;;
end

let get_resource ?(give_up = Deferred.never ()) ?load_balance ?r_ivar ~f t args =
  let%map result =
    Test_cache.with_any ~give_up ?load_balance t args ~f:(fun r ->
      printf "Got resource %d,%d\n" (Resource.key r) (Resource.id r);
      Option.iter r_ivar ~f:(fun r_ivar -> Ivar.fill r_ivar r);
      f r)
  in
  result
;;

let r_ivar_or_create r_ivar =
  match r_ivar with
  | None -> Ivar.create ()
  | Some r_ivar -> r_ivar
;;

let assert_no_resource_available ?give_up t args =
  match%map
    get_resource
      ?give_up
      ~f:(fun _ ->
        raise_s [%message "Asserted no resource available, but got a resource"])
      t
      args
  with
  | Ok (_, ()) -> raise_s [%message "asserted [with_] would return an error"]
  | Error e -> printf !"Error getting %{sexp: int list}: %{sexp:Error.t}\n" args e
;;

let assert_no_resource_available_now =
  assert_no_resource_available ~give_up:Deferred.unit
;;

module Open_resource : sig
  type t =
    { resource : Resource.t Deferred.t
    ; release : unit -> unit Deferred.t
    }

  val create : ?load_balance:bool -> ?now:bool -> Test_cache.t -> int list -> t
end = struct
  type t =
    { resource : Resource.t Deferred.t
    ; release : unit -> unit Deferred.t
    }

  let assert_resource_available
        ?(release = Deferred.unit)
        ?r_ivar
        ?give_up
        ?load_balance
        t
        args
    =
    let f r =
      let%bind () = release in
      printf "Releasing resource %d,%d\n" (Resource.key r) (Resource.id r);
      Deferred.unit
    in
    let r_ivar = r_ivar_or_create r_ivar in
    let%bind _, () = get_resource ~r_ivar ?give_up ?load_balance ~f t args >>| ok_exn in
    Ivar.read r_ivar
  ;;

  let create ?load_balance ?(now = false) t ids =
    let f =
      if now
      then assert_resource_available ?load_balance ~give_up:Deferred.unit
      else assert_resource_available ?load_balance ?give_up:None
    in
    let resource_ivar = Ivar.create () in
    let release_ivar = Ivar.create () in
    let released =
      f ~r_ivar:resource_ivar ~release:(Ivar.read release_ivar) t ids
      >>| (ignore : Resource.t -> unit)
    in
    let resource = Ivar.read resource_ivar in
    let release () =
      Ivar.fill release_ivar ();
      released
    in
    { resource; release }
  ;;
end

let close_and_flush t =
  printf "Closing cache\n";
  let closed_and_flushed =
    let%map () = Test_cache.close_and_flush t in
    printf "Closed cache\n"
  in
  match%map Clock_ns.with_timeout (Time_ns.Span.of_sec 1.) closed_and_flushed with
  | `Timeout -> printf "BUG: timeout waiting for [close_and_flush]\n"
  | `Result () -> ()
;;

let config =
  { Resource_cache.Config.max_resources = 2
  ; idle_cleanup_after = Time_ns.Span.day
  ; max_resources_per_id = 1
  ; max_resource_reuse = 2
  ; close_idle_resources_when_at_limit = false
  }
;;

let%expect_test "respect [max_resources_per_id]" =
  let t = Test_cache.init ~config () in
  (* Open a resource. *)
  let r0 = Open_resource.create ~now:true t [ 0 ] in
  let%bind (_ : Resource.t) = r0.resource in
  let%bind () = [%expect {|
      Opening 0,0
      Got resource 0,0
    |}] in
  (* Only 1 resource is allowed per id. *)
  let%bind () = assert_no_resource_available_now t [ 0 ] in
  let%bind () =
    [%expect {|
      Error getting (0): "Gave up waiting for resource"
    |}]
  in
  (* Release the original resource. *)
  let%bind () = r0.release () in
  let%bind () = [%expect {| Releasing resource 0,0 |}] in
  (* Now we can reuse the released resource. *)
  let r0' = Open_resource.create ~now:true t [ 0 ] in
  let%bind (_ : Resource.t) = r0'.resource in
  let%bind () = [%expect {| Got resource 0,0 |}] in
  (* Close on release because the [max_resource_reuse] is 2. *)
  let%bind () = r0'.release () in
  let%bind () = [%expect {|
      Releasing resource 0,0
      Closing 0,0
    |}] in
  (* Close and flush with no resources open. *)
  let%bind () = close_and_flush t in
  let%bind () = [%expect {|
      Closing cache
      Closed cache
    |}] in
  return ()
;;

let%expect_test "respect [max_resources]" =
  let t = Test_cache.init ~config () in
  (* Open a resource and release it immediately. *)
  let r0 = Open_resource.create ~now:true t [ 0 ] in
  let%bind r0_resource = r0.resource in
  let%bind () = r0.release () in
  let%bind () =
    [%expect
      {|
      Opening 0,0
      Got resource 0,0
      Releasing resource 0,0
    |}]
  in
  (* Open a resource with a different key and release it immediately. *)
  let r1 = Open_resource.create ~now:true t [ 1 ] in
  let%bind (_ : Resource.t) = r1.resource in
  let%bind () = r1.release () in
  let%bind () =
    [%expect
      {|
      Opening 1,1
      Got resource 1,1
      Releasing resource 1,1
    |}]
  in
  (* We can't open a resource with a different key because [max_resources] is 2. *)
  let%bind () = assert_no_resource_available_now t [ 2 ] in
  let%bind () =
    [%expect {|
      Error getting (2): "Gave up waiting for resource" |}]
  in
  (* Open a resource with a different key, without giving up. *)
  let r2 = Open_resource.create t [ 2 ] in
  (* Once we explicitly close a previous resource, we have capacity. *)
  let%bind () = Resource.close r0_resource in
  let%bind (_ : Resource.t) = r2.resource in
  let%bind () =
    [%expect {|
      Closing 0,0
      Opening 2,2
      Got resource 2,2 |}]
  in
  return ()
;;

let%expect_test "[with_any] respects order" =
  let t = Test_cache.init ~config () in
  (* Open any of [0; 1]. We should get 0 because [with_any] respects order. *)
  let r0 = Open_resource.create ~now:true t [ 0; 1 ] in
  let%bind (_ : Resource.t) = r0.resource in
  let%bind () = [%expect {|
      Opening 0,0
      Got resource 0,0 |}] in
  (* Open any of [0; 1]. We should get a resource 1 because there is already a 0 resource
     in use (and the limit per key is 1). *)
  let r1 = Open_resource.create ~now:true t [ 0; 1 ] in
  let%bind (_ : Resource.t) = r1.resource in
  let%bind () = [%expect {|
      Opening 1,1
      Got resource 1,1 |}] in
  (* Now, neither of [0; 1] are available immediately. *)
  let%bind () = assert_no_resource_available_now t [ 0; 1 ] in
  let%bind () = [%expect {| Error getting (0 1): "Gave up waiting for resource" |}] in
  (* Release resource 0. *)
  let%bind () = r0.release () in
  let%bind () = [%expect {| Releasing resource 0,0 |}] in
  (* Make sure we can now get resource 0. *)
  let r0' = Open_resource.create ~now:true t [ 0; 1 ] in
  let%bind (_ : Resource.t) = r0'.resource in
  let%bind () = [%expect {|
      Got resource 0,0 |}] in
  return ()
;;

let%expect_test "[f] raises" =
  let t = Test_cache.init ~config () in
  (* Open a resource and pass through an [f] that raises. Make sure the resource is
     closed. *)
  let%bind () =
    match%map
      Deferred.Or_error.try_with_join (fun () ->
        get_resource ~f:(fun _ -> failwith "failure") t [ 0 ])
    with
    | Ok (_r, _res) -> failwith "exn should have been caught"
    | Error e -> show_raise ~hide_positions:true (fun () -> Error.raise e)
  in
  let%bind () =
    [%expect
      {|
      Opening 0,0
      Got resource 0,0
      Closing 0,0
      (raised (monitor.ml.Error (Failure failure) ("<backtrace elided in test>"))) |}]
  in
  return ()
;;

let%expect_test "[f] raises to correct monitor" =
  let t =
    Test_cache.init
      ~config:
        { max_resources = 1
        ; max_resources_per_id = 1
        ; max_resource_reuse = 10
        ; idle_cleanup_after = Time_ns.Span.day
        ; close_idle_resources_when_at_limit = false
        }
      ()
  in
  let%bind r0 =
    match%map
      Monitor.try_with
        ~rest:(`Call (fun exn -> print_s [%message "unexpected exception" (exn : exn)]))
        (fun () ->
           let r0 = Open_resource.create ~now:true t [ 0 ] in
           let%map (_ : Resource.t) = r0.resource in
           r0)
    with
    | Ok r0 -> r0
    | Error exn -> raise exn
  in
  let%bind () = [%expect {|
    Opening 0,0
    Got resource 0,0 |}] in
  let deferred_result =
    Monitor.try_with ~name:"usage" (fun () ->
      get_resource ~f:(fun _ -> failwith "from within usage monitor") t [ 0 ])
  in
  let%bind () = r0.release () in
  let%bind () = [%expect {|
    Releasing resource 0,0
    Got resource 0,0 |}] in
  let%bind () =
    match%map deferred_result with
    | Ok _ -> failwith "an exception was expected"
    | Error e -> show_raise ~hide_positions:true (fun () -> raise e)
  in
  [%expect
    {|
    (raised (
      monitor.ml.Error
      (Failure "from within usage monitor")
      ("<backtrace elided in test>" "Caught by monitor usage"))) |}]
;;

let%expect_test "close_and_flush with nothing open" =
  let t = Test_cache.init ~config () in
  let%bind () = close_and_flush t in
  let%bind () = [%expect {|
      Closing cache
      Closed cache
    |}] in
  return ()
;;

let%expect_test "close_and_flush closes resources" =
  let t = Test_cache.init ~config () in
  (* Open a resource *)
  let r0 = Open_resource.create ~now:true t [ 0 ] in
  let%bind (_ : Resource.t) = r0.resource in
  let%bind () = [%expect {|
      Opening 0,0
      Got resource 0,0 |}] in
  (* Release the resource *)
  let%bind () = r0.release () in
  let%bind () = [%expect {|
      Releasing resource 0,0 |}] in
  (* [close_and_flush] should close the idle resource. *)
  let%bind () = close_and_flush t in
  let%bind () =
    [%expect {|
      Closing cache
      Closing 0,0
      Closed cache |}]
  in
  assert (Test_cache.close_started t);
  let%bind () = assert_no_resource_available_now t [ 0 ] in
  let%bind () = [%expect {|
      Error getting (0): "Cache is closed" |}] in
  let%bind () = assert_no_resource_available t [ 0 ] in
  let%bind () = [%expect {|
      Error getting (0): "Cache is closed" |}] in
  return ()
;;

let%expect_test "close_and_flush clears queue, waits for all jobs to finish" =
  let t = Test_cache.init ~config () in
  (* Open a resource *)
  let r0 = Open_resource.create ~now:true t [ 0 ] in
  let%bind (_ : Resource.t) = r0.resource in
  let%bind () = [%expect {|
      Opening 0,0
      Got resource 0,0 |}] in
  (* Start waiting for resource 0  *)
  let waiting_for_r0 = assert_no_resource_available t [ 0 ] in
  (* [close_and_flush] while there is a job waiting for a resource. *)
  let closed_and_flushed = close_and_flush t in
  assert (Test_cache.close_started t);
  (* The waiting job stops waiting. *)
  let%bind () = waiting_for_r0 in
  let%bind () =
    [%expect {|
      Closing cache
      Error getting (0): "Cache is closed" |}]
  in
  (* Release the open resource. Now, [closed_and_flushed] will become determined. *)
  let%bind () = r0.release () in
  let%bind () = closed_and_flushed in
  let%bind () =
    [%expect {|
      Releasing resource 0,0
      Closing 0,0
      Closed cache |}]
  in
  return ()
;;

let test_load_balance ~load_balance =
  let t =
    Test_cache.init
      ~config:
        { max_resources = 10
        ; max_resources_per_id = 5
        ; max_resource_reuse = 1
        ; idle_cleanup_after = Time_ns.Span.day
        ; close_idle_resources_when_at_limit = false
        }
      ()
  in
  let%bind () =
    List.init 10 ~f:(fun (_ : int) ->
      let r = Open_resource.create ~load_balance ~now:true t [ 0; 1; 2; 3 ] in
      let%bind (_ : Resource.t) = r.resource in
      r.release ())
    |> Deferred.all_unit
  in
  close_and_flush t
;;

let%expect_test "without load balancing" =
  let%bind () = test_load_balance ~load_balance:false in
  [%expect
    {|
    Opening 0,0
    Opening 0,1
    Opening 0,2
    Opening 0,3
    Opening 0,4
    Opening 1,5
    Opening 1,6
    Opening 1,7
    Opening 1,8
    Opening 1,9
    Got resource 0,0
    Got resource 0,1
    Got resource 0,2
    Got resource 0,3
    Got resource 0,4
    Got resource 1,5
    Got resource 1,6
    Got resource 1,7
    Got resource 1,8
    Got resource 1,9
    Releasing resource 0,0
    Releasing resource 0,1
    Releasing resource 0,2
    Releasing resource 0,3
    Releasing resource 0,4
    Releasing resource 1,5
    Releasing resource 1,6
    Releasing resource 1,7
    Releasing resource 1,8
    Releasing resource 1,9
    Closing 0,0
    Closing 0,1
    Closing 0,2
    Closing 0,3
    Closing 0,4
    Closing 1,5
    Closing 1,6
    Closing 1,7
    Closing 1,8
    Closing 1,9
    Closing cache
    Closed cache |}]
;;

let%expect_test "with load balancing" =
  let%bind () = test_load_balance ~load_balance:true in
  [%expect
    {|
    Opening 0,0
    Opening 1,1
    Opening 2,2
    Opening 3,3
    Opening 0,4
    Opening 1,5
    Opening 2,6
    Opening 3,7
    Opening 0,8
    Opening 1,9
    Got resource 0,0
    Got resource 1,1
    Got resource 2,2
    Got resource 3,3
    Got resource 0,4
    Got resource 1,5
    Got resource 2,6
    Got resource 3,7
    Got resource 0,8
    Got resource 1,9
    Releasing resource 0,0
    Releasing resource 1,1
    Releasing resource 2,2
    Releasing resource 3,3
    Releasing resource 0,4
    Releasing resource 1,5
    Releasing resource 2,6
    Releasing resource 3,7
    Releasing resource 0,8
    Releasing resource 1,9
    Closing 0,0
    Closing 1,1
    Closing 2,2
    Closing 3,3
    Closing 0,4
    Closing 1,5
    Closing 2,6
    Closing 3,7
    Closing 0,8
    Closing 1,9
    Closing cache
    Closed cache |}]
;;

let%expect_test "close idle resources when at limit" =
  let config =
    { Resource_cache.Config.max_resources = 2
    ; idle_cleanup_after = Time_ns.Span.day
    ; max_resources_per_id = 2
    ; max_resource_reuse = 2
    ; close_idle_resources_when_at_limit = true
    }
  in
  let t = Test_cache.init ~config () in
  (* Open a resource. *)
  let r0 = Open_resource.create ~now:true t [ 0 ] in
  let%bind r0_resource = r0.resource in
  let%bind () = [%expect {|
    Opening 0,0
    Got resource 0,0 |}] in
  (* Open another resource with the same key. *)
  let r0' = Open_resource.create ~now:true t [ 0 ] in
  let%bind r0'_resource = r0'.resource in
  let%bind () = [%expect {|
    Opening 0,1
    Got resource 0,1 |}] in
  (* Release the first resource. *)
  let%bind () = r0.release () in
  let%bind () = [%expect {|
    Releasing resource 0,0 |}] in
  (* Release the second resource. *)
  let%bind () = r0'.release () in
  let%bind () = [%expect {|
    Releasing resource 0,1 |}] in
  (* There are 2 idle resources. [r0] is the least recently used. Trying to get a
     resource with key 1 results in closing [r0] to make room. *)
  let r1 = Open_resource.create t [ 1 ] in
  let%bind () = Resource.close_finished r0_resource in
  let%bind (_ : Resource.t) = r1.resource in
  let%bind () = [%expect {|
    Closing 0,0
    Opening 1,2
    Got resource 1,2 |}] in
  (* Now we close [r0'] to make room. *)
  let r1' = Open_resource.create t [ 1 ] in
  let%bind () = Resource.close_finished r0'_resource in
  let%bind (_ : Resource.t) = r1'.resource in
  let%bind () = [%expect {|
    Closing 0,1
    Opening 1,3
    Got resource 1,3 |}] in
  return ()
;;
