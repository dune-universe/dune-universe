open Core
open Async

module Raw = struct
  module Handle = struct
    type t = { release : unit -> unit Deferred.t }

    let create ~release = { release = Memo.unit release }
    let release { release } = release ()

    let check ~alert h =
      let released = ref false in
      let release () =
        released := true;
        release h
      in
      let result = create ~release in
      Gc.add_finalizer
        (Heap_block.create_exn result)
        (fun _ -> if not (!released) then alert ());
      result

    let noop = { release = fun () -> Deferred.return () }

    let%test_module _ = (module struct
      let%test_unit "check calls alert" =
        Async_unix.Thread_safe.block_on_async_exn (fun () ->
          let alerted = ref false in
          let _ = check ~alert:(fun () -> alerted := true) noop in
          Gc.compact ();
          Clock.after (Time.Span.of_ms 1.) >>= fun () ->
          [%test_result: bool] ~expect:true (!alerted);
          return ()
        )

      let%test_unit "check doesn't call alert" =
        Async_unix.Thread_safe.block_on_async_exn (fun () ->
          let alerted = ref false in
          let _ =
            let h = check ~alert:(fun () -> alerted := true) noop in
            release h
          in
          Gc.compact ();
          Clock.after (Time.Span.of_ms 1.) >>= fun () ->
          [%test_result: bool] ~expect:false (!alerted);
          return ()
        )
    end)
  end

  type ('a, 'e) t =
    { acquire : unit -> ('a * Handle.t, 'e) Result.t Deferred.t }

  let create acquire = { acquire }
  let acquire { acquire } = acquire ()
end

include Raw

let create' ~acquire ~release = create
  (fun () ->
    Deferred.Result.map (acquire ())
      ~f:(fun a -> (a, Handle.create ~release:(fun () -> release a))))

let with_ { acquire } ~f =
  acquire ()
  >>= function
  | Error e -> return (Error e)
  | Ok (r, h) ->
    Monitor.protect (fun () -> f r) ~finally:(fun () -> Handle.release h)
    >>= fun x -> return (Ok x)

let fail err = create (fun () -> return (Error err))

module Memo (Key : Hashable) = struct
  type 'a entry =
    { users : int
    ; result : 'a
    ; handle : Handle.t
    }

  let memo f =
    let module Sequencer_table = Sequencer_table.Make(Key) in
    let table : ('a entry, unit) Sequencer_table.t = Sequencer_table.create () in
    fun key ->
      let create_handle () =
        Handle.create ~release:(fun () ->
          Sequencer_table.enqueue table ~key (function
            | None -> failwith "tried to deallocate a resource that's already released"
            | Some { users; result; handle } ->
              let users = users - 1 in
              if Int.(=) users 0 then
                (Sequencer_table.set_state table ~key None;
                 Handle.release handle)
              else
                (Sequencer_table.set_state table ~key (
                   Some { users; result; handle });
                 Deferred.unit)
          ))
      in
      create (fun () ->
        Sequencer_table.enqueue table ~key (function
          | Some { users; result; handle } ->
            Sequencer_table.set_state table ~key
              (Some { users = users + 1; result; handle });
            return (Ok (result, create_handle ()))
          | None ->
            acquire (f key)
            >>= function
            | Error err -> return (Error err)
            | Ok (result, handle) ->
              Sequencer_table.set_state table ~key (
                Some { users = 1; result; handle });
              return (Ok (result, create_handle ()))
        ))
end

let shared t =
  let module MemoUnit = Memo(Unit) in
  MemoUnit.memo (fun () -> t) ()

let delayed_release t ~delay =
  create (fun () ->
    acquire t
    >>= function
    | Error err -> return (Error err)
    | Ok (r, h) -> return (Ok (r, Handle.create ~release:(fun () ->
      don't_wait_for (Clock.after delay >>= fun () -> Handle.release h);
      return ()
    )))
  )

let return resource =
  create (fun () -> Deferred.Result.return (resource, Handle.noop))

let map t ~f =
  create (fun () ->
    acquire t >>= function
    | Error err -> Deferred.return (Error err)
    | Ok (r, h) ->
      try Deferred.return (Ok (f r, h)) with
      | exn ->
        ( Handle.release h
          >>= fun () ->
          raise exn))

let map_error t ~f =
  create (fun () -> Deferred.Result.map_error (acquire t) ~f)

let bind t ~f =
  create (fun () ->
    acquire t
    >>= function
    | Error err -> Deferred.return (Error err)
    | Ok (r1, h1) ->
      (try acquire (f r1) with
       | exn ->
         Handle.release h1
         >>= fun () ->
         raise exn)
      >>= function
      | Error err ->
        Handle.release h1
        >>= fun () ->
        Deferred.return (Error err)
      | Ok (r2, h2) ->
        let handle =
          Handle.create ~release:(fun () ->
            Handle.release h2
            >>= fun () ->
            Handle.release h1)
        in
        Deferred.Result.return (r2, handle))

include Monad.Make2 (struct
  let return = return
  let map = `Custom map
  let bind = bind
  type nonrec ('a, 'e) t = ('a, 'e) t
end)

let%test_module _ = (module struct

  let res_return = return

  open Async_kernel
  open Async_unix

  let expect_fail resource =
    acquire resource
    >>= function
    | Ok _ -> failwith "counter should have failed"
    | Error () -> return ()

  let expect_succeed resource i =
    acquire resource >>|
    function
    | Ok (i', h) ->
      if i' = i
      then h
      else failwithf "counter said %d instead of %d" i' i ()
    | Error _ ->
      failwith "counter failed"

  let mk_counter ~limit =
    let maximum = ref 0 in
    let current = ref 0 in
    create (fun () -> Deferred.return (
      if !current >= limit
      then Error ()
      else
        begin
          current := !current + 1;
          maximum := max (!maximum) (!current);
          Ok (!current, Handle.create ~release:(fun () ->
            Deferred.return (current := !current - 1)
          ))
        end))

  let test_counter ~limit resource =
    Deferred.List.init limit ~f:(fun i -> expect_succeed resource (i + 1))
    >>= fun handles ->
    expect_fail resource
    >>= fun () ->
    match handles with
    | [] -> return ()
    | h1 :: handles ->
      Handle.release h1 >>= fun () ->
      expect_succeed resource limit
      >>= fun h1' ->
      expect_fail resource >>= fun () ->
      Handle.release h1 >>= fun () ->
      expect_fail resource >>= fun () ->
      Handle.release h1' >>= fun () ->
      Deferred.List.iter handles ~f:Handle.release
      >>= fun () -> expect_succeed resource 1
      >>= fun h ->
      Handle.release h

  let%test_unit "Counter works" =
    Thread_safe.block_on_async_exn (fun () ->
      let counter = (mk_counter ~limit:2) in
      test_counter ~limit:2 counter)

  let%test_unit "map ~f:Fn.id = Fn.id" =
    Thread_safe.block_on_async_exn (fun () ->
      let counter = (mk_counter ~limit:2) in
      let counter = map ~f:Fn.id counter in
      test_counter ~limit:2 counter)

  let%test_unit "map_error ~f:Fn.id = Fn.id" =
    Thread_safe.block_on_async_exn (fun () ->
      let counter = (mk_counter ~limit:2) in
      let counter = map_error ~f:Fn.id counter in
      test_counter ~limit:2 counter)

  let expect_async_fail f =
    Monitor.try_with f
    >>= function
    | Error _ -> return ()
    | Ok _ -> failwith "should have failed"

  let%test_unit "[map] recovers from exceptions"=
    Thread_safe.block_on_async_exn (fun () ->
      let counter = mk_counter ~limit:2 in
      let bad_counter = map ~f:(fun _ -> failwith "failed") counter in
      expect_async_fail (fun () -> acquire bad_counter)
      >>= fun () ->
      test_counter ~limit:2 counter)

  let%test_unit "[map_error] recovers from exceptions" =
    Thread_safe.block_on_async_exn (fun () ->
      let counter = mk_counter ~limit:2 in
      let bad_counter = map_error ~f:(fun _ -> failwith "failed") counter in
      expect_succeed bad_counter 1 >>= fun h1 ->
      expect_succeed bad_counter 2 >>= fun h2 ->
      expect_async_fail (fun () -> acquire bad_counter) >>= fun () ->
      Handle.release h1 >>= fun () ->
      Handle.release h2 >>= fun () ->
      test_counter ~limit:2 counter)

  let%test_unit "x >>= return = x" =
    Thread_safe.block_on_async_exn (fun () ->
      let resource1 = mk_counter ~limit:2 in
      let resource2 = bind resource1 ~f:res_return in
      test_counter ~limit:2 resource2
    )

  let%test_unit "return () >>= f = f ()" =
    Thread_safe.block_on_async_exn (fun () ->
      let resource1 = mk_counter ~limit:2 in
      let f () = resource1 in (* f must be side-effect-free, which mk_counter isn't *)
      let resource2 = bind (res_return ()) ~f in
      test_counter ~limit:2 resource2
    )


  let%test_unit "bind 2 3" =
    Thread_safe.block_on_async_exn (fun () ->
      let resource1 = mk_counter ~limit:2 in
      let resource2 = mk_counter ~limit:3 in
      let resource = bind resource1 ~f:(fun _n -> resource2) in
      test_counter ~limit:2 resource
      >>= fun () ->
      test_counter ~limit:2 resource
    )

  let%test_unit "bind 3 2" =
    Thread_safe.block_on_async_exn (fun () ->
      let resource1 = mk_counter ~limit:3 in
      let resource2 = mk_counter ~limit:2 in
      let resource = bind resource1 ~f:(fun _n -> resource2) in
      test_counter ~limit:2 resource
      >>= fun () ->
      test_counter ~limit:2 resource
    )

  let%test_unit "bind yourself" =
    Thread_safe.block_on_async_exn (fun () ->
      let resource1 = mk_counter ~limit:5 in
      let resource = bind resource1 ~f:(fun _n -> resource1) in
      test_counter ~limit:2 (map ~f:(fun x -> x / 2) resource)
    )

  let%test_unit "bind recovers from exceptions in f" =
    Thread_safe.block_on_async_exn (fun () ->
      let resource1 = mk_counter ~limit:3 in
      let resource2 = bind resource1 ~f:(fun _n -> failwith "failed") in
      expect_async_fail (fun () -> acquire resource2)
      >>= fun () ->
      test_counter ~limit:3 resource1
    )

  let%test_unit "bind recovers from exceptions in acquire" =
    Thread_safe.block_on_async_exn (fun () ->
      let resource1 = mk_counter ~limit:3 in
      let resource2 = bind resource1 ~f:(fun _n -> create (fun () -> failwith "failed")) in
      expect_async_fail (fun () -> acquire resource2)
      >>= fun () ->
      test_counter ~limit:3 resource1
    )

  let memo_key_supposed_to_work ~cnt_should_be memoed key =
    cnt_should_be 0;
    let res = memoed key in
    cnt_should_be 0;
    expect_succeed res 1
    >>= fun h_1 -> cnt_should_be 1;
    expect_succeed res 1
    >>= fun h_2 -> cnt_should_be 1;
    Handle.release h_1
    >>= fun () -> cnt_should_be 1;
    expect_succeed res 1
    >>= fun h_3 ->
    Handle.release h_2
    >>= fun () -> cnt_should_be 1;
    Handle.release h_3
    >>= fun () -> cnt_should_be 1;
    expect_succeed res 1
    >>= fun h_4 -> cnt_should_be 2;
    Handle.release h_4

  let memo_key_supposed_to_fail_once ~cnt_should_be memoed key =
    cnt_should_be 0;
    let res = memoed key in
    cnt_should_be 0;
    expect_fail res
    >>= fun () ->
    cnt_should_be 1;
    expect_succeed res 1
    >>= fun h ->
    cnt_should_be 2;
    Handle.release h

  let memo_key_supposed_to_fail ~cnt_should_be memoed key =
    cnt_should_be 0;
    let res = memoed key in
    cnt_should_be 0;
    expect_fail res
    >>= fun () ->
    cnt_should_be 1;
    expect_fail res
    >>= fun () ->
    cnt_should_be 2;
    return ()


  let memo_key_supposed_to_raise ~cnt_should_be memoed key =
    cnt_should_be 0;
    let res = memoed key in
    cnt_should_be 0;
    expect_async_fail (fun () -> acquire res)
    >>= fun () ->
    cnt_should_be 1;
    expect_async_fail (fun () -> acquire res)
    >>= fun () ->
    cnt_should_be 2;
    return ()

  let%test_unit "memo works" =
    Thread_safe.block_on_async_exn (fun () ->
      let module M = Memo(Int) in
      let cnt = ref 0 in
      let cnt_should_be n = [%test_result: int] ~expect:n (!cnt) in
      let counter0 = mk_counter ~limit:1 in
      let counter1 = mk_counter ~limit:1 in
      let failed1 = ref false in
      let memoed =
        M.memo (
          fun x ->
            incr cnt;
            match x with
            | 0 -> counter0
            | 1 -> (match !failed1 with
              | false ->
                failed1 := true;
                fail ()
              | true ->
                counter1)
            | 2 -> fail ()
            | _ -> failwith "unknown key!"
        )
      in
      cnt := 0;
      memo_key_supposed_to_work ~cnt_should_be memoed 0
      >>= fun () ->
      cnt := 0;
      memo_key_supposed_to_fail_once ~cnt_should_be memoed 1
      >>= fun () ->
      cnt := 0;
      memo_key_supposed_to_fail ~cnt_should_be memoed 2
      >>= fun () ->
      cnt := 0;
      memo_key_supposed_to_raise ~cnt_should_be memoed 3
    )

  let%test_unit "with_ succeed" =
    Thread_safe.block_on_async_exn (fun () ->
      let res = mk_counter ~limit:1 in
      with_ res ~f:(fun n ->
        expect_fail res
        >>= fun () ->
        return n)
      >>= fun result ->
      [%test_result: (int, unit) Result.t] ~expect:(Ok 1) result;
      test_counter ~limit:1 res
    )

  let%test_unit "with_ fail" =
    Thread_safe.block_on_async_exn (fun () ->
      let res = mk_counter ~limit:0 in
      with_ res ~f:(fun n -> return n)
      >>= fun res ->
      [%test_result: (int, unit) Result.t] ~expect:(Error ()) res;
      return ()
    )

  let%test_unit "with_ raise" =
    Thread_safe.block_on_async_exn (fun () ->
      let res = mk_counter ~limit:1 in
      expect_async_fail (fun () ->
        with_ res ~f:(fun _n -> failwith "failed")
      ) >>= fun () ->
      test_counter ~limit:1 res
    )
end)

let create = create'
