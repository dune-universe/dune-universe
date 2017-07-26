open Core
open Import

let debug_on_find_state = ref ignore

module Make (Key : Hashable) = struct
  module Tag = struct
    type 'job_tag t =
      | User_job of 'job_tag option
      | Prior_jobs_done
    [@@deriving sexp]
  end

  module Job = struct
    type ('state, 'job_tag) t =
      { tag : 'job_tag Tag.t
      ; run : ('state option -> unit Deferred.t) sexp_opaque
      }

    let sexp_of_t _ sexp_of_job_tag t = t.tag |> [%sexp_of: job_tag Tag.t]
  end

  type ('state, 'job_tag) t =
    { states : 'state Key.Table.t
    (* We use a [Queue.t] and implement the [Throttle.Sequencer] functionality ourselves,
       because throttles don't provide a way to get notified when they are empty, and we
       need to remove the table entry for an emptied throttle. *)
    ; jobs   : ('state, 'job_tag) Job.t Queue.t Key.Table.t
    }
  [@@deriving sexp_of, fields]

  let create () =
    { states = Key.Table.create ()
    ; jobs   = Key.Table.create ()
    }
  ;;

  let rec run_jobs_until_none_remain t ~key (queue : (_, _) Job.t Queue.t) =
    match Queue.peek queue with
    | None -> Hashtbl.remove t.jobs key
    | Some job ->
      (* The state of [key] is found and fed to [job] immediately; there should be no
         deferred in between. *)
      let state = Hashtbl.find t.states key in
      !debug_on_find_state ();
      job.run state >>> fun () ->
      assert (phys_equal (Queue.dequeue_exn queue) job);
      run_jobs_until_none_remain t ~key queue;
  ;;

  let set_state t ~key = function
    | None       -> Hashtbl.remove  t.states  key
    | Some state -> Hashtbl.set t.states ~key ~data:state
  ;;

  let enqueue t ~key ?tag f =
    Deferred.create (fun ivar ->
      (* when job is called, [f] is invoked immediately, there shall be no deferred in
         between *)
      let run state_opt =
        Monitor.try_with ~run:`Now (fun () -> f state_opt) >>| Ivar.fill ivar
      in
      let job = { Job. tag = Tag.User_job tag; run } in
      match Hashtbl.find t.jobs key with
      | Some queue -> Queue.enqueue queue job
      | None ->
        let queue = Queue.create () in
        Queue.enqueue queue job;
        Hashtbl.set t.jobs ~key ~data:queue;
        (* never start a job in the same async job *)
        upon Deferred.unit (fun () ->
          run_jobs_until_none_remain t ~key queue);
    )
    >>| function
    | Error exn -> raise (Monitor.extract_exn exn)
    | Ok res -> res
  ;;

  let find_state t key = Hashtbl.find t.states key

  let num_unfinished_jobs t key =
    match Hashtbl.find t.jobs key with
    | None -> 0
    | Some queue -> Queue.length queue
  ;;

  let mem t key = Hashtbl.mem t.states key || Hashtbl.mem t.jobs key

  let fold t ~init ~f =
    let all_keys =
      Key.Hash_set.create ~size:(Hashtbl.length t.jobs + Hashtbl.length t.states) ()
    in
    Hashtbl.iteri t.jobs   ~f:(fun ~key ~data:_ -> Hash_set.add all_keys key);
    Hashtbl.iteri t.states ~f:(fun ~key ~data:_ -> Hash_set.add all_keys key);
    Hash_set.fold all_keys ~init ~f:(fun acc key ->
      f acc ~key (Hashtbl.find t.states key))
  ;;

  let prior_jobs_done t =
    Hashtbl.fold t.jobs ~init:[] ~f:(fun ~key:_ ~data:queue acc ->
      let this_key_done =
        Deferred.create (fun ivar ->
          Queue.enqueue queue
            { tag = Tag.Prior_jobs_done
            ; run = (fun _ -> Ivar.fill ivar (); Deferred.unit)
            })
      in
      this_key_done :: acc)
    |> Deferred.all_unit
  ;;
end

let%test_module _ =
  (module struct
    module T = Make(Int)

    let (=) = Pervasives.(=)

    exception Abort of int

    let%test_unit _ =
      (* don't run a job immediately *)
      Thread_safe.block_on_async_exn (fun () ->
        let t = T.create () in
        let i = ref 0 in
        let res = T.enqueue t ~key:0 (fun _ -> incr i; Deferred.unit) in
        assert (!i = 0);
        res >>| fun () ->
        assert (!i = 1)
      )

    let%test_unit _ =
      (* no deferred between finding state and running the job *)
      (* let [enqueue] function, when [Monitor.try_with] did not take [~run:`Now], then this
         unit test failed to pass *)
      Thread_safe.block_on_async_exn (fun () ->
        let t = T.create () in
        let i = ref `init in
        debug_on_find_state := (fun () ->
          Deferred.unit >>> fun () -> i := `deferred_determined
        );
        T.enqueue t ~key:0 (fun _ ->
          [%test_eq: [`init | `deferred_determined]] !i `init;
          Deferred.unit
        )
        >>| fun () ->
        debug_on_find_state := ignore
      )

    let%test_unit _ =
      Thread_safe.block_on_async_exn (fun () ->
        let t = T.create () in
        let num_keys = 100 in
        let keys = List.init num_keys ~f:Fn.id in
        let started_jobs = Queue.create () in
        let enqueue key x =
          Monitor.try_with (fun () ->
            T.enqueue t ~key (fun state ->
              let state =
                match state with
                | None -> [x]
                | Some xs -> x::xs
              in
              T.set_state t ~key (Some state);
              Queue.enqueue started_jobs key;
              Clock.after (sec 0.01) >>| fun () ->
              (* check continue on error *)
              raise (Abort key)
            )
          )
          >>| function
          | Error exn ->
            begin match Monitor.extract_exn exn with
            | (Abort i) when i = key -> ()
            | _ -> assert false
            end
          | _ -> assert false
        in
        Deferred.List.iter keys ~how:`Parallel ~f:(fun key ->
          Deferred.List.iter ['a'; 'b'; 'c' ] ~how:`Parallel ~f:(enqueue key)
        )
        >>| fun () ->
        List.iter keys ~f:(fun key ->
          (* check [find_state] *)
          (* check jobs are sequentialized for the same key *)
          assert (T.find_state t key = Some ['c'; 'b'; 'a'])
        );
        (* check jobs on different keys can run concurrently *)
        let started_jobs_in_batched =
          List.groupi (Queue.to_list started_jobs)
            ~break:(fun i _ _ -> i mod num_keys = 0)
        in
        List.iter started_jobs_in_batched ~f:(fun l ->
          assert (List.sort l ~cmp:Int.compare = keys)
        );
      )
    ;;

    let%test_unit _ =
      (* Test [num_unfinished_jobs] *)
      Thread_safe.block_on_async_exn (fun () ->
        let t = T.create () in
        assert (T.num_unfinished_jobs t 0 = 0);
        let job1 =
          T.enqueue t ~key:0 (fun _ ->
            assert (T.num_unfinished_jobs t 0 = 3); Deferred.unit)
        in
        let job2 =
          T.enqueue t ~key:0 (fun _ ->
            assert (T.num_unfinished_jobs t 0 = 2); Deferred.unit)
        in
        let job3 =
          T.enqueue t ~key:0 (fun _ ->
            assert (T.num_unfinished_jobs t 0 = 1); Deferred.unit)
        in
        assert (T.num_unfinished_jobs t 0 = 3);
        Deferred.all_unit [job1; job2; job3] >>| fun () ->
        assert (T.num_unfinished_jobs t 0 = 0)
      )

    let%test_unit _ =
      (* Test [mem] *)
      Thread_safe.block_on_async_exn (fun () ->
        let t = T.create () in
        (* empty *)
        assert (T.mem t 0 = false);
        let job = T.enqueue t ~key:0 (fun _ -> Deferred.unit) in
        (* with job *)
        assert (T.mem t 0);
        job >>= fun () ->
        (* without job *)
        assert (T.mem t 0 = false);
        (* with state *)
        T.set_state t ~key:0 (Some 'a');
        assert (T.mem t 0);
        T.set_state t ~key:0 None;
        (* without state *)
        assert (T.mem t 0 = false);
        let job =
          T.set_state t ~key:0 (Some 'a');
          T.enqueue t ~key:0 (fun _ -> Deferred.unit)
        in
        (* with job and state *)
        assert (T.mem t 0);
        job >>| fun () ->
        (* without job but with state *)
        assert (T.mem t 0)
      )
    ;;

    let%test_unit _ = (* enqueueing within a job doesn't lead to monitor nesting *)
      Thread_safe.block_on_async_exn (fun () ->
        let t = T.create () in
        let rec loop n =
          if n = 0
          then Deferred.unit
          else
            T.enqueue t ~key:13 (fun _ ->
              assert (Monitor.depth (Monitor.current ()) < 5);
              don't_wait_for (loop (n - 1));
              Deferred.unit)
        in
        loop 100)
    ;;

    let%test_unit _ = (* [flushed] is determined after all current jobs are finished *)
      Thread_safe.block_on_async_exn (fun () ->
        let t = T.create () in
        let phase1_finished = Ivar.create () in
        let phase2_finished = Ivar.create () in
        let num_flushed = 2 in
        let num_not_flushed = 2 in
        for i = 1 to num_flushed do
          don't_wait_for (T.enqueue t ~key:i (fun _ -> Ivar.read phase1_finished));
        done;
        let phase1_flushed = T.prior_jobs_done t in
        for i = 1 to num_flushed + num_not_flushed do
          don't_wait_for (T.enqueue t ~key:i (fun _ -> Ivar.read phase2_finished));
        done;
        for i = 1 to num_flushed do
          (* two jobs we enqueued, and one job [flush] added *)
          assert (T.num_unfinished_jobs t i = 3)
        done;
        for i = num_flushed + 1 to num_flushed + num_not_flushed do
          assert (T.num_unfinished_jobs t i = 1)
        done;
        Ivar.fill phase1_finished ();
        phase1_flushed
        >>= fun () ->
        for i = 1 to num_flushed + num_not_flushed do
          assert (T.num_unfinished_jobs t i = 1)
        done;
        Ivar.fill phase2_finished ();
        T.prior_jobs_done t
        >>| fun () ->
        for i = 1 to num_flushed + num_not_flushed do
          assert (T.num_unfinished_jobs t i = 0)
        done;
      )
    ;;
  end)
