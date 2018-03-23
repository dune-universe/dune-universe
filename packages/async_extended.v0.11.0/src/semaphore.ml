open Core
open Async

type t =
  { mutable value: int;
    waiters: unit Ivar.t Queue.t;
  }
[@@deriving fields, sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~value:(check (fun value -> assert (value >= 0)))
      ~waiters:(check (fun waiters ->
        if t.value > 0 then assert (Queue.is_empty waiters))))
;;

let create init =
  if init <= 0 then
    failwiths "Semaphore.create got a nonpositive value" init [%sexp_of: int];
  { value = init;
    waiters = Queue.create ();
  }
;;

let incr t =
  match Queue.dequeue t.waiters with
  | None -> t.value <- t.value + 1
  | Some i -> Ivar.fill i ()
;;

let decr t =
  if t.value > 0 then begin
    assert (Queue.length t.waiters = 0);
    t.value <- t.value - 1;
    return ()
  end else begin
    let i = Ivar.create () in
    Queue.enqueue t.waiters i;
    Ivar.read i
  end
;;

let resource t =
  Resource.create
    ~acquire:(fun () -> decr t >>| fun () -> Ok ())
    ~release:(fun () -> incr t; return ())

let%test_module _ = (module struct
  let stabilize = Async_kernel_scheduler.run_cycles_until_no_jobs_remain

  (* check if we go to sleep on negative counts *)
  let%test_unit _ =
    let test n =
      let t = create n in
      let rec loop n =
        let n = n - 1 in
        decr t >>= fun () ->
        assert (n >= 0);
        loop n
      in loop n
    in
    for i = 1 to 20 do
      don't_wait_for (test i);
      stabilize ();
    done
  ;;

  (* check if we wakeup after sleeping *)
  let%test_unit _ =
    let total_wakeups_per_job = 5 in
    let total_wakeups = ref 0 in
    let wakeups_ivar = Ivar.create () in
    let test n =
      let t = create n in
      let wakeup = ref false in
      let sleeper n =
        let rec loop n =
          if n > 0 then begin
            decr t >>= fun () ->
            if n <= total_wakeups_per_job then begin
              assert (!wakeup);
              total_wakeups := !total_wakeups + 1;
            end;
            loop (n-1)
          end
          else return ()
        in
        loop (n + total_wakeups_per_job)
      in
      let waker () =
        Ivar.read wakeups_ivar >>= fun () ->
        wakeup := true;
        let rec loop n =
          if n > 0 then begin
            incr t;
            loop (n-1)
          end
          else ()
        in
        loop total_wakeups_per_job;
        return ()
      in
      don't_wait_for (sleeper n);
      don't_wait_for (waker ());
    in
    let total_jobs = 10 in
    for i = 1 to total_jobs do
      test i
    done;
    stabilize ();
    Ivar.fill wakeups_ivar ();
    stabilize ();
    (* eprintf "(total_wakeups_per_job (%d) * total_jobs (%d) = !total_wakeups (%d)) \n%!"
     *   total_wakeups_per_job total_jobs !total_wakeups; *)
    assert (total_wakeups_per_job * total_jobs = !total_wakeups)
  ;;

end)

