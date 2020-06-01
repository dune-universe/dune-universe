open! Core
open! Async
open! Import
open! Expect_test_helpers_base
open! Bus
open! Async_bus

let%expect_test "[first_exn]" =
  let bus =
    Bus.create
      [%here]
      Arity1
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Error.raise
  in
  let d = first_exn bus [%here] Arity1 ~f:(fun _ -> Some ()) in
  let print d =
    print_s
      [%message
        ""
          ~is_determined:(Deferred.is_determined d : bool)
          ~num_subscribers:(Bus.num_subscribers bus : int)]
  in
  print d;
  let%bind () = [%expect {|
    ((is_determined   false)
     (num_subscribers 1)) |}] in
  Bus.write bus 0;
  print d;
  let%bind () = [%expect {|
    ((is_determined   true)
     (num_subscribers 0)) |}] in
  let d = first_exn bus [%here] Arity1 ~f:(fun i -> if i = 13 then Some () else None) in
  Bus.write bus 12;
  print d;
  let%bind () = [%expect {|
    ((is_determined   false)
     (num_subscribers 1)) |}] in
  Bus.write bus 13;
  print d;
  let%bind () = [%expect {|
    ((is_determined   true)
     (num_subscribers 0)) |}] in
  return ()
;;

let%expect_test "[first_exn] where [~f] raises" =
  let bus =
    Bus.create
      [%here]
      Arity1
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Error.raise
  in
  let d =
    Monitor.try_with_or_error (fun () ->
      first_exn bus [%here] Arity1 ~f:(fun _ -> failwith "raising"))
  in
  Bus.write bus 0;
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s ~hide_positions:true [%sexp (d : int Or_error.t Deferred.t)];
  [%expect
    {|
    (Full (
      Error (
        monitor.ml.Error
        ("Bus subscriber raised"
          (exn (Failure raising))
          (backtrace ("<backtrace elided in test>"))
          (subscriber (
            Bus.Subscriber.t (
              (on_callback_raise <fun>)
              (subscribed_from lib/async_bus/test/test_async_bus.ml:LINE:COL)))))
        ("Caught by monitor try_with_or_error")))) |}]
;;

let%expect_test "[first_exn ~stop:(Deferred.never ())]" =
  (* Providing the [stop] argument tickles some different codepaths.  Check that
     basic functionality still works. *)
  let bus =
    Bus.create
      [%here]
      Arity1
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Error.raise
  in
  let d = first_exn ~stop:(Deferred.never ()) bus [%here] Arity1 ~f:Fn.id in
  let print () =
    print_s
      [%message
        ""
          ~is_determined:(Deferred.is_determined d : bool)
          ~num_subscribers:(Bus.num_subscribers bus : int)]
  in
  Bus.write bus None;
  print ();
  let%bind () = [%expect {|
    ((is_determined   false)
     (num_subscribers 1)) |}] in
  Bus.write bus (Some 5);
  print ();
  let%bind () = [%expect {|
    ((is_determined   true)
     (num_subscribers 0)) |}] in
  return ()
;;

let%expect_test "[first_exn ~stop] where [stop] becomes determined" =
  let bus =
    Bus.create
      [%here]
      Arity1
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Error.raise
  in
  let stop = Ivar.create () in
  let num_calls = ref 0 in
  let d =
    first_exn ~stop:(Ivar.read stop) bus [%here] Arity1 ~f:(fun () ->
      incr num_calls;
      None)
  in
  let print () =
    print_s
      [%message
        ""
          (num_calls : int ref)
          ~is_determined:(Deferred.is_determined d : bool)
          ~num_subscribers:(Bus.num_subscribers bus : int)]
  in
  upon d Nothing.unreachable_code;
  print ();
  let%bind () =
    [%expect
      {|
    ((num_calls       0)
     (is_determined   false)
     (num_subscribers 1)) |}]
  in
  Bus.write bus ();
  print ();
  let%bind () =
    [%expect
      {|
    ((num_calls       1)
     (is_determined   false)
     (num_subscribers 1)) |}]
  in
  Ivar.fill stop ();
  (* [stop] is determined, so even if we write, the callback should not be called. *)
  Bus.write bus ();
  print ();
  let%bind () =
    [%expect
      {|
    ((num_calls       1)
     (is_determined   false)
     (num_subscribers 1)) |}]
  in
  (* If we allow the handler on the stop deferred to fire, it will unsubscribe. *)
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print ();
  let%bind () =
    [%expect
      {|
    ((num_calls       1)
     (is_determined   false)
     (num_subscribers 0)) |}]
  in
  return ()
;;

let%expect_test "[pipe1_exn] where the bus is closed" =
  let bus =
    Bus.create
      [%here]
      Arity1
      ~on_subscription_after_first_write:Allow
      ~on_callback_raise:Error.raise
  in
  let pipe = pipe1_exn bus [%here] in
  Bus.write bus 13;
  Bus.close bus;
  let%bind all = Pipe.read_all pipe in
  print_s [%sexp (all : int Queue.t)];
  [%expect {|
    (13) |}]
;;
