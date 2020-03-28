open Bastet_async

let id, compose = Bastet.Function.(Category.id, Semigroupoid.compose)

let ( >>= ) = Async_kernel.( >>= )

let test check a b =
  a >>= fun expected ->
  b >>= fun actual -> Async_kernel.Deferred.return @@ check "" expected actual

module Functor = struct
  let identity () =
    let expected = "foo" in
    Async_kernel.Deferred.return expected |> Bastet_async.Functor.map id >>= fun actual ->
    Async_kernel.Deferred.return @@ Alcotest.(check string) "" expected actual

  let composition () =
    let a = Functor.map (compose string_of_int (( + ) 1)) (Async_kernel.Deferred.return 123)
    and b =
      compose
        (Bastet_async.Functor.map string_of_int)
        (Bastet_async.Functor.map (( + ) 1))
        (Async_kernel.Deferred.return 123)
    in
    test Alcotest.(check string) a b
end

module Apply = struct
  open Bastet_async.Infix

  let associative_composition () =
    let f = Async_kernel.Deferred.return string_of_int
    and g = Async_kernel.Deferred.return (( + ) 1)
    and h = Async_kernel.Deferred.return 123 in
    test Alcotest.(check string) (Bastet_async.Functor.map compose f <*> g <*> h) (f <*> (g <*> h))
end

module Applicative = struct
  open Bastet_async.Infix

  let identity () =
    test
      Alcotest.(check string)
      (Bastet_async.Applicative.pure id <*> Async_kernel.Deferred.return "foo")
      (Async_kernel.Deferred.return "foo")

  let homomorphism () =
    let double x = x * 2 in
    test
      Alcotest.(check int)
      (Bastet_async.Applicative.pure double <*> Bastet_async.Applicative.pure 123)
      (Bastet_async.Applicative.pure (double 123))

  let interchange () =
    let double = Bastet_async.Applicative.pure (( * ) 2) in
    test
      Alcotest.(check int)
      (double <*> Bastet_async.Applicative.pure 123)
      (Bastet_async.Applicative.pure (fun f -> f 123) <*> double)
end

module Monad = struct
  open Bastet_async.Infix

  let double : int -> int Async_kernel.Deferred.t = compose Bastet_async.Applicative.pure (( * ) 2)

  let to_string : int -> string Async_kernel.Deferred.t =
    compose Bastet_async.Applicative.pure string_of_int

  let identity () =
    test Alcotest.(check int) (Bastet_async.Monad.pure 123 >>= double) (double 123) >>= fun _ ->
    test
      Alcotest.(check int)
      (Bastet_async.Monad.pure 123 >>= Bastet_async.Monad.pure)
      (Bastet_async.Monad.pure 123)

  let associativity () =
    let value = Bastet_async.Applicative.pure 123 in
    test
      Alcotest.(check string)
      (value >>= double >>= to_string)
      (value >>= (fun k -> double k) >>= to_string)
end

let () =
  let _ =
    Alcotest_async.run
      "Bastet_async"
      [
        ( "Functor",
          [
            Alcotest_async.test_case "" `Quick Functor.identity;
            Alcotest_async.test_case "" `Quick Functor.composition;
          ] );
        "Apply", [Alcotest_async.test_case "" `Quick Apply.associative_composition];
        ( "Applicative",
          [
            Alcotest_async.test_case "" `Quick Applicative.identity;
            Alcotest_async.test_case "" `Quick Applicative.homomorphism;
            Alcotest_async.test_case "" `Quick Applicative.interchange;
          ] );
        ( "Monad",
          [
            Alcotest_async.test_case "" `Quick Monad.identity;
            Alcotest_async.test_case "" `Quick Monad.associativity;
          ] );
      ]
  in
  Core.never_returns @@ Async_unix.Scheduler.go ()
