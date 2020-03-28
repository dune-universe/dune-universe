open Bastet_lwt

let id, compose = Bastet.Function.(Category.id, Semigroupoid.compose)

let ( >>= ) = Lwt.( >>= )

let test check a b =
  a >>= fun expected ->
  b >>= fun actual -> Lwt.return @@ check "" expected actual

module Functor = struct
  let identity _ () =
    let expected = "foo" in
    Lwt.return expected |> Bastet_lwt.Functor.map id >>= fun actual ->
    Lwt.return @@ Alcotest.(check string) "" expected actual

  let composition _ () =
    let a = Functor.map (compose string_of_int (( + ) 1)) (Lwt.return 123)
    and b =
      compose
        (Bastet_lwt.Functor.map string_of_int)
        (Bastet_lwt.Functor.map (( + ) 1))
        (Lwt.return 123)
    in
    test Alcotest.(check string) a b
end

module Apply = struct
  open Bastet_lwt.Infix

  let associative_composition _ () =
    let f = Lwt.return string_of_int and g = Lwt.return (( + ) 1) and h = Lwt.return 123 in
    test Alcotest.(check string) (Bastet_lwt.Functor.map compose f <*> g <*> h) (f <*> (g <*> h))
end

module Applicative = struct
  open Bastet_lwt.Infix

  let identity _ () =
    test
      Alcotest.(check string)
      (Bastet_lwt.Applicative.pure id <*> Lwt.return "foo")
      (Lwt.return "foo")

  let homomorphism _ () =
    let double x = x * 2 in
    test
      Alcotest.(check int)
      (Bastet_lwt.Applicative.pure double <*> Bastet_lwt.Applicative.pure 123)
      (Bastet_lwt.Applicative.pure (double 123))

  let interchange _ () =
    let double = Bastet_lwt.Applicative.pure (( * ) 2) in
    test
      Alcotest.(check int)
      (double <*> Bastet_lwt.Applicative.pure 123)
      (Bastet_lwt.Applicative.pure (fun f -> f 123) <*> double)
end

module Monad = struct
  open Bastet_lwt.Infix

  let double : int -> int Lwt.t = compose Bastet_lwt.Applicative.pure (( * ) 2)

  let to_string : int -> string Lwt.t = compose Bastet_lwt.Applicative.pure string_of_int

  let identity _ () =
    test Alcotest.(check int) (Bastet_lwt.Monad.pure 123 >>= double) (double 123) >>= fun _ ->
    test
      Alcotest.(check int)
      (Bastet_lwt.Monad.pure 123 >>= Bastet_lwt.Monad.pure)
      (Bastet_lwt.Monad.pure 123)

  let associativity _ () =
    let value = Bastet_lwt.Applicative.pure 123 in
    test
      Alcotest.(check string)
      (value >>= double >>= to_string)
      (value >>= (fun k -> double k) >>= to_string)
end

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "Bastet_lwt"
       [
         ( "Functor",
           [
             Alcotest_lwt.test_case "" `Quick Functor.identity;
             Alcotest_lwt.test_case "" `Quick Functor.composition;
           ] );
         "Apply", [Alcotest_lwt.test_case "" `Quick Apply.associative_composition];
         ( "Applicative",
           [
             Alcotest_lwt.test_case "" `Quick Applicative.identity;
             Alcotest_lwt.test_case "" `Quick Applicative.homomorphism;
             Alcotest_lwt.test_case "" `Quick Applicative.interchange;
           ] );
         ( "Monad",
           [
             Alcotest_lwt.test_case "" `Quick Monad.identity;
             Alcotest_lwt.test_case "" `Quick Monad.associativity;
           ] );
       ]
