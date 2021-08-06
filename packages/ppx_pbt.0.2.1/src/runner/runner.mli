val add_test : QCheck.Test.t -> unit
(** Add a generated test to the test suite *)

val add_tests : QCheck.Test.t list -> unit
(** Add a list of generated test to the test suite *)

val run : unit -> unit
(** Run the generated tests using Alcotest runner *)
