
(** Add a test to the framework  *)
val add_test : path:string -> test_name:string -> (unit -> unit) -> unit

(** Run inline alcotests *)
val run : unit -> unit
