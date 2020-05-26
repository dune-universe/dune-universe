type t (** The test type *)

(** {6 Test registration} *)

val test_unit : Location.t -> Longident.t -> (unit -> unit) -> unit
(** Add a test of type [unit -> unit]. 
    This is called when [let %TEST_UNIT] ppx phrase is called.
*)

val test : Location.t -> Longident.t -> (unit -> bool) -> unit
(** Add a test of type [unit -> bool].
    This is called when [let %TEST] ppx phrase is called.
 *)

val test_fail : Location.t -> Longident.t -> (unit -> unit) -> unit
(** Add a test of type [unit -> unit]. It must fail by raising an exception.
    This is called when [let %TEST_FAIL] ppx phrase is called.
 *)

(** {6 Module opened in the test code} *)

module TestTool : sig

  val must_raise : (unit -> 'any) -> bool
  (** [must_raise f] tests [f ()] raises some exception. *)
end
  
(** {6 Higher level API} *)

val collect : unit -> unit
(** Run tests added by [test], [test_unit] and [add].
    Then it prints the report to stderr and exit.
    The exit status is 0 if there is no error, otherwise, 1.

    This is for standalone test executables: it parses command line options 
    using [arg_specs] to select tests to perform. All the tests are executed
    by default.

    For application programs with integrated test interface,
    use [run_tests] instead.
*)

(** {6 Middle level APIs} *)

module Name : sig
  (** Name of tests *)

  type t = { label : Longident.t option; location : Location.t option; }
  val null : t
end

module Error : sig
  (** Test error descriptions *)

  type t = [ `Exn of exn * Printexc.raw_backtrace
           | `False ]
  val format : Format.formatter -> t -> unit
end

module Result : sig
  (** Test result *)

  type t = { 
    time : float; (** seconds used (not implemented yet) *)   
    result : [ `Error of Error.t | `Ok of unit ]; 
  }
end

module Report : sig
  (** Report for the test run *)

  type t = (Name.t * Result.t) list * int (** num of errors *)  

  val print : t -> unit
  (** Print report to stderr *)

  val print_then_exit : t -> unit
  (** Return value is dependent on the number of errors *)
end

val run_tests : bool -> Report.t
(** Run tests added by [test], [test_unit] and [add].
    It returns a report.
    
    This is for application programs with integrated tests.
    
    The boolean is to set the default value to perform each test or not.
    For test selections, applications must use [arg_specs] at its command line
    option parsing.
*)

val arg_specs : (string * Arg.spec * string) list
(** Command line switches for testing options.
    Useful when integrating the tests into main applications
    instead of building independent test executables. 
*)

(** {6 Lower level APIs} *)

val add : t -> unit
val fun_ : (unit -> bool) -> t
val label : string -> t -> t
val ident : Longident.t -> t -> t
val list : t list -> t

