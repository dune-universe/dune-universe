(** Dependency-free library for creating and running tests. *)

(** Types and functions for creating and structuring unit test suites. *)
module Suite : sig

  exception TestFailure of string
  (** The exception thrown by failing tests. *)

  type result =
    | Error of exn           (** An unexpected error occurred in the test. *)
    | Fail of string         (** An assertion failed in the test. *)
    | Pass                   (** The test passed. *)
  (** The result of running a single testcase. *)

  type outcome = {
    label: string;
    result: result;
    time_s: float;
  }
  (** The outcome of a test: its label, result, and time taken to run.*)

  val string_of_result : result -> string

  module Sync : sig
    type test_fun = unit -> unit
    (** A synchronous test function. *)

    val bracket : (unit -> 'a) -> ('a -> unit) -> ('a -> unit) -> test_fun
    (** [bracket setup test teardown] generates a {{:#TYPEtest_fun}test_fun}
        which will use [setup] to create state needed for the test, then pass
        that state to [test], and finally will pass that state to [teardown]. *)
  end

  module Async : sig
    type callback = unit -> unit
    (** The type of an asynchronous callback which will run as part of an
        asynchronous test. *)

    val noop : callback
    (** The noop callback - this is just an alias for [fun () -> ()]. *)

    type wrapper = callback -> unit
    (** A wrapper function to be passed to an asynchronous test. *)

    type test_fun = wrapper -> unit
    (** An asynchronous test function. When run it will be passed a wrapper
          function - this should be used to wrap any asynchronous code which the
          test case is expected to run. *)

    val bracket :
      (unit -> 'a) -> ('a -> wrapper -> unit) -> ('a -> unit) -> test_fun
    (** [bracket setup test teardown] generates a {{:#TYPEtest_fun}test_fun}
        which will use [setup] to create state needed for the test, then pass
        that state to [test], and finally will pass that state to [teardown]. *)

    val run_one :
      string -> test_fun -> (string -> unit ) -> (outcome -> unit) -> unit
    (** Run an asynchronous test and pass its result to a callback. *)

    val of_sync : Sync.test_fun -> test_fun
    (** Convert a synchronous test into an asynchronous test. *)
  end

  type t =
    | TestCase of string * Async.test_fun (** A labelled single test. *)
    | TestList of string * t list         (** A labelled list of tests. *)
  (** A labelled wrapper around a test or list of suites. *)

  val (>::) : string -> Sync.test_fun -> t
  (** Convenience function to create a suite from a label and a
      {{:Webtest.Suite.Sync.html#TYPEtest_fun}Sync.test_fun}. *)

  val (>:~) : string -> Async.test_fun -> t
  (** Convenience function to create a suite from a label and an
      {{:Webtest.Suite.Async.html#TYPEtest_fun}Async.test_fun}. *)

  val (>:::) : string -> t list -> t
  (** Convenience function to create a suite from a label and a list of
      suites. *)

  val assert_true : ?label:string -> bool -> unit
  (** [assert_bool label value] returns unit if [value] is true, and otherwise
      raises {{:#EXCEPTIONTestFailure}TestFailure}. *)

  val assert_equal :
    ?equal:('a -> 'a -> bool) -> ?label:string ->
    ?printer:('a -> string) -> 'a -> 'a -> unit
  (** [assert_equal a b] returns unit if [a] is equal to [b], and otherwise
      raises {{:#EXCEPTIONTestFailure}TestFailure}. *)

  val assert_raises : ?label:string -> exn -> (unit -> unit) -> unit
  (** [assert_raises e task] returns unit if [task ()] raises [e], and otherwise
        raises {{:#EXCEPTIONTestFailure}TestFailure}. *)

  val assert_raises_string : ?label:string -> string -> (unit -> unit) -> unit
  (** [assert_raises_string str task] returns unit if [task ()] raises an
      exception [e] for which [Printexc.to_string e = str], and otherwise
      raises {{:#EXCEPTIONTestFailure}TestFailure}. *)
end

(** A zipper implementation based on {{:Webtest.Suite.html#TYPEt}Suite.t}, which
    represents the current location in the tree as well as the path used to
    reach the current location from the root.

    Generally this module should not be used directly; instead
    {{:Webtest.Utils.html#VALrun}Utils.run} can be used to traverse the test
    tree and report results. *)
module Zipper : sig

  type crumb = {
    left: Suite.t list;
    (** The list of siblings to the left of the current location. *)
    label: string;
    (** The label of the parent of the current location. *)
    right: Suite.t list;
    (** The list of siblings to the right of the current location. *)
  }
  (** A type representing the path through a
     {{:Webtest.Suite.html#TYPEt}Webtest.Suite.t} from the root to the current
     location. *)

  type t = {
    crumbs: crumb list;
    (** The list of crumbs which leads to the current location in the tree. *)
    location: Suite.t;
    (** The current location in the tree. *)
  }
  (** A zipper implementation based on {{:Webtest.Suite.html#TYPEt}Suite.t}. *)

  val of_suite : Suite.t -> t
  (** Convert a {{:Webtest.Suite.html#TYPEt}Suite.t} into a
      {{:#TYPEt}Zipper.t}. *)

  val to_suite : t -> Suite.t
  (** Convert a {{:#TYPEt}Zipper.t} into a {{:Webtest.Suite.html#TYPEt}Suite.t}.
      Note that this does not include the crumbs, only the subtree at the
      current location. *)

  val move_up    : t -> t option
  (** Attempt to move up to the parent node. *)

  val move_down  : t -> t option
  (** Attempt to move down to the first child node. *)

  val move_right : t -> t option
  (** Attempt to move right to the next sibling. *)

  val next_location : t -> t option
  (** Attempt to move to the next location while traversing the tree. Return
      None if we're already at the last location to be traversed. *)

  val get_labels : t -> string list
  (** Get the list of labels from all crumbs plus that of the current
      location, starting at the root of the tree. *)
end

(** Types and functions for running unit tests. *)
module Utils : sig

  type output = {
    log: string list;             (** The logging produced while running the
                                      tests. *)
    outcomes: Suite.outcome list; (** The results of running the tests. *)
  }
  (** The output generated by running a test. *)

  type raw_summary = {
    total: int;
    errors: int;
    failures: int;
    passes: int;
    passed: bool
  }
  (** Raw summary of test run with the total number of tests, and
      failed/passed tests. *)

  type summary = {
    report: string;             (** A report summarising the test results. *)
    passed: bool;               (** A flag indicating whether all the tests
                                    passed. *)
  }
  (** A summary of a test run: short description of results plus a flag
      indicating whether all the tests passed. *)

  val run : Suite.t -> (output -> unit) -> unit
  (** [run suite callback] runs [suite], passes the output to [callback]. *)

  val summarise_raw : Suite.outcome list -> raw_summary
  (** [summarise outcomes] converts a list of test outcomes into a raw
      summary. *)

  val summary_of_raw : raw_summary -> summary
  (** [summary_of_raw] creates a basic summary from a raw summary. *)

  val summarise : Suite.outcome list -> summary
  (** [summarise outcomes] converts a list of test outcomes into a summary. *)
end
