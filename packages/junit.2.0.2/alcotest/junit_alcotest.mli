(** Interface to product JUnit reports for Alcotest

    It tries to provide a layer as thin as possible on top of Alcotest
    to allow to port existing test without writing a lot a boilerplate.
*)

val wrap_test :
  ?classname:string ->
  (Junit.Testcase.t -> unit) ->
  unit Alcotest.test_case ->
  unit Alcotest.test_case
(** [wrap_test handle_result test_cases] wraps test cases to create
    Junit testcases and pass them to [handle_result].

    Can be used with {!run} to create customized Junit testsuites if
    the output of {!run_and_report} is not as expected.

    @param classname will populate the 'classname' attribute
    for the test case. For best hierarchic rendering in Jenkins, it
    should contain a period. For example, "foo.bar.baz" will be rendered
    a package "foo.bar" that contains a class "baz", which contains the
    current test case and others. Defaults to the name of the test case.
*)

val run: ?argv:string array -> string -> unit Alcotest.test list -> unit
(** [run ?argv n t] is a wrapper around {!Alcotest.run}, only setting
    [and_exit] to false. It is mandatory to be able to process results
    after the end of the run.

    Low level function. It is easier to use {!run_and_report}.
*)

type exit = unit -> unit
(** [exit ()] exists with appropriate code if {!run_and_report}'s
    [and_exit] was [true] or raise {!Alcotest.Test_error} in case of
    error.
*)

val run_and_report:
  ?and_exit:bool ->
  ?package:string ->
  ?timestamp:Ptime.t ->
  ?argv:string array ->
  string ->
  (string * unit Alcotest.test_case list) list ->
  (Junit.Testsuite.t * exit)
(** [run name tests] is a wrapper around {!run} and {!wrap_test}. It
    runs the tests and creates a Junit testsuite from the results.

    As {!Alcotest.run} is always called with [and_exit = false] to be
    able to produce a report, the behavior is emulated by the returned
    {!exit} function.

    The optional argument [and_exit] controls what happens when the
    {!exit} function is called. By default, [and_exit] is set, which
    makes the function exit with [0] if everything is fine or [1] if
    there is an issue. If [and_exit] is [false], then the function
    raises [Test_error] on error.

    [?argv] is forwarded to {!run}. [?package] and [?timestamp] are
    forwarded to {!Junit.Testsuite.make}.
*)
