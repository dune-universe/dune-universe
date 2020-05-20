module Suite = struct
  exception TestFailure of string

  type result =
    | Error of exn
    | Fail of string
    | Pass

  type outcome = {
    label: string;
    result: result;
    time_s: float;
  }

  let string_of_result = function
    | Error e -> Printf.sprintf "Error: %s" (Printexc.to_string e)
    | Fail msg -> Printf.sprintf "Fail: %s" msg
    | Pass -> "Pass"

  let finally f cleanup =
    let result =
      try f ()
      with e ->
        cleanup ();
        raise e
    in
    cleanup ();
    result

  module Sync = struct
    type test_fun = unit -> unit

    let bracket setup test teardown () =
      let state = setup () in
      finally
        (fun () -> test state)
        (fun () -> teardown state)
  end

  module Async = struct
    type callback = unit -> unit
    let noop () = ()

    type wrapper = callback -> unit
    type test_fun = wrapper -> unit

    let bracket setup test teardown =
      (fun wrapper ->
        let state = setup () in
        let wrapper' f =
          wrapper (fun () -> finally f (fun () -> teardown state)) in
        try
          test state wrapper'
        with e ->
          teardown state;
          raise e)

    let run_one label test log handle_outcome =
      (* Make sure we only handle one result per test. This prevents a successful
         callback from triggering a continuation of the tests if the synchronous
         code has already failed or errored. *)
      let handled = ref false in
      let start_time = ref 0.0 in
      let handle_result_once result =
        if not !handled
        then begin
          handled := true;
          log "End";
          log (string_of_result result);
          handle_outcome {label; result; time_s = Sys.time () -. !start_time}
        end
      in
      let catch_all f =
        try f ()
        with
          | TestFailure msg -> handle_result_once (Fail msg)
          | e -> handle_result_once (Error e)
      in
      (* This catch_all will catch failures and errors coming from the
         synchronous part of the test case, i.e. before the callback has been
         triggered. *)
      catch_all (fun () ->
        log "Start";
        start_time := Sys.time ();
        test
          (fun callback ->
            (* This catch_all will catch failures and errors coming from the
               asynchronous callback. *)
            catch_all (fun () ->
              callback ();
              handle_result_once Pass)))

    let of_sync test wrapper =
      test ();
      wrapper noop
  end

  type t =
    | TestCase of string * Async.test_fun
    | TestList of string * t list

  let (>::) label test_fun = TestCase (label, Async.of_sync test_fun)
  let (>:~) label test_fun = TestCase (label, test_fun)
  let (>:::) label tests = TestList (label, tests)

  let string_of_opt = function
    | Some value -> Printf.sprintf " (%s)" value
    | None -> ""

  let assert_true ?label value =
    if not value then begin
      let msg = Printf.sprintf "test value was false%s" (string_of_opt label) in
      raise (TestFailure msg)
    end

  let assert_equal ?(equal = (=)) ?label ?printer a b =
    if not (equal a b)
    then begin
      let values = match printer with
      | Some printer -> Printf.sprintf ": %s %s" (printer a) (printer b)
      | None -> ""
      in
      let msg = Printf.sprintf "not equal%s%s" (string_of_opt label) values in
      raise (TestFailure msg)
    end

  let assert_raises ?label expected_exn task =
    match
      try task (); None
      with raised_exn -> Some raised_exn
    with
    | None ->
      let msg =
        Printf.sprintf
          "expected exception not raised%s" (string_of_opt label)
      in
      raise (TestFailure msg)
    | Some raised_exn when raised_exn = expected_exn -> ()
    | Some raised_exn ->
      let msg =
        Printf.sprintf
          "unexpected exception raised%s: %s"
          (string_of_opt label)
          (Printexc.to_string raised_exn)
      in
      raise (TestFailure msg)

  let assert_raises_string ?label expected_exn_string task =
    match
      try task (); None
      with raised_exn -> Some raised_exn
    with
    | None ->
      let msg =
        Printf.sprintf
          "expected exception not raised%s" (string_of_opt label)
      in
      raise (TestFailure msg)
    | Some raised_exn
      when (Printexc.to_string raised_exn) = expected_exn_string -> ()
    | Some raised_exn ->
      let msg =
        Printf.sprintf
          "unexpected exception raised%s: %s"
          (string_of_opt label)
          (Printexc.to_string raised_exn)
      in
      raise (TestFailure msg)
end

module Zipper = struct
  type crumb = {
    left: Suite.t list;
    label: string;
    right: Suite.t list;
  }

  type t = {
    crumbs: crumb list;
    location: Suite.t;
  }

  let of_suite suite = {
    crumbs = [];
    location = suite;
  }

  let to_suite {location; _} = location

  let move_up {crumbs; location} =
    match crumbs with
    (* Already at the top of the tree, so nowhere to go. *)
    | [] -> None
    (* Move to the head of the list of crumbs. *)
    | {left; label; right} :: other_crumbs -> Some {
      crumbs = other_crumbs;
      location =
        Suite.TestList (label, List.rev_append (location :: left) right);
    }

  let move_down {crumbs; location} =
    match location with
    (* A TestCase has no children. *)
    | Suite.TestCase _ -> None
    (* A TestList may not have any children to move down to. *)
    | Suite.TestList (_, []) -> None
    (* Move down to the first child of the TestList. *)
    | Suite.TestList (label, first_child :: other_children) -> Some {
      crumbs = {
        left = [];
        label;
        right = other_children;
      } :: crumbs;
      location = first_child;
    }

  let move_right {crumbs; location} =
    match crumbs with
    (* At the top of the tree, so no siblings. *)
    | [] -> None
    (* Already at the rightmost sibling. *)
    | {right = []; _} :: _ -> None
    (* Move to the next sibling to the right. *)
    | {left; label; right = first_right :: other_right} :: other_crumbs -> Some {
      crumbs = {
        left = location :: left;
        label;
        right = other_right;
      } :: other_crumbs;
      location = first_right;
    }

  let rec next_sibling zipper =
    match move_right zipper with
    (* Move to the next sibling to the right. *)
    | (Some _) as result -> result
    (* No more siblings, so try to move up. *)
    | None -> begin
      match move_up zipper with
      (* If moving up succeeds, try moving right again. *)
      | Some zipper' -> next_sibling zipper'
      (* We can't move up, so we must be at the top of the tree. *)
      | None -> None
    end

  let next_location zipper =
    match move_down zipper with
    | Some _ as result -> result
    | None -> next_sibling zipper

  let get_labels {crumbs; location} =
    let location_label = match location with
    | Suite.TestCase (label, _) -> label
    | Suite.TestList (label, _) -> label
    in
    location_label :: (List.map (fun crumb -> crumb.label) crumbs) |> List.rev
end

module Utils = struct
  type output = {
    log: string list;
    outcomes: Suite.outcome list;
  }

  type raw_summary = {
    total: int;
    errors: int;
    failures: int;
    passes: int;
    passed: bool
  }

  type summary = {
    report: string;
    passed: bool;
  }

  let run suite callback =
    let log = ref [] in
    let log_with_prefix prefix msg =
      let line = Printf.sprintf "%s:%s" prefix msg in
      log := (line :: !log)
    in
    let zipper = Zipper.of_suite suite in
    let rec run' ({Zipper.location; _} as zipper) outcomes =
      let continue zipper outcomes' =
        match Zipper.next_location zipper with
        | Some zipper' -> run' zipper' outcomes'
        | None ->
          callback {
            log = List.rev !log;
            outcomes = List.rev outcomes'
          }
      in
      match location with
      | Suite.TestCase (_, test_fun) ->
        let prefix = Zipper.get_labels zipper |> String.concat ":" in
        let log = log_with_prefix prefix in
        Suite.Async.run_one
          prefix test_fun log
          (fun outcome -> continue zipper (outcome :: outcomes))
      | Suite.TestList (_, _) -> continue zipper outcomes
    in
    run' zipper []

  let summarise_raw outcomes =
    let total, errors, failures, passes =
      List.fold_left
        (fun (total, errors, failures, passes) outcome ->
          let open Suite in
          match outcome.result with
          | Error _ -> total + 1, errors + 1, failures, passes
          | Fail _ -> total + 1, errors, failures + 1, passes
          | Pass -> total + 1, errors, failures, passes + 1)
        (0, 0, 0, 0) outcomes
    in
    {
      total; errors; failures; passes;
      passed = (total = passes)
    }

  let summary_of_raw raw =
    let report =
      String.concat "\n"
        [
          Printf.sprintf "%d tests run" raw.total;
          Printf.sprintf "%d errors" raw.errors;
          Printf.sprintf "%d failures" raw.failures;
          Printf.sprintf "%d passes" raw.passes;
        ]
    in
    {
      report;
      passed = raw.passed;
    }

  let summarise outcomes =
    summarise_raw outcomes
    |> summary_of_raw
end
