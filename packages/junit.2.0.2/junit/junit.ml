module Property = struct
  type t =
    {
      name: string;
      value: string;
    }

  let make ~name ~value =
    {
      name;
      value;
    }
end

module Testcase = struct
  type error = Junit_xml.error =
    {
      message: string option;
      typ: string;
      description: string;
    }

  type failure = Junit_xml.failure =
    {
      message: string option;
      typ: string;
      description: string;
    }

  type result = Junit_xml.result =
    | Error of error
    | Failure of failure
    | Pass
    | Skipped

  type t =
    {
      name: string;
      classname: string;
      time: float;
      result: result;
    }

  let make ~name ~classname ~time result =
    {
      name;
      classname;
      time;
      result
    }

  let error ?message ~typ ~name ~classname ~time description  =
    let result =
      Error {
        message;
        typ;
        description;
      }
    in
    make ~name ~classname ~time result


  let failure ?message ~typ ~name ~classname ~time description  =
    let result =
      Failure {
        message;
        typ;
        description;
      }
    in
    make ~name ~classname ~time result

  let skipped ~name ~classname ~time =
    make ~name ~classname ~time Skipped

  let pass ~name ~classname ~time =
    make ~name ~classname ~time Pass
end

module Testsuite = struct
  type t =
    {
      package: string;
      id: int;
      name: string;
      timestamp: Ptime.t;
      hostname: string;
      tests: int;
      failures: int;
      errors: int;
      time: float;
      system_out: string option;
      system_err: string option;
      properties: Property.t list;
      testcases: Testcase.t list;
    }

  let make
      ?package
      ?timestamp
      ?(hostname="localhost")
      ?system_out
      ?system_err
      ~name
      ()
    =
    let package =
      match package with
      | None -> name
      | Some p -> p
    in
    let timestamp =
      match timestamp with
      | None -> Ptime_clock.now ()
      | Some t -> t
    in
    {
      package = package;
      id = 0;
      name;
      timestamp;
      hostname;
      tests = 0;
      failures = 0;
      errors = 0;
      time = 0.;
      system_out;
      system_err;
      properties = [];
      testcases = [];
    }

  let add_testcase testcase t =
    let t =
      {
        t with
        tests = t.tests + 1;
        time = t.time +. testcase.Testcase.time;
        testcases = testcase :: t.testcases
      }
    in
    match testcase.Testcase.result with
    | Testcase.Pass
    | Testcase.Skipped -> t
    | Testcase.Error _ ->
      { t with errors = t.errors + 1; }
    | Testcase.Failure _ ->
      { t with failures = t.failures + 1; }

  let add_testcases testcases t =
    List.fold_left (fun t tc -> add_testcase tc t) t testcases

  let add_property properties t =
    { t with properties = properties :: t.properties}

  let add_properties properties t =
    List.fold_left (fun t tc -> add_property tc t) t properties
end

type t =
  {
    next_id: int;
    testsuites: Testsuite.t list;
  }

let make testsuites =
  let testsuites = List.mapi (fun i t ->
      Testsuite.{ t with id = i; }
    ) testsuites
  in
  let length = List.length testsuites in
  {
    next_id = length;
    testsuites;
  }

let add_testsuite testsuite t =
  let ts = Testsuite.{ testsuite with id = t.next_id; } in
  {
    next_id = succ t.next_id;
    testsuites = ts :: t.testsuites;
  }

let to_xml (t:t) =
  let testsuites = List.map (fun t ->
      let properties =
        let open Property in
        List.map (fun p ->
            Junit_xml.property ~name:p.name ~value:p.value
          ) t.Testsuite.properties
      in
      let testcases =
        let open Testcase in
        List.map (fun p ->
            Junit_xml.testcase
              ~name:p.name
              ~classname:p.classname
              ~time:p.time
              p.result
          ) t.Testsuite.testcases
      in
      let open Testsuite in
      Junit_xml.testsuite
        ?system_out:t.system_out
        ?system_err:t.system_err
        ~package:t.package
        ~id:t.id
        ~name:t.name
        ~timestamp:(Junit_xml.timestamp t.timestamp)
        ~hostname:t.hostname
        ~tests:t.tests
        ~failures:t.failures
        ~errors:t.errors
        ~time:t.time
        properties
        testcases
    ) t.testsuites
  in
  Junit_xml.to_xml testsuites

let to_file (t:t) filename =
  let xml_report = to_xml t in
  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "@[%a@]@." (Tyxml.Xml.pp ()) xml_report;
  close_out oc;
  ()
