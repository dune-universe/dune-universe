open Tyxml.Xml

type token = string

type timestamp = string

let timestamp time =
  let (y, m, d), ((hh, ss, mm), _) = Ptime.to_date_time time in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" y m d hh ss mm

type property =
  {
    name : token;
    value : string;
  }

type properties = property list

let property ~name ~value =
  {
    name;
    value;
  }

let property_to_xml property =
  let name = string_attrib "name" property.name in
  let value = string_attrib "value" property.value in
  node "property" ~a:[name; value] []

let properties_to_xml properties =
  node "properties" (List.map property_to_xml properties)

type error =
  {
    message : string option;
    typ : string;
    description : string;
  }

let error ?message ~typ description : error =
  {
    message;
    typ;
    description;
  }

let error_to_xml (error : error) =
  let typ = string_attrib "type" error.typ in
  let attributes = [typ] in
  let attributes =
    match error.message with
    | None -> attributes
    | Some m ->
      let message = string_attrib "message" m in
      message :: attributes
  in
  let description = pcdata error.description in
  node "error" ~a:attributes [description]

type failure =
  {
    message : string option;
    typ : string;
    description : string;
  }

let failure ?message ~typ description : failure =
  {
    message;
    typ;
    description;
  }

let failure_to_xml (failure : failure) =
  let typ = string_attrib "type" failure.typ in
  let attributes = [typ] in
  let attributes =
    match failure.message with
    | None -> attributes
    | Some m ->
      let message = string_attrib "message" m in
      message :: attributes
  in
  let description = pcdata failure.description in
  node "failure" ~a:attributes [description]

type result =
  | Error of error
  | Failure of failure
  | Pass
  | Skipped

let result_to_xml : result -> Tyxml.Xml.elt = function
  | Error e -> error_to_xml e
  | Failure f -> failure_to_xml f
  | Pass -> Tyxml.Xml.empty ()
  | Skipped -> node "skipped" []

type testcase =
  {
    name : string;
    classname : token;
    time : float;
    result : result;
  }

type testcases = testcase list

let testcase ~name ~classname ~time result =
  {
    name;
    classname;
    time;
    result;
  }

let testcase_to_xml (testcase : testcase) =
  let name = string_attrib "name" testcase.name in
  let classname = string_attrib "classname" testcase.classname in
  let time = float_attrib "time" testcase.time in
  let result = result_to_xml testcase.result in
  node "testcase" ~a:[name; classname; time] [result]

type testsuite =
  {
    package : token;
    id : int;
    name : token;
    timestamp : timestamp;
    hostname : token;
    tests : int;
    failures : int;
    errors : int;
    time : float;
    properties : properties;
    testcases : testcases;
    system_out : string option;
    system_err : string option;
  }

type testsuites = testsuite list

let testsuite
    ?system_out
    ?system_err
    ~package
    ~id
    ~name
    ~timestamp
    ~hostname
    ~tests
    ~failures
    ~errors
    ~time
    properties
    testcases
  =
  {
    package;
    id;
    name;
    timestamp;
    hostname;
    tests;
    failures;
    errors;
    time;
    properties;
    testcases;
    system_out;
    system_err;
  }

let testsuite_to_xml testsuite =
  let package = string_attrib "package" testsuite.package in
  let id = int_attrib "id" testsuite.id in
  let name = string_attrib "name" testsuite.name in
  let timestamp = string_attrib "timestamp" testsuite.timestamp in
  let hostname = string_attrib "hostname" testsuite.hostname in
  let tests = int_attrib "tests" testsuite.tests in
  let failures = int_attrib "failures" testsuite.failures in
  let errors = int_attrib "errors" testsuite.errors in
  let time = float_attrib "time" testsuite.time in
  let attributes =
    [
      package;
      id;
      name;
      timestamp;
      hostname;
      tests;
      failures;
      errors;
      time;
    ]
  in
  let system_out =
    match testsuite.system_out with
    | None -> empty ()
    | Some so -> node "system_out" [pcdata so]
  in
  let system_err =
    match testsuite.system_err with
    | None -> empty ()
    | Some se -> node "system_err" [pcdata se]
  in
  let properties = properties_to_xml testsuite.properties in
  let testcases = List.map testcase_to_xml testsuite.testcases in
  node
    "testsuite"
    ~a:attributes
    (properties :: system_out :: system_err :: testcases)

let to_xml testsuites =
  let elements = List.map testsuite_to_xml testsuites in
  node "testsuites" elements
