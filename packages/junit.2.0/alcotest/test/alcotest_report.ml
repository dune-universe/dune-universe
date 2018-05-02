module A = Alcotest
module JA = Junit_alcotest

module To_test = struct
  let capit letter = Astring.Char.Ascii.uppercase letter
  let plus int_list = List.fold_left (fun a b -> a + b) 0 int_list
end

let capit () =
  A.(check char) "Check A" 'A' (To_test.capit 'a')

let plus () =
  A.(check int)"Sum equals to 7" 7 (To_test.plus [1;1;2;3])

let wrong_result () =
  A.(check string) "string_of_int equals to '7'" "7" (string_of_int 8)

let raise_unexpected_exn () =
  A.(check int) "int_of_string equals to 7" 7 (invalid_arg "7")

let test_set = [
  A.test_case "Test with unexpected exception" `Quick raise_unexpected_exn;
  A.test_case "Capitalize" `Quick capit;
  A.test_case "Add entries" `Slow plus;
  A.test_case "Test with wrong result" `Quick wrong_result;
]

let success_test_set = [
  A.test_case "Capitalize" `Quick capit;
  A.test_case "Add entries" `Slow plus;
]

let timestamp =
  match Ptime.of_date_time ((2013, 5, 24), ((10, 23, 58), 0)) with
  | Some t -> t
  | None -> assert false

let alcotest path =
  let package = "junit_alcotest" in
  let (testsuite1, _) = JA.run_and_report ~package ~timestamp "My first test" [
    "Basic tests", test_set;
  ]
  in
  let (testsuite2, _) = JA.run_and_report ~package ~timestamp "My second test" [
    "Basic tests", test_set;
  ]
  in
  let (testsuite3, exit) = JA.run_and_report ~and_exit:false ~package ~timestamp "Success test suite" [
    "Good tests", success_test_set;
  ]
  in
  let report = Junit.make [testsuite1; testsuite2; testsuite3] in
  begin match path with
    | None ->
      let xml_report = Junit.to_xml report in
      Format.printf "%a\n" (Tyxml.Xml.pp ()) xml_report
    | Some path ->
      Junit.to_file report path
  end;
  exit ()

let () =
  let path = try Some (Sys.getenv "REPORT_PATH") with _ -> None  in
  alcotest path
