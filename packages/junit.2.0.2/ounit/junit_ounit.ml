(** Interface from OUnit result to JUnit reports *)

module O = OUnit
module J = Junit

let of_result o =
  let time = 0. in
  let classname = "" in
  let typ = "" in
  match o with
  | O.RError (path, msg) ->
    J.Testcase.error
      ~typ ~classname ~time
      ~name:(O.string_of_path path)
      ~message:msg
      ""
  | O.RSuccess path ->
    J.Testcase.pass
      ~classname ~time
      ~name:(O.string_of_path path)
  | O.RFailure (path, msg) ->
    J.Testcase.failure
      ~typ ~classname ~time
      ~message:msg
      ~name:(O.string_of_path path)
      ""
  | O.RSkip (path, _msg) ->
    J.Testcase.skipped
      ~classname ~time
      ~name:(O.string_of_path path)
  | O.RTodo (path, _msg) ->
    J.Testcase.skipped
      ~classname ~time
      ~name:(O.string_of_path path ^ "(todo)")

let of_results ~name l =
  let l = List.map of_result l in
  let suite = J.Testsuite.make ~name () in
  J.Testsuite.add_testcases l suite

let to_file ~name file l =
  let suite = of_results ~name l in
  let report = Junit.make [suite] in
  let xml_report = Junit.to_xml report in
  let oc = open_out file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "@[%a@]@." (Tyxml.Xml.pp ()) xml_report;
  close_out oc;
  ()
