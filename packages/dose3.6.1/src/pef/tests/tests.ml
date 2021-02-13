open OUnit
module Version = Dose_versioning.Debian

let testdir = "./tests"

let returns_result ?(printer = fun _ -> "(FIXME)") function_to_test
    expected_result args () =
  assert_equal ~printer (function_to_test args) expected_result

and raises_failure function_to_test failure_text args () =
  assert_raises (Failure failure_text) (fun () -> function_to_test args)

let parse_pef_vpkgformula =
  let function_to_test par =
    let f =
      Dose_pef.Packages.parse_s ~default:[] Dose_pef.Packages.parse_vpkgformula
    in
    Dose_pef.Packages.get_field_value ~parse:f ~par ~field:("Depends", None)
  in
  let returns = returns_result function_to_test in
  [ ( "depends empty",
      [("Depends", (Dose_common.Format822.dummy_loc, ""))],
      returns ("Depends", []) );
    ( "depends simple",
      [("Depends", (Dose_common.Format822.dummy_loc, "a"))],
      returns ("Depends", [[(("a", None), None)]]) );
    ( "depends simple constr",
      [("Depends", (Dose_common.Format822.dummy_loc, "a ( <= 3.4 )"))],
      returns ("Depends", [[(("a", None), Some ("<=", "3.4"))]]) );
    ( "depends simple arch",
      [("Depends", (Dose_common.Format822.dummy_loc, "a:arch1"))],
      returns ("Depends", [[(("a", Some "arch1"), None)]]) );
    ( "depends simple any",
      [("Depends", (Dose_common.Format822.dummy_loc, "a:any"))],
      returns ("Depends", [[(("a", Some "any"), None)]]) );
    ( "depends disj",
      [("Depends", (Dose_common.Format822.dummy_loc, "a,b"))],
      returns ("Depends", [[(("a", None), None)]; [(("b", None), None)]]) );
    ( "depends conj",
      [("Depends", (Dose_common.Format822.dummy_loc, "a|b"))],
      returns ("Depends", [[(("a", None), None); (("b", None), None)]]) );
    ( "depends cnf",
      [("Depends", (Dose_common.Format822.dummy_loc, "a|b,c"))],
      returns
        ( "Depends",
          [[(("a", None), None); (("b", None), None)]; [(("c", None), None)]] )
    ) ]

let parse_pef_builddepsformula =
  let function_to_test par =
    let f =
      Dose_pef.Packages.parse_s
        ~default:[]
        Dose_pef.Packages.parse_builddepsformula
    in
    Dose_pef.Packages.get_field_value ~parse:f ~par ~field:("BuildDepends", None)
  in
  let returns = returns_result function_to_test in
  (* let raises  = raises_failure function_to_test in *)
  [ ( "build depends empty",
      [("BuildDepends", (Dose_common.Format822.dummy_loc, ""))],
      returns ("BuildDepends", []) );
    ( "build depends simple",
      [("BuildDepends", (Dose_common.Format822.dummy_loc, "a"))],
      returns ("BuildDepends", [[((("a", None), None), [], [])]]) );
    ( "build depends simple arch filter",
      [("BuildDepends", (Dose_common.Format822.dummy_loc, "a [arch]"))],
      returns ("BuildDepends", [[((("a", None), None), [(true, "arch")], [])]])
    );
    ( "build depends simple profile filter",
      [("BuildDepends", (Dose_common.Format822.dummy_loc, "a <prof>"))],
      returns ("BuildDepends", [[((("a", None), None), [], [[(true, "prof")]])]])
    );
    ( "build depends cnf profile filter",
      [ ( "BuildDepends",
          (Dose_common.Format822.dummy_loc, "a <prof1> <!prof2 prof3>") ) ],
      returns
        ( "BuildDepends",
          [ [ ( (("a", None), None),
                [],
                [[(true, "prof1")]; [(false, "prof2"); (true, "prof3")]] ) ] ]
        ) ) ]

let parse_pef_archlist =
  let function_to_test par =
    let f =
      Dose_pef.Packages.parse_s ~default:[] Dose_pef.Packages.parse_archlist
    in
    Dose_pef.Packages.get_field_value
      ~parse:f
      ~par
      ~field:("Architectures", None)
  in
  let returns = returns_result function_to_test in
  (* let raises  = raises_failure function_to_test in *)
  [ ( "architectures",
      [("Architectures", (Dose_common.Format822.dummy_loc, "arch1 arch2"))],
      returns ("Architectures", ["arch1"; "arch2"]) ) ]

let pefcudf_loadl =
  let packagelist =
    Dose_pef.Packages.input_raw [Filename.concat testdir "pef/unittests.pef"]
  in
  let tables = Dose_pef.Pefcudf.init_tables Version.compare packagelist in
  let function_to_test ?arch ?(archs = []) l =
    Dose_pef.Pefcudf.loadl tables ?arch ~archs l
  in
  let returns ?arch ?(archs = []) =
    returns_result (function_to_test ?arch ~archs)
  in
  (* let raises  = raises_failure function_to_test in *)
  [ ("loadl simple", [(("a", None), None)], returns [("a", None)]);
    ( "loadl simple arch",
      [(("a", Some "arch"), None)],
      returns [("a%3aarch", None)] );
    ( "loadl simple any",
      [(("a", Some "any"), None)],
      returns
        ~arch:"arch1"
        ~archs:["arch1"; "arch2"]
        [("a%3aarch1", None); ("a%3aarch2", None)] ) ]

let make_test_cases triplets =
  List.map
    (fun (test_name, input, assert_function) ->
      test_name >:: assert_function input)
    triplets

let test_parsing =
  "test_parsing"
  >::: [ "test vpkgformula" >::: make_test_cases parse_pef_vpkgformula;
         "test builddepsformula" >::: make_test_cases parse_pef_builddepsformula;
         "test archlist" >::: make_test_cases parse_pef_archlist ]

let test_pefcudf =
  "test_pefcudf" >::: ["test loadl" >::: make_test_cases pefcudf_loadl]

let all = "all tests" >::: [test_parsing; test_pefcudf]

let main () = OUnit.run_test_tt_main all

;;
main ()
