open Base

let test_case description run_test = Alcotest.test_case description `Quick run_test

module Path = struct
  let correct = Mmdb.Path.of_string "sample.mmdb"

  let incorrect = Mmdb.Path.of_string "sample-not-there.mmdb"
end

module Ip = struct
  let correct = Mmdb.Ip.of_string "172.56.31.240"

  let incorrect = Mmdb.Ip.of_string "blah 172.56.31.240"
end

module Open_file_suite = struct
  (* (language list, (major version, minor version)) *)
  type mmdb_properties = string list * (int * int)

  let tests =
    let run_test path expected_result =
      let actual_result =
        Mmdb.open_file path
        |> Result.map ~f:(fun mmdb ->
               let languages =
                 Mmdb.languages mmdb |> List.map ~f:Mmdb.Language.to_string
               in
               let version =
                 Mmdb.binary_format_version mmdb
                 |> Mmdb.Version_number.to_major_and_minor
               in
               languages, version )
      in
      let properties_testable = Alcotest.(pair (list string) (pair int int)) in
      let open_file_error_testable =
        Alcotest.testable Mmdb.Open_file_error.pp (fun expected actual ->
            match expected, actual with
            | `File_open_error _, `File_open_error _ -> true
            | `Invalid_metadata _, `Invalid_metadata _ -> true
            | `Unknown_database_format _, `Unknown_database_format _ -> true
            | `Corrupt_search_tree _, `Corrupt_search_tree _ -> true
            | `Io_error _, `Io_error _ -> true
            | `Out_of_memory _, `Out_of_memory _ -> true
            | `Invalid_data _, `Invalid_data _ -> true
            | _, _ -> false )
      in
      let result_testable =
        Alcotest.(result properties_testable open_file_error_testable)
      in
      let message = "Mmdb.open_file result is not as expected" in
      Alcotest.check result_testable message expected_result actual_result
    in
    [ test_case "Successfully opens a valid MMDB file" (fun () ->
          run_test Path.correct (Ok (["en"], (2, 2))) );
      test_case
        "Returns 'File_open_error' when opening a non-existing MMDB file"
        (fun () -> run_test Path.incorrect (Error (`File_open_error ""))) ]
end

module Metadata_suite = struct
  let tests =
    [ test_case "Library version is a dot-separated list of integers" (fun () ->
          let version_number_strings = String.split Mmdb.library_version ~on:'.' in
          List.iter version_number_strings ~f:(fun each ->
              Int.of_string each |> Fn.ignore ) ) ]
end

module Fetching = struct
  let tests name fetch success_value value_testable =
    let run_test ip expected_result =
      match Mmdb.open_file Path.correct with
      | Error e -> Mmdb.Open_file_error.show e |> Alcotest.fail
      | Ok mmdb ->
          let actual_result = fetch mmdb ip in
          let fetch_error_testable =
            Alcotest.testable Mmdb.Fetch_error.pp (fun expected actual ->
                match expected, actual with
                | `Unsupported_data_type _, `Unsupported_data_type _ -> true
                | `Invalid_address_info, `Invalid_address_info -> true
                | `Ipv6_lookup_in_ipv4_database _, `Ipv6_lookup_in_ipv4_database _ ->
                    true
                | `Corrupt_search_tree _, `Corrupt_search_tree _ -> true
                | `Io_error _, `Io_error _ -> true
                | `Out_of_memory _, `Out_of_memory _ -> true
                | `Invalid_data _, `Invalid_data _ -> true
                | _, _ -> false )
          in
          let expectation_testable =
            Alcotest.(result (option value_testable) fetch_error_testable)
          in
          let message = Printf.sprintf "Lookup of %s is not as expected" name in
          Alcotest.check expectation_testable message expected_result actual_result
    in
    [ test_case
        (Printf.sprintf "Successfully fetches %s for a valid IP address" name)
        (fun () -> run_test Ip.correct (Ok (Some success_value)));
      test_case
        (Printf.sprintf
           "Returns 'Invalid_address_info' when fetching %s for an invalid IP address"
           name)
        (fun () -> run_test Ip.incorrect (Error `Invalid_address_info)) ]
end

module Coordinate_fetching_suite = struct
  let tests =
    let coordinates_testable =
      Alcotest.(
        testable Mmdb.Coordinates.pp (fun first second ->
            let precision = 1e-6 in
            let close x y = Float.(abs (x - y) <= precision) in
            close first.latitude second.latitude
            && close first.longitude second.longitude ))
    in
    let fetch_coordinates db ip = Mmdb.Coordinates.(from_db db ip location) in
    Fetching.tests
      "coordinates"
      fetch_coordinates
      Mmdb.Coordinates.{latitude = 1.2; longitude = 3.4}
      coordinates_testable
end

module Country_code_fetching_suite = struct
  let tests =
    let fetch_country_code db ip = Mmdb.String.(from_db db ip country_code) in
    Fetching.tests "country code" fetch_country_code "Country" Alcotest.string
end

module Country_name_fetching_suite = struct
  let tests =
    let language = Mmdb.Language.of_string "en" in
    let query = Mmdb.String.country_name language in
    let fetch_country_name db ip = Mmdb.String.(from_db db ip query) in
    Fetching.tests "country name" fetch_country_name "Atlantis" Alcotest.string
end

module Region_code_fetching_suite = struct
  let tests =
    let fetch_region_code db ip = Mmdb.String.(from_db db ip region_code) in
    Fetching.tests "region code" fetch_region_code "Region" Alcotest.string
end

let () =
  let e2e_tests =
    List.concat
      [ Open_file_suite.tests;
        Metadata_suite.tests;
        Coordinate_fetching_suite.tests;
        Country_code_fetching_suite.tests;
        Country_name_fetching_suite.tests;
        Region_code_fetching_suite.tests ]
  in
  Alcotest.run "ocaml-mmdb test suite" ["End-to-end", e2e_tests]
