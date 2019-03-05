open Base

module type ParametrizedTestSuite = sig
  type parameter

  val tests : (parameter * string) list

  val run_test : parameter -> unit
end

module ToAlcotestSuite (S : ParametrizedTestSuite) = struct
  let tests =
    List.map S.tests ~f:(fun (parameter, description) ->
        Alcotest.test_case description `Quick (fun () -> S.run_test parameter)
    )
end

module Path = struct
  let correct = Mmdb.Path.of_string "sample.mmdb"

  let incorrect = Mmdb.Path.of_string "sample-not-there.mmdb"
end

module Ip = struct
  let correct = Mmdb.Ip.of_string "172.56.31.240"

  let incorrect = Mmdb.Ip.of_string "blah 172.56.31.240"
end

module OpenFileSuite = ToAlcotestSuite (struct
  module Expectation = struct
    type t = (unit, Mmdb.Open_file_error.t) Result.t

    let success = Ok ()

    let file_open_error = Error (`File_open_error "")

    let check expected actual =
      let open_file_error_testable =
        Alcotest.testable Mmdb.Open_file_error.pp (fun expected actual ->
            match (expected, actual) with
            | `File_open_error _, `File_open_error _ -> true
            | `Invalid_metadata _, `Invalid_metadata _ -> true
            | `Unknown_database_format _, `Unknown_database_format _ -> true
            | `Corrupt_search_tree _, `Corrupt_search_tree _ -> true
            | `Io_error _, `Io_error _ -> true
            | `Out_of_memory _, `Out_of_memory _ -> true
            | `Invalid_data _, `Invalid_data _ -> true
            | _, _ -> false )
      in
      let expectation_testable =
        Alcotest.(result unit open_file_error_testable)
      in
      let message = "Mmdb.open_file result is not as expected" in
      Alcotest.check expectation_testable message expected actual
  end

  type parameter = Mmdb.Path.t * Expectation.t

  let test path expectation description = ((path, expectation), description)

  let tests =
    [ test Path.correct Expectation.success
        "Successfully opens a valid MMDB file"
    ; test Path.incorrect Expectation.file_open_error
        "Returns 'File_open_error' when opening a non-existing MMDB file" ]

  let run_test (path, expected) =
    let actual = Mmdb.open_file path |> Result.ignore in
    Expectation.check expected actual
end)

module FetchingCommon = struct
  module Expectation = struct
    type value_found = bool

    type t = (value_found, Mmdb.Lookup_error.t) Result.t

    let value_found = Ok true

    let invalid_address_info = Error `Invalid_address_info

    let check fetch_name expected actual =
      let open_file_error_testable =
        Alcotest.testable Mmdb.Lookup_error.pp (fun expected actual ->
            match (expected, actual) with
            | `Unsupported_data_type _, `Unsupported_data_type _ -> true
            | `Invalid_address_info, `Invalid_address_info -> true
            | `Invalid_lookup_path _, `Invalid_lookup_path _ -> true
            | ( `Lookup_path_does_not_match_data _
              , `Lookup_path_does_not_match_data _ ) ->
                true
            | `Invalid_node_number _, `Invalid_node_number _ -> true
            | `Ipv6_lookup_in_ipv4_database _, `Ipv6_lookup_in_ipv4_database _
              ->
                true
            | `Corrupt_search_tree _, `Corrupt_search_tree _ -> true
            | `Io_error _, `Io_error _ -> true
            | `Out_of_memory _, `Out_of_memory _ -> true
            | `Invalid_data _, `Invalid_data _ -> true
            | _, _ -> false )
      in
      let expectation_testable =
        Alcotest.(result bool open_file_error_testable)
      in
      let message =
        Printf.sprintf "Lookup of %s is not as expected" fetch_name
      in
      Alcotest.check expectation_testable message expected actual
  end

  type parameter = Mmdb.Ip.t * Expectation.t

  let test path expectation description = ((path, expectation), description)

  let tests fetch_name =
    [ Printf.sprintf "Can fetch %s for a valid IP address" fetch_name
      |> test Ip.correct Expectation.value_found
    ; Printf.sprintf
        "Returns 'Invalid_lookup_path' when fetching %s for an invalid IP \
         address"
        fetch_name
      |> test Ip.incorrect Expectation.invalid_address_info ]

  let run_test fetch_name fetch (ip, expected) =
    match Mmdb.open_file Path.correct with
    | Error e -> Mmdb.Open_file_error.show e |> Alcotest.fail
    | Ok mmdb ->
        let actual =
          fetch mmdb ip
          |> Result.map ~f:(function None -> false | Some _ -> true)
        in
        Expectation.check fetch_name expected actual
end

module CoordinateFetchingSuite = ToAlcotestSuite (struct
  include FetchingCommon

  let fetch_name = "coordinates"

  let tests = tests fetch_name

  let run_test = run_test fetch_name Mmdb.coordinates
end)

module CountryCodeFetchingSuite = ToAlcotestSuite (struct
  include FetchingCommon

  let fetch_name = "country code"

  let tests = tests fetch_name

  let run_test = run_test fetch_name Mmdb.country_code
end)

module RegionCodeFetchingSuite = ToAlcotestSuite (struct
  include FetchingCommon

  let fetch_name = "region code"

  let tests = tests fetch_name

  let run_test = run_test fetch_name Mmdb.region_code
end)

let () =
  let e2e_tests =
    List.concat
      [ OpenFileSuite.tests
      ; CoordinateFetchingSuite.tests
      ; CountryCodeFetchingSuite.tests
      ; RegionCodeFetchingSuite.tests ]
  in
  Alcotest.run "ocaml-mmdb test suite" [("End-to-end", e2e_tests)]
