open OUnit
open Semver

let valids, invalids, sorted =
  let json = Yojson.Basic.from_file "fixtures.json" in
  let open Yojson.Basic.Util in
  let read_list key = json |> member key |> to_list |> List.map to_string in
  read_list "valid", read_list "invalid", read_list "compare"

let test_valids () =
  let assert_iso v =
    match of_string v with
    | Some v' ->
       assert_equal (to_string v') v;
       assert_equal (Format.asprintf "%a" Semver.pp v') v
    | None -> assert_failure ("valid version detected as invalid: " ^ v)
  in
  List.iter assert_iso valids

let test_invalids () =
  List.iter (fun v -> assert_bool (v ^ " is invalid") (not (is_valid v))) invalids

let test_compare () =
  let rec of_string_list parsed raw =
    match raw with
    | [] -> parsed
    | hd :: tl -> match of_string hd with
      | Some v -> of_string_list (v :: parsed) tl
      | None -> of_string_list parsed tl
  in
  let list_printer lst =
    "\n[\n" ^ (lst |> List.map (fun s -> "  " ^ s ^ ";\n") |> String.concat "") ^ "]\n"
  in
  assert_equal ~printer:list_printer
    sorted
    (sorted |> of_string_list [] |> List.sort compare |> List.map to_string)

let test_from_parts () =
  let is_success = function
    | None -> false
    | Some _ -> true
  in
  assert_bool "negative version rejected"
    (not (is_success (from_parts (-1) 0 2 [] [])));
  assert_bool "invalid prerelease rejected"
    (not (is_success (from_parts 1 0 0 ["03"] [])));
  assert_bool "invalid build rejected"
    (not (is_success (from_parts 1 0 0 [] ["!"])));
  assert_bool "positive version accepted"
    (is_success (from_parts 1 0 2 ["3"; "foo"; "0a5b"] ["bar"]))

let suite =
  "semver suite" >:::
  ["test valids" >:: test_valids;
   "test invalids" >:: test_invalids;
   "test compare" >:: test_compare;
   "test from_parts" >:: test_from_parts]

let _ = run_test_tt_main suite
