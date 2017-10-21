module Make_core_runtime
  ( Capitalize: sig
      val capitalize_ascii: bytes -> bytes
    end
  ) = struct


open Printf
open Capitalize
open Core_util

module TestDescription = struct
  type t = {
    test_name: string;
    test_title: string;
  }
end

module TestSuite = struct
  type t = {
    suite_title: string;
    suite_name: string;
    suite_path: string;
    timestamp: float;
    tests: TestDescription.t list;
  }
end

module TestFramework = struct
  type t = Alcotest | OUnit

  let of_string = function
    | "alcotest" -> Alcotest
    | "ounit" -> OUnit
    | other -> raise (Invalid_argument ("Not supported test framework: " ^ other))

  let to_string = function
    | Alcotest -> "alcotest"
    | OUnit -> "ounit"

  let package = function
    | Alcotest -> "alcotest"
    | OUnit -> "oUnit"

end

open TestDescription
open TestSuite


let title_from (v:bytes) : string =
  Bytes.to_string @@ capitalize_ascii (Core_util.util_title_from v)

let title_from_no_padding (v:bytes) : string  =
  Bytes.to_string @@ capitalize_ascii (Core_util.util_title_from_filename v)

let in_build_dir () =
  is_substring (Sys.getcwd ()) "build/"

let should_ignore ~ignore name =
  match ignore with
  | [] -> false
  | _ -> List.exists (fun v -> Core_util.is_substring name v) ignore


(* XXX: we could add support for inline namespaced tests here *)
let rec should_ignore_path ~only path =
  ( match only with
    | [] -> false
    | _ -> List.exists (fun v -> Core_util.is_substring path v) only
  )

let extract_from ~filename : TestDescription.t list =
  tests_from filename |>
  List.map
    (fun test_name ->
      let test_title : string = title_from test_name in
      let test_name = Bytes.to_string test_name in
      { test_name; test_title }
    )


let timestamp_from filename =
  Unix.((stat filename).st_mtime)

let suite_from ~dir filename : TestSuite.t =
  let suite_path = dir ^ sep ^ filename in
  let name = (Filename.basename filename) in
  { suite_name = to_string @@ capitalize_ascii (to_bytes (Filename.chop_suffix name ".ml"));
    suite_title = title_from_no_padding (to_bytes (Filename.chop_suffix name ".ml"));
    suite_path;
    timestamp = timestamp_from suite_path;
    tests = extract_from ~filename:(sprintf "%s%s%s" dir sep name)
  }

let test_name ~current_module suite test =
  if current_module then
    test.test_name
  else
    (suite.suite_name ^ "." ^ test.test_name)

let cache_dir () =
  let flag_ref = ref false in
  let root_found = ref "" in
  Str.split (Str.regexp sep) (Sys.getcwd ()) |>
  List.rev |>
  List.filter
    ( fun dir ->
      if !flag_ref then
        true
      else
      ( if (dir = "_build") || (dir = "build") then
        ( flag_ref := true;
          root_found := dir;
        );
        false
      )
    ) |>
  List.rev |>
  function
  | []  -> failwith "Dryunit is not being preprocessed from build directory"
  | l -> sep ^ (String.concat sep l) ^ sep ^ !root_found ^ sep ^ ".dryunit"


let cache_file ~main ~custom_dir =
  let dir =
    ( match custom_dir with
      | None -> cache_dir ()
      | Some dir -> dir
    ) in
  (* let dir = cache_dir () in *)
  if not @@ Sys.file_exists dir then
    Unix.mkdir dir 0o755;
  close_out (open_out (dir ^ sep ^ ".jbuilder-keep"));
  let s : bytes = Bytes.of_string (main ^ Sys.ocaml_version) in
  let hash = Digest.(to_hex @@ bytes s) in
  dir ^ sep ^ hash

let save_cache ~main ~custom_dir ~cache_active suites =
  if not cache_active then ()
  else
  ( let path = cache_file ~main ~custom_dir in
    if Sys.file_exists path then
      Sys.remove path;
    let c = open_out_bin path in
    Marshal.to_channel c suites [];
    flush c;
    close_out c
  )


let load_cache ~main ~custom_dir ~cache_active =
  let path = cache_file ~main ~custom_dir in
  if cache_active && Sys.file_exists path then
  ( let c = open_in_bin path in
    let suites : TestSuite.t list = Marshal.from_channel c in
    close_in c;
    suites
  )
  else []

let get_from_cache ~cache ~dir filename : TestSuite.t option =
  try
    let filename = dir ^ sep ^ filename in
    List.find
    ( fun s ->
      if s.suite_path = filename  then
        if timestamp_from s.suite_path = s.timestamp then
          true
        else false
      else false
    )
    cache |>
    fun s ->
    Some s
  with
    Not_found -> None

let detect_suites ~filename ~custom_dir ~cache_active ~(ignore_path:string list) : TestSuite.t list =
  let cache = load_cache ~main:filename ~custom_dir ~cache_active in
  let cache_dirty = ref false in
  let dir = Filename.dirname filename in
  let main_basename = Filename.basename filename in
  Sys.readdir dir |>
  Array.to_list |>
  List.filter
  ( fun v ->
    if v = main_basename then
      false
    else
    ( let basename = Bytes.of_string @@ Filename.basename v in
      let len = Bytes.length basename in
      if Bytes.index basename '.' == (len - 3) && len > 7 then
        let c = Bytes.get basename (len - 8) in
        if c == 't' || c == 'T' then
          if ends_with v "ests.ml" then
            not (should_ignore_path ~only:ignore_path (to_string basename))
          else false
        else false
      else false
    )
  ) |>
  (* only over records already in cache, invalidating the cache if needed *)
  List.map
  ( fun filename ->
    ( match get_from_cache ~dir ~cache filename with
      | Some suite -> suite
      | None ->
        ( cache_dirty := true;
          suite_from ~dir filename
        )
    )
  ) |>
  fun suites ->
  if !cache_dirty then
    save_cache ~main:filename ~custom_dir ~cache_active suites;
  suites

let pp name tests =
  print_endline ("Tests in `" ^ name ^ "`");
  List.iter (fun t -> Printf.printf " - %s [%s]\n" t.test_title t.test_name) tests

let print_tests_from ~filename : string =
  let tests = ref [] in
  let _ : unit =
    detect_suites ~filename ~custom_dir:None ~cache_active:true ~ignore_path:[]
    |> List.iter
       ( fun suite ->
         tests := !tests @ suite.tests
      )
  in
  !tests |>
  List.map (fun test -> test.test_title) |>
  List.sort String.compare |>
  String.concat "\n"


module Test = struct
  let name (t:TestDescription.t) = t.test_name
  let title (t:TestDescription.t) = t.test_title
end

let extract_name_from_file ~filename =
  capitalize_ascii

let mkdir_p dir =
  split sep dir |>
  List.fold_left
  ( fun acc basename ->
    let path = acc ^ sep ^ basename in
    if not (Sys.file_exists path) then
      Unix.mkdir path 0o755;
    path
  )
  "" |>
  ignore


let filter_from ~throw ~name value : string list =
  let l = split " " value in
  List.iter
    ( fun v ->
      if String.length v < 4 then
        throw (sprintf "Each word in the field `%s` must be at least 3 chars long" name);
      if v = "test" then
        throw (sprintf "You are not allowed to use the word `test` in the field `%s`" name)
    )
    l;
  l


let should_ignore ~ignore name =
  match ignore with
  | [] -> assert false
  | _ -> List.exists (fun v -> Core_util.is_substring name v) ignore

let should_filter ~only name =
  match only with
  | [] -> assert false
  | _ -> List.exists (fun v -> Core_util.is_substring name v) only


let apply_filters ~only ~(ignore:string list) suites =
  let only_tests tests =
    ( if List.length ignore == 0 then tests
      else
        List.filter (fun test -> not (should_ignore ~ignore test.test_name)) tests
    ) |>
    fun tests ->
    ( if List.length only == 0 then tests
      else
        List.filter (fun test -> should_filter ~only test.test_name) tests
    )
  in
  ( List.fold_left
      ( fun acc suite ->
        match only_tests suite.tests with
        | [] -> acc
        | active_tests -> { suite with tests = active_tests } :: acc
      )
      []
      suites
  )

let validate_filters ~throw ~ignore ~only =
  match ignore, only with
  | [], [] -> ()
  | _v, [] -> ()
  | [], _v -> ()
  | _ ->
    List.iter
      ( fun v_filter ->
        if List.exists (fun v -> v_filter = v) ignore then
          throw (sprintf "Query `%s` appears in the fields `filter` and `ignore`." v_filter)
      )
      only

end
