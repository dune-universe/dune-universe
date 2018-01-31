(*
  Our cmd-free app definitions and models
*)
open Core_normalization
open Core_util
open Printf
open Core_runtime

let mkdir_p dir =
  split Filename.dir_sep dir |>
  List.fold_left
  ( fun acc basename ->
    let path = acc ^ Filename.dir_sep ^ basename in
    if not (Sys.file_exists path) then
      Unix.mkdir path 0o755;
    path
  )
  "" |>
  ignore

let gen_extension ~nocache ~framework ~cache_dir ~ignore ~only ~targets ~ignore_path =
  let _ = TestFramework.of_string framework in
  let detection = "dir" in
  let get_int () =
    (Random.int 9999) + 1 in
  let msg = "This file is supposed to be generated before build with a random ID." in
  let id = sprintf "%d%d%d" (get_int ()) (get_int ()) (get_int ()) in
  ( sprintf "(*\n  %s\n  ID = %s\n*)\nlet () =\n  [%%dryunit\n    { cache_dir   = \"%s\"\n    ; cache       = %s\n    ; framework   = \"%s\"\n    ; ignore      = \"%s\"\n    ; only        = \"%s\"\n    ; detection   = \"%s\"\n    ; ignore_path = \"%s\"\n    }\n  ]\n"
      msg id cache_dir (string_of_bool @@ not nocache) framework ignore only detection ignore_path
  ) |>
  fun output ->
  if List.length targets == 0 then
    print_endline output
  else
    ( List.iter
      ( fun target ->
        let path = Sys.getcwd () ^ Filename.dir_sep ^ target in
        let dir = Filename.dirname path in
        if not (Sys.file_exists dir) then
          mkdir_p dir;
        let oc = open_out path in
        Printf.fprintf oc "%s" output;
        close_out oc
      )
      targets
    )


let throw s =
  Printf.eprintf "%s\n" s;
  exit 1


let get_suites ~nocache ~framework ~cache_dir ~ignore ~only ~targets ~ignore_path ~detection ~main : TestSuite.t list =
  let custom_dir =
    if (cache_dir = ".dryunit") || (cache_dir = "_build/.dryunit") then None
    else
    ( if Core_util.starts_with cache_dir Filename.dir_sep then
        let () = mkdir_p cache_dir in
        Some cache_dir
      else if not !Core_runtime.running_ppx then
        Some cache_dir
      else
        throw ("Cache directory must be \".dryunit\" or a full custom path. Current value is `" ^ cache_dir ^ "`");
    ) in
  let ignore = filter_from ~throw ~name:"ignore" ignore in
  let only = filter_from ~throw ~name:"only" only in
  let ignore_path = filter_from ~throw ~name:"ignore_path" ignore_path in
  validate_filters ~throw ~ignore ~only;
  let filename = main in
  ( match detection with
    | "dir" -> detect_suites ~filename ~custom_dir ~cache_active:true ~ignore_path
    | "file" -> [ suite_from ~dir:(Filename.dirname filename) (Filename.basename filename) ]
    | _ -> throw "The field `detection` only accepts \"dir\" or \"file\"."
  )
  |> apply_filters ~only ~ignore

let gen_executable framework suites oc =
  (* let oc = open_out path in *)
  let f =
    ( match framework with
      | TestFramework.Alcotest -> Core_serializer.boot_alcotest
      | TestFramework.OUnit ->  Core_serializer.boot_ounit
    ) in
    f oc suites
    (* close_out oc *)
