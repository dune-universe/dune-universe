#if OCAML_VERSION < (4, 03, 0)
#define Pconst_string Const_string
#define OCaml_OMP OCaml_402
#define Ast_OMP Ast_402
#define Ppx_tools_OMP Ppx_tools_402
#elif OCAML_VERSION < (4, 04, 0)
#define OCaml_OMP OCaml_403
#define Ast_OMP Ast_403
#define Ppx_tools_OMP Ppx_tools_403
#elif OCAML_VERSION < (4, 05, 0)
#define OCaml_OMP OCaml_404
#define Ast_OMP Ast_404
#define Ppx_tools_OMP Ppx_tools_404
#elif OCAML_VERSION < (4, 06, 0)
#define OCaml_OMP OCaml_405
#define Ast_OMP Ast_405
#define Ppx_tools_OMP Ppx_tools_405
#elif OCAML_VERSION < (4, 07, 0)
#define OCaml_OMP OCaml_406
#define Ast_OMP Ast_406
#define Ppx_tools_OMP Ppx_tools_406
#endif


let format = Printf.sprintf

module Ppx_dryunit_runtime = struct
  open Printf

  let throw ~loc msg =
    raise (Location.Error (Location.error ~loc msg))

  type record_fields = (Longident.t Asttypes.loc *  Parsetree.expression) list

  let validate_params ~loc (current: record_fields) expected_fields =
    let param n =
      ( match fst @@ List.nth current n with
        | {txt = Lident current} -> current
        | _ -> throw ~loc ("Unexpected structure")
      ) in
    let check_param n expected =
      let current =
        ( try param n with
          | _ -> throw ~loc ("Missing configuration: " ^ expected)
        ) in
      if not (current = expected) then
        throw ~loc (format "I was expecting `%s`, but found `%s`." expected current)
    in
    List.iteri
      ( fun i name ->
        check_param i name
      )
      expected_fields;
    let expected_len = List.length expected_fields in
    if List.length current > expected_len then
      throw ~loc (format "Unknown configuration field: `%s`." (param expected_len))


  module Util = struct
    let is_substring string substring =
      let string, substring = Bytes.of_string string, Bytes.of_string substring in
      let ssl = Bytes.length substring and sl = Bytes.length string in
      if ssl = 0 || ssl > sl then false else
        let max = sl - ssl and clone = Bytes.create ssl in
        let rec check pos =
          pos <= max && (
            Bytes.blit string pos clone 0 ssl ; clone = substring
            || check (Bytes.index_from string (succ pos) (Bytes.get substring 0))
          )
        in
        try check (Bytes.index string (Bytes.get substring 0))
        with Not_found -> false

    let starts_with s1 s2 =
      let open  String in
      let len1, len2 = length s1, length s2 in
      if len1 == len2 then
        s1 = s2
      else if len1 < len2 then
        false
      else begin
        (sub s1 0 len2) = s2
      end

    let ends_with s1 s2 =
      let open  String in
      let len1, len2 = length s1, length s2 in
      if len1 == len2 then
        s1 = s2
      else if len1 < len2 then
        false
      else begin
        (sub s1 (len1 - len2) len2) = s2
      end

    (*
      Hardcore filter to let bindings starting with "test_"
      It does not recognizes test functions inside nested modules
    *)
    let is_possible_test_entry (line:string) =
      let open String in
      if length line > 20 then
        if get line 7 == ' ' then
          if get line 10 == 'P' then
            if get line 15 == 'v' then
              (sub line 20 4) = "test"
            else false
          else false
        else false
      else false

    (**
      Definition used in parsing logic.

      When a new test entry is added, we activate probation logic,
      that checks the following input lines until we know it's not
      a false positive.
     *)
    type probation = {
      active: bool ref;
      time: int ref;
    }

    (* limit for probation iterations, assiciated with time counter *)
    let deadline = 14

    let fun_name line : string =
      String.(sub line 20 ((index_from line 21 '"') - 20))

    let new_probation () = {
      active = ref false;
      time = ref 0;
    }

    let start_probation ~probation () =
      probation.active := true;
      probation.time := 0

    let reset_probation ~probation () =
      probation.active := false;
      probation.time := 0

    let can_confirm_test_entry line =
      let open String in
      if length line > 20 then
        if get line 7 == ' ' then
          if get line 10 == 'P' then
            if get line 15 == 'v' then
              (sub line 20 5) = "test_"
            else false
          else false
        else false
      else false

    let probation_pass ~probation line =
      probation.time := !(probation.time) + 1;
      if !(probation.time) > deadline || String.length line < 8 then
        let () = reset_probation ~probation () in
        Some false
      else
      ( if !(probation.time) > 3 then
        ( if is_substring line "construct \"()\"" then
          let () = reset_probation ~probation () in
          Some true
          else
          ( if is_substring line "_var \"" then (* ounit ctx's param *)
           let () = reset_probation ~probation () in
           Some true
           else None
          )
        )
        else None
      )


    let feed_with ~chan =
      let lines = ref [] in
      try
        let probation = new_probation () in
        reset_probation ~probation ();
        while true; do
          let line = input_line chan in
          if !(probation.active) then
          (match probation_pass ~probation line with
            | Some true -> ()
            | Some false ->
                lines := List.tl !lines
            | _ -> ()
          )
          else
          ( if is_possible_test_entry line then
            ( lines := (fun_name line) :: !lines;
              start_probation ~probation ()
            );
          )
        done; !lines
      with End_of_file ->
        close_in chan;
        List.rev !lines



    let tests_from path =
      let cmd = format "ocamlopt -dparsetree %s 2>&1 >/dev/null" path in
      let chan = Unix.open_process_in cmd in
      feed_with ~chan


    let title_from name =
      let name = Bytes.of_string name in
      let name =
        if (Bytes.get name 4) = '_' then
          Bytes.sub name 4 ((Bytes.length name) - 4)
        else name in
      let i, len = ref 0, Bytes.length name in
      while !i < len do
        try
          i := Bytes.index_from name !i '_';
          Bytes.set name !i ' ';
          i := !i + 1;
        with
          _ -> i := len;
      done;
      name |> Bytes.trim |> Bytes.to_string


    let title_from_filename name =
      let name = Bytes.of_string name in
      let len = Bytes.length name in
      let i, len = ref 0, len in
      while !i < len do
        try
          i := Bytes.index_from name !i '_';
          Bytes.set name !i ' ';
          i := !i + 1;
        with
          _ -> i := len;
      done;
      name |> Bytes.trim |> Bytes.to_string
  end

  open Util



  #if OCAML_VERSION < (4, 03, 0)
    let capitalize_ascii = String.capitalize
  #else
    let capitalize_ascii = String.capitalize_ascii
  #endif

  let sep = Filename.dir_sep


  type test = {
    test_name: string;
    test_title: string;
  }

  type testsuite = {
    suite_title: string;
    suite_name: string;
    suite_path: string;
    timestamp: float;
    tests: test list;
  }

  let title_from v = capitalize_ascii @@ title_from v
  let title_from_no_padding v = capitalize_ascii @@ title_from_filename v

  let in_build_dir () =
    is_substring (Sys.getcwd ()) "build/"

  let should_ignore ~ignore name =
    match ignore with
    | [] -> false
    | _ -> List.exists (fun v -> Util.is_substring name v) ignore

  let extract_from ~filename : test list =
    tests_from filename |>
    List.map
    (fun test_name ->
      { test_name; test_title = title_from test_name }
    )


  let timestamp_from filename =
    Unix.((stat filename).st_mtime)

  let suite_from ~dir filename : testsuite =
    let name = (Filename.basename filename) in
    { suite_name = capitalize_ascii (Filename.chop_suffix name ".ml");
      suite_title = title_from_no_padding (Filename.chop_suffix name ".ml");
      suite_path = dir ^ sep ^ filename;
      timestamp = timestamp_from (dir ^ sep ^ filename);
      tests = extract_from ~filename:(format "%s%s%s" dir sep name)
    }

  let test_name ~current_module suite test =
    if current_module then
      test.test_name
    else
      (suite.suite_name ^ "." ^ test.test_name)

  let split pattern value =
    Str.split (Str.regexp pattern) value

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
    let hash = Digest.(to_hex @@ bytes (main ^ Sys.ocaml_version)) in
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
      let suites : testsuite list = Marshal.from_channel c in
      close_in c;
      suites
    )
    else []

  let get_from_cache ~cache ~dir filename : testsuite option =
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

  let detect_suites ~filename ~custom_dir ~cache_active : testsuite list =
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
      ( let basename = Filename.basename v in
        (* XXX: not sure if we should apply the ignore filter here *)
        (* if should_ignore ~ignore basename then
          false
        else *)
          let len = String.length basename in
          (ends_with v ".ml") && (Bytes.index basename '.' == (len - 3))
      )
    ) |>
    (* filter over records already in cache, invalidating the cache if needed *)
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
      detect_suites ~filename ~custom_dir:None ~cache_active:true
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
    let name (t:test) = t.test_name
    let title (t:test) = t.test_title
  end

  let extract_name_from_file ~filename =
    capitalize_ascii
end






open Migrate_parsetree
open OCaml_OMP.Ast
open Parsetree

open Ast_OMP
open Ppx_tools_OMP
open Ast_convenience

open Ast_helper
open Ppx_dryunit_runtime


let bootstrap_alcotest suites =
  suites |>
  List.map
  ( fun suite ->
    let current_module = (suite.suite_path = !Location.input_name) in
    suite.tests |>
    List.map
    ( fun t ->
      tuple
      [ str t.test_title
      ; Exp.variant "Quick" None
      ; evar (test_name ~current_module suite t)
      ]
    ) |>
    ( fun test_set ->
      tuple [ str suite.suite_title; list test_set ]
    )
  ) |>
  ( fun pairs ->
    app (evar "Alcotest.run") [ str "Default"; list pairs ]
  )


let bootstrap_ounit suites =
  suites |>
  List.map
  ( fun suite ->
    let current_module = (suite.suite_path = !Location.input_name) in
    suite.tests |>
    List.map
    ( fun t ->
      app (evar "OUnit2.>::") [ str (suite.suite_title ^ "." ^ t.test_name);
        evar (test_name ~current_module suite t) ]
    )
  ) |>
  List.flatten |>
  ( fun tests ->
    app (evar "OUnit2.run_test_tt_main") [ app (evar "OUnit2.>:::") [str "Default"; list tests] ]
  )

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


let filter_from ~loc ~name value : string list =
  let l = split " " value in
  List.iter
    ( fun v ->
      if String.length v < 4 then
        throw ~loc (format "Each word in the field `%s` must be at least 3 chars long" name);
      if v = "test" then
        throw ~loc (format "You are not allowed to use the word `test` in the field `%s`" name)
    )
    l;
  l


let should_ignore ~ignore name =
  match ignore with
  | [] -> assert false
  | _ -> List.exists (fun v -> Util.is_substring name v) ignore

let should_filter ~filter name =
  match filter with
  | [] -> assert false
  | _ -> List.exists (fun v -> Util.is_substring name v) filter


let apply_filters ~loc ~filter ~ignore suites =
  let filter_tests tests =
    ( if List.length ignore == 0 then tests
      else
        List.filter (fun test -> not (should_ignore ~ignore test.test_name)) tests
    ) |>
    fun tests ->
    ( if List.length filter == 0 then tests
      else
        List.filter (fun test -> should_filter ~filter test.test_name) tests
    )
  in
  ( List.fold_left
      ( fun acc suite ->
        match filter_tests suite.tests with
        | [] -> acc
        | active_tests -> { suite with tests = active_tests } :: acc
      )
      []
      suites
  )

let validate_filters ~loc ~ignore ~filter =
  match ignore, filter with
  | [], [] -> ()
  | _v, [] -> ()
  | [], _v -> ()
  | _ ->
    List.iter
      ( fun v_filter ->
        if List.exists (fun v -> v_filter = v) ignore then
          throw ~loc (format "Query `%s` appears in the fields `filter` and `ignore`." v_filter)
      )
      filter


let boot ~loc ~cache_dir ~cache_active ~framework ~ignore ~filter ~detection =
  let f =
    ( match framework with
      | "alcotest" -> bootstrap_alcotest
      | "ounit" -> bootstrap_ounit
      | _ -> throw ~loc (format "Test framework not recognized: `%s`" framework)
    ) in
  let custom_dir =
    if (cache_dir = ".dryunit") || (cache_dir = "_build/.dryunit") then None
    else
    ( if Util.starts_with cache_dir Filename.dir_sep then
        let () = mkdir_p cache_dir in
        Some cache_dir
      else
        throw ~loc ("Cache directory must be \".dryunit\" or a full custom path. Current value is `" ^ cache_dir ^ "`");
    ) in
  let ignore = filter_from ~loc ~name:"ignore" ignore in
  let filter = filter_from ~loc ~name:"filter" filter in
  let suites =
    let filename = !Location.input_name in
    ( match detection with
      | "dir" -> detect_suites ~filename ~custom_dir ~cache_active
      | "file" -> [ suite_from ~dir:(Filename.dirname filename) (Filename.basename filename) ]
      | _ -> throw ~loc "The field `detection` only accepts \"dir\" or \"file\"."
    ) in
  validate_filters ~loc ~ignore ~filter;
  f (apply_filters ~loc ~filter ~ignore suites)


let rewriter _config _cookies =
  let super = Ast_mapper.default_mapper in
  if not (in_build_dir ()) then
    { super with expr = fun _ _ -> unit () }
  else
  let expr self e =
    match e.pexp_desc with
    (* debug just returns a string with detected tests *)
    | Pexp_extension ({ txt = "dryunit_debug"; _ }, PStr []) ->
      let output = Ppx_dryunit_runtime.print_tests_from ~filename:!Location.input_name in
      { e with pexp_desc = Pexp_constant (Pconst_string (output, None)) }

    (* debug just returns a string with detected tests *)
    | Pexp_extension ({ txt = "dryunit_debug2"; _ }, PStr []) ->
      app (evar "Printf.printf") [str "%s %s"; str "Hello"; str "World!" ]

    (* alcotest *)
    | Pexp_extension ({ txt = "alcotest"; _ }, PStr []) ->
      bootstrap_alcotest (detect_suites ~filename:!Location.input_name
        ~custom_dir:None ~cache_active:true)

    (* ounit *)
    | Pexp_extension ({ txt = "ounit"; _ }, PStr []) ->
      bootstrap_ounit (detect_suites ~filename:!Location.input_name
        ~custom_dir:None ~cache_active:true)

    (* new-interface *)
    | Pexp_extension ({ txt = "dryunit"; _ },
        PStr [ {pstr_desc = (Pstr_eval ({pexp_desc = Pexp_record (configs, None);
          pexp_loc; _}, attr)); _} ]) ->
        ( match configs with
          | [({txt = Lident "cache_dir"},
              {pexp_desc = Pexp_constant (Pconst_string (cache_dir, None))});
             ({txt = Lident "cache"},
              {pexp_desc = Pexp_construct ({txt = Lident cache}, None)});
             ({txt = Lident "framework"},
              {pexp_desc = Pexp_constant (Pconst_string (framework, None))});
             ({txt = Lident "ignore"},
              {pexp_desc = Pexp_constant (Pconst_string (ignore, None))});
             ({txt = Lident "filter"},
              {pexp_desc = Pexp_constant (Pconst_string (filter, None))});
             ({txt = Lident "detection"},
              {pexp_desc = Pexp_constant (Pconst_string (detection, None))})]
            when cache = "true" || cache = "false" ->
              let cache_active = (cache = "true") in
              boot ~loc:e.pexp_loc ~cache_dir ~cache_active ~framework ~ignore ~filter ~detection
         | _ ->
          validate_params ~loc:e.pexp_loc configs
            ["cache_dir"; "cache"; "framework"; "ignore"; "filter"; "detection" ];
          throw ~loc:e.pexp_loc "Configuration for ppx_dryunit is invalid."
        )
    | Pexp_extension ({ txt = "dryunit"; _ }, _ ) ->
        throw ~loc:e.pexp_loc "Dryunit configuration should defined as a record."

    (* anything else *)
    | _ -> super.expr self e
  in
  { super with expr }

let () =
  Driver.register ~name:"ppx_dryunit"
    (module OCaml_OMP)
    rewriter
