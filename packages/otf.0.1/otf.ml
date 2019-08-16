(* CLI handler *)

type cli_action = RunTest | PrintConfig | RunCollection
let action = ref RunTest
let set_action a () = action := a

let debug = ref false
let silent = ref false

let max_diff = ref (1024*1024)

let cli_arguments = [
    "-print-config", Arg.Unit (set_action PrintConfig), "print and checks the config produced";
    "-run-test", Arg.Unit (set_action RunTest), "run the given test (default action)";
    "-run-collection", Arg.Unit (set_action RunCollection), "run the collection from the given directory";
    "-silent", Arg.Set silent, "only produce output on failure (only impacts -run-test).";
    "-d", Arg.Set debug, "keep temporary files instead of cleaning them up";
    "-max-diff", Arg.Set_int max_diff, "set the maximum length of the diff to present";
  ]

let test_names = ref []

let parse_filenames s = test_names:=(!test_names)@[s]

let usage_msg = "otf [-print-config|-run-test] <test_name>"

let error error_msg =
  let msg = usage_msg ^ "\nError: " ^ error_msg in
  prerr_endline msg;
  exit 1


(* Types and functions defining the options in the config file *)

type option_type = StringVal | IntVal | StringListVal

let string_of_value_type = function
  | StringVal -> "string"
  | IntVal -> "int"
  | StringListVal -> "string list"

type option_value = S of string | I of int | SL of string list

let quote_string s = "\"" ^ (String.escaped s) ^ "\""

let parse_value n t v =
  match t with
  | StringVal -> S v
  | StringListVal -> SL (String.split_on_char ' ' v)
  | IntVal ->
     try I (int_of_string v)
     with Failure _ -> error ("Variable \"" ^ (String.escaped n) ^ "\" should be an integer")

let string_of_value = function
  | None -> "None"
  | Some (S s) -> quote_string s
  | Some (I i) -> string_of_int i
  | Some (SL l) ->
     "[" ^ (String.concat ", " (List.map quote_string l)) ^ "]"

type option_description = { name: string;
                value_type: option_type;
                default_value: option_value option;
                required: bool }

let mk_str_option_d name default_value =
  { name;
    value_type = StringVal;
    default_value = Some (S default_value);
    required = true }

let mk_str_option_r name required =
  { name;
    value_type = StringVal;
    default_value = None;
    required }

let mk_int_option_d name default_value =
  { name;
    value_type = IntVal;
    default_value = Some (I default_value);
    required = true }

let mk_strlist_option_d name default_value =
  { name;
    value_type = StringListVal;
    default_value = Some (SL default_value);
    required = true }



(* Types defining the instanciated config *)

module Config = Map.Make (String)

type config_elt = { value_type: option_type;
                    value_content: option_value option;
                    required: bool;
                    origin: string}

type config = config_elt Config.t

let get_str config varname =
  match Config.find_opt varname config with
  | None -> error ("Unable to find variable \"" ^ varname ^ "\" in config")
  | Some { value_content = Some (S s); _ } -> s
  | Some _ -> error ("Expecting string in variable \"" ^ varname ^ "\" in config")

let get_strlist config varname =
  match Config.find_opt varname config with
  | None -> error ("Unable to find variable \"" ^ varname ^ "\" in config")
  | Some { value_content = Some (SL l); _ } -> l
  | Some _ -> error ("Expecting string list in variable \"" ^ varname ^ "\" in config")

let get_int config varname =
  match Config.find_opt varname config with
  | None -> error ("Unable to find variable \"" ^ varname ^ "\" in config")
  | Some { value_content = Some (I i); _ } -> i
  | Some _ -> error ("Expecting integer in variable \"" ^ varname ^ "\" in config")



(* Options and default configuration *)

let options = [mk_str_option_r "PROG" true;
               mk_strlist_option_d "ARGS" [];
               mk_int_option_d "EXIT_CODE" 0;
               mk_strlist_option_d "DIFFOPTIONS" [];
              ]

let update_default_config_with_option c (o: option_description) =
  let config_elt = {
      value_type = o.value_type;
      value_content = o.default_value;
      required = o.required;
      origin = ""
    } in
  Config.add o.name config_elt c

let init_config =
  List.fold_left update_default_config_with_option Config.empty options



(* Functions to manipulate configs *)

let print_config c =
  let print_elt n e =
    print_string n;
    print_string ": ";
    print_string (string_of_value_type e.value_type);
    print_string " = ";
    print_string (string_of_value e.value_content);
    print_string " [";
    if e.required && e.value_content = None then print_string "MISSING; ";
    if e.origin = ""
    then print_string "from default configuration"
    else print_string ("from \"" ^ e.origin ^ "\"");
    print_endline "]";
  in
  Config.iter print_elt c

let check_config c =
  let check_type n e error_msgs =
    match e.value_type, e.value_content with
    | StringVal, Some (S _)
    | IntVal, Some (I _)
    | StringListVal, Some (SL _)
    | _, None -> error_msgs
    | _, _ -> ("Invalid type for \"" ^ n ^ "\"")::error_msgs
  and check_required n e error_msgs =
    if e.value_content = None && e.required
    then ("Missing required element \"" ^ n ^ "\"")::error_msgs
    else error_msgs
  in
  let check_elt n e error_msgs =
    let res1 = check_type n e error_msgs in
    let res2 = check_required n e res1 in
    res2
  in
  Config.fold check_elt c []

let enrich_config filename may_not_exist config =
  let rec handle_file f c =
    try
      let line = input_line f in
      match String.split_on_char '=' line with
      | [] -> handle_file f c
      | varname::value ->
         let config_elt = match Config.find_opt varname c with
           | None -> error ("Invalid variable name \"" ^ (String.escaped varname) ^ "\"")
           | Some e -> e
         in
         let new_val = parse_value varname config_elt.value_type (String.concat "=" value) in
         let new_config_elt = { config_elt with
                                value_content = Some new_val;
                                origin = filename } in
         handle_file f (Config.add varname new_config_elt c)
    with End_of_file -> c
  in
  try
    let f = open_in filename in
    handle_file f config
  with Sys_error _ ->
    if may_not_exist
    then config
    else error ("Could not open config file \"" ^ (String.escaped filename) ^ "\"")



(* Functions to actually run tests *)

let open_temporary_file suffix =
  let filename = Filename.temp_file "otf_test_" suffix in
  Unix.unlink filename;
  let f = Unix.openfile filename [O_WRONLY; O_CREAT; O_EXCL] 0o400 in
  filename, f

let read_file_content filename =
  let f = open_in filename in
  let len = in_channel_length f in
  let n = min len !max_diff in
  let result = really_input_string f n in
  if len > !max_diff
  then result ^ "\n... truncated\n"
  else result

let compare_output config expected_filename produced_filename =
  let file_exists =
    try Unix.access expected_filename [Unix.R_OK; Unix.F_OK]; true
    with Unix.Unix_error _ -> false
  in
  if file_exists then begin
    let diff_out_filename, diff_out_fd = open_temporary_file "_diffout" in
    let devnull_fd = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
    let args = Array.of_list ("diff"::(get_strlist config "DIFFOPTIONS")@[expected_filename; produced_filename]) in
    let pid = Unix.create_process "diff" args devnull_fd diff_out_fd devnull_fd in
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 ->
       Unix.unlink diff_out_filename;
       None
    | _, Unix.WEXITED _
    | _, Unix.WSIGNALED _
    | _, Unix.WSTOPPED _ ->
      let diff_output = Some (read_file_content diff_out_filename) in
      Unix.unlink diff_out_filename;
      diff_output
  end else None (* If no expected output was found, just accept whatever was output by the program *)

let run_test (test_name, config) =
  let test_result = ref true
  and test_output = ref [] in

  let stdin_fd =
    try Unix.openfile (test_name ^ ".stdin") [Unix.O_RDONLY] 0
    with Unix.Unix_error _ -> Unix.openfile "/dev/null" [Unix.O_RDONLY] 0
  in
  let stdout_filename, stdout_fd = open_temporary_file "_out"
  and stderr_filename, stderr_fd = open_temporary_file "_err" in

  let progname = get_str config "PROG" in
  let args = Array.of_list (progname::(get_strlist config "ARGS")) in
  let pid = Unix.create_process progname args stdin_fd stdout_fd stderr_fd in

  let exit_code = match Unix.waitpid [] pid with
    | _, Unix.WEXITED exitcode -> exitcode
    | _, Unix.WSIGNALED _ -> -1024
    | _, Unix.WSTOPPED _ -> -2048
  in
  let expected_exit_code = get_int config "EXIT_CODE" in
  if exit_code <> expected_exit_code
  then begin
    test_output := ("Invalid exit code (got " ^ (string_of_int exit_code) ^
                      " while expecting " ^ (string_of_int expected_exit_code) ^ ")\n")::(!test_output);
    test_result := false
  end;
  begin
    match compare_output config (test_name ^ ".stdout") stdout_filename with
    | None -> ()
    | Some diff_output ->
       test_output := "Invalid standard output\n"::(!test_output);
       test_output := diff_output::(!test_output);
       test_result := false
  end;
  begin
    match compare_output config (test_name ^ ".stderr") stderr_filename with
    | None -> ()
    | Some diff_output ->
       test_output := "Invalid standard error\n"::(!test_output);
       test_output := diff_output::(!test_output);
       test_result := false
  end;
  if not !debug then begin
    Unix.unlink stdout_filename;
    Unix.unlink stderr_filename;
  end;
  !test_result, String.concat "" (List.rev !test_output)


let mk_test_case raw_test_name =
  let test_name =
    if Filename.check_suffix raw_test_name ".test"
    then Filename.chop_suffix raw_test_name ".test"
    else raw_test_name
  in
  let base_config = enrich_config (Filename.concat (Filename.dirname test_name) "config.otfrc") true init_config in
  test_name, enrich_config (test_name ^ ".test") false base_config

let collect_test_cases dirname =
  let base_config = enrich_config (Filename.concat dirname "config.otfrc") true init_config in
  let test_files = List.filter (fun fn -> Filename.check_suffix fn ".test") (Array.to_list (Sys.readdir dirname)) in
  let sorted_test_files = List.sort String.compare test_files in
  let mk_one_test_case fn =
    let test_name = Filename.concat dirname (Filename.chop_suffix fn ".test") in
    test_name, enrich_config (test_name ^ ".test") false base_config
  in
  List.map mk_one_test_case sorted_test_files

let print_prologue coll_name n_tests =
  print_endline (String.make 80 '=');
  print_endline ("Running collection \"" ^ (String.escaped coll_name) ^ "\"");
  print_endline ((string_of_int n_tests) ^ " tests collected");
  print_newline ()

let string_of_test_name name =
  let dashes_len = max 0 (72 - (String.length name)) in
  "------ " ^ name ^ " " ^ (String.make dashes_len '-')

let print_summary passed failed =
  print_endline (String.make 80 '=');
  print_endline ("Summary: " ^ (string_of_int passed) ^ " passed, " ^ (string_of_int failed) ^ " failed");
  print_endline (String.make 80 '=')


let _ =
  Arg.parse cli_arguments parse_filenames usage_msg;
  let test_cases = match !action, !test_names with
    | PrintConfig, [] -> ["", init_config]
    | PrintConfig, [test_name] -> [mk_test_case test_name]
    | PrintConfig, _::_::_ -> error "-print-config can only be called with one test"
    | RunTest, [] -> error "Missing test filename"
    | RunTest, names -> List.map mk_test_case names
    | RunCollection, [] -> error "Missing collection dirname"
    | RunCollection, [coll_name] ->
       let tc = collect_test_cases coll_name in
       print_prologue coll_name (List.length tc);
       tc
    | RunCollection, _::_::_ -> error "Running multiple collections is not supported"
  in

  match !action, test_cases with
  | PrintConfig, [_, config] ->
     begin
       print_config config;
       match check_config config with
       | [] -> print_endline "Config OK"
       | error_msgs ->
          print_endline (String.concat "\n  " ("Errors:"::error_msgs));
          exit 1
     end

  | RunTest, _::_ ->
     let handle_test (passed, failed) test_case =
       let res, output = run_test test_case in
       if not res || not !silent then begin
         print_endline (string_of_test_name (fst test_case));
         print_endline output;
       end;
       if res then (passed + 1, failed) else (passed, failed+1)
     in
     let (passed, failed) = List.fold_left handle_test (0, 0) test_cases in
     if failed != 0 || not !silent then print_summary passed failed;
     exit (if failed != 0 then 1 else 0)

  | RunCollection, _::_ ->
     let handle_test (passed, failed, cumulated_output) test_case =
       let test_name = fst test_case in
       print_string ((Filename.basename test_name) ^ " ");
       let res, output = run_test test_case in
       print_endline (if res then "." else "F");
       let new_passed, new_failed = if res then passed+1, failed else passed, failed+1
       and new_cumulated_output =
         if not res
         then output::(string_of_test_name test_name)::cumulated_output
         else cumulated_output
       in
       new_passed, new_failed, new_cumulated_output
     in
     let (passed, failed, output) = List.fold_left handle_test (0, 0, []) test_cases in
     print_newline ();
     if output != [] then begin
       print_endline (String.make 80 '=');
       print_endline (String.concat "\n" (List.rev output))
     end;
     print_summary passed failed;
     exit (if failed != 0 then 1 else 0)

  | PrintConfig, _
  | RunTest, []
  | RunCollection, [] -> error "Internal error"
