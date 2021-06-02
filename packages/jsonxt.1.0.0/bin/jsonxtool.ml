open Printf

let die msg = 
  printf "ERROR: %s\n" msg;
  exit 255

let help_error err = 
  match err with
  | Some err -> printf "ERROR: %s\n\n" err
  | None -> ()

let help_msg msg err =
  help_error err;
  printf "%s" msg;
  exit 0

let load_file f =
  let ic = if String.equal f "-" then stdin else open_in f in
  let buf = Buffer.create 100 in
  let rec loop () =
    try begin Buffer.add_channel buf ic 4096; loop () end with
    | End_of_file -> ()
  in
  loop ();
  close_in ic;
  Buffer.contents buf

let command_dump idx sys_argv_len =
  let usage = "\
   jsonxtool dump [-help|-h] [-compliance [strict|basic|extended|yjbasic|yjsafe]] file

   Load the specified file or stdin of file is minus (-) and dump out in internal tree format.
   The compliance level can be selected, defaulting to extended.\n"
  in
  let rdstdin = ref false in
  let file = ref "" in
  let compliance = ref `Extended in
  let set_compliance = function
    | "strict" -> compliance := `Strict
    | "basic" -> compliance := `Basic
    | "extended" -> compliance := `Extended
    | "yjbasic" -> compliance := `Yojson_basic
    | "yjsafe" -> compliance := `Yojson_safe
    | level -> die ("unknown compliance level: " ^ level)
  in
  let anon arg =
    if String.equal !file "" && not !rdstdin then file := arg
    else die "only one file maybe dumped"
  in
  let set_rdstdin () =
    if String.equal !file "" && not !rdstdin then rdstdin := true
    else die "only one file maybe dumped"
  in
  let argspec = [
      ("-compliance",
       Arg.Symbol (["strict"; "basic"; "extended"; "yjbasic"; "yjsafe"], set_compliance),
       "compliance level")
    ; ("-", Unit set_rdstdin, "read from stdin")
    ]
  in
  let current = ref (idx - 1) in
  if sys_argv_len < 3 then help_msg usage (Some "dump expected at least one option");
  begin
    try Arg.parse_argv ~current Sys.argv argspec anon usage with
    | Arg.Bad err -> die err
    | Arg.Help msg -> help_msg msg None
  end;
  if !rdstdin then file := "-";
  if String.equal !file "" then die "expected file to dump";
  let contents = load_file !file in
  let dump =
    try begin
      match !compliance with
      | `Strict -> let json = Jsonxt.Strict.of_string contents in Jsonxt.Utilities.json_to_string_repr json
      | `Basic -> let json = Jsonxt.Basic.of_string contents in Jsonxt.Utilities.json_to_string_repr json
      | `Extended -> let json = Jsonxt.Extended.of_string contents in Jsonxt.Utilities.json_to_string_repr json
      | `Yojson_basic -> let json = Jsonxt.Yojson.Basic.from_string contents in Jsonxt.Utilities.json_to_string_repr json
      | `Yojson_safe -> let json = Jsonxt.Yojson.Safe.from_string contents in Jsonxt.Utilities.json_to_string_repr json
    end
    with
    | Failure err -> sprintf "Parse failed with: %s" err
  in
  printf "%s\n" dump

let () =
  let usage = "jsonxtool [help|dump]\n" in
  let sys_argv_len = Array.length Sys.argv in
  if sys_argv_len < 2 then help_msg usage (Some "expected command");
  match Sys.argv.(1) with
  | "help" -> help_msg usage None
  | "dump" -> command_dump 2 sys_argv_len
  | _ -> help_msg usage (Some ("unknown command " ^ Sys.argv.(1)))
