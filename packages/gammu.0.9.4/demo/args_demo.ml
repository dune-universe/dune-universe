open Printf

let parse_args () =
  let gammurc = ref None
  and section = ref 0 in
  let args = [
    ("--gammurc", Arg.String (fun s -> gammurc := Some s),
     "<file> Force gammurc file path (no autodetection).");
    ("--section", Arg.Set_int section,
     "<integer> Section number from gammurc to load.");
  ] in
  let anon _ = raise (Arg.Bad "No anonymous arguments.") in
  Arg.parse (Arg.align args) anon (sprintf "Usage: %s [options]" Sys.argv.(0));
  (!gammurc, !section)

let configure s =
  let path, section = parse_args () in
  (* Not ideal but quick workaround to effectively use debug configuration
     from gammurc. *)
  let ini = Gammu.INI.of_gammurc ?path () in
  let cfg = Gammu.INI.config ini section in
  Gammu.push_config s { cfg with Gammu.use_global_debug_file = false }

(* Connect and unlock phone. *)
let prepare_phone s =
  configure s;
  (* Unlock the phone asking user for codes. *)
  let rec ask_user_code s code_type code_type_name =
    printf "\n  Enter %s code: %!" code_type_name;
    let code = read_line () in
    try
      Gammu.enter_security_code s ~code_type ~code;
      printf "  Code entered.\n%!";
      Unix.sleep 1; (* required, if one tries to access the card too fast,
                       NOSIM is raised. *)
      (* Check if there's another security code to enter. *)
      unlock_phone s;
    with Gammu.Error Gammu.SECURITYERROR ->
      printf "  Wrong code, retry.%!";
      ask_user_code s code_type code_type_name;
  and unlock_phone s =
    let sec_status =
      try Gammu.get_security_status s
      with Gammu.Error Gammu.UNKNOWNRESPONSE -> Gammu.SEC_None
    in
    match sec_status with
    | Gammu.SEC_None -> print_endline "done."
    | Gammu.SEC_SecurityCode -> ask_user_code s sec_status "Security"
    | Gammu.SEC_Pin -> ask_user_code s sec_status "PIN"
    | Gammu.SEC_Pin2 -> ask_user_code s sec_status "PIN2"
    | Gammu.SEC_Puk -> ask_user_code s sec_status "PUK"
    | Gammu.SEC_Puk2 -> ask_user_code s sec_status "PUK2"
    | Gammu.SEC_Phone -> ask_user_code s sec_status "Phone"
    | Gammu.SEC_Network -> ask_user_code s sec_status "Network"
  in
  printf "Trying to connect... %!";
  Gammu.connect s;
  printf "done.\n%!";
  printf "Phone model : %S\n" (Gammu.Info.model s);
  printf "Unlock SIM/Phone... %!";
  unlock_phone s

