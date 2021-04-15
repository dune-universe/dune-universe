let std_open_in_bin fname = if fname = "-" then stdin else open_in_bin fname;;

let std_open_out_bin fname =
  if fname = "-" then stdout else open_out_bin fname;;
let std_close_in ic = if ic != stdin then close_in ic;;
let std_close_out oc = if oc == stdout then flush oc else close_out oc;;
let std_add_extension fname ext =
  if Filename.check_suffix fname ext then fname else
  if fname = "" then "a" ^ ext else
  if fname = "-" then fname else fname ^ ext
;;

let apg_file_name fname = std_add_extension fname Config.apg_file_extension;;

let not_an_apg_file fname =
  failwith
    (Printf.sprintf
      "input_apg: %s is not an Active-DVI portable graphic file." fname)
;;

let magic_number = Config.apg_magic_number;;
let output_magic_number oc = output_string oc magic_number;;

let check_magic_number =
  let mn = String.copy magic_number in
  let len = String.length magic_number in
  (fun fname ic ->
     String.fill mn 0 len ' ';
     begin
       try really_input ic mn 0 len with
       | End_of_file -> not_an_apg_file fname
     end;
     if mn <> magic_number then not_an_apg_file fname)
;;

let output_apg, input_apg =
  (fun oc (prog : Apg_types.program) ->
     output_magic_number oc;
     Marshal.to_channel oc prog [Marshal.No_sharing]),
  (fun fname ic ->
     check_magic_number fname ic;
     (Marshal.from_channel ic : Apg_types.program))
;;

let save_apg fname prog =
  let fname = apg_file_name fname in
  let oc = std_open_out_bin fname in
  output_apg oc prog;
  std_close_out oc
;;

let load_apg fname =
  let ic = std_open_in_bin fname in
  let prog = input_apg fname ic in
  std_close_in ic;
  prog
;;
