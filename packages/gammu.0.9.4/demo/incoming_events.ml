(* Simple demo/test file that listen for incoming events. *)
open Printf
open Args_demo
open Utils_tests

module G = Gammu

let () =
  try
    let s = G.make () in
    prepare_phone s;
    let incoming_sms_callback sms =
      print_string "\nIncoming SMS !\n";
      printf "From number: %s\n" sms.G.SMS.number;
    and incoming_call_callback call =
      print_string "\nIncoming Call !\n";
      printf "From number: %s\n" call.G.Call.number;
    in
    (* Check for (maybe) non supported operation. Unfortunately, libGammu
       doesn't recognize the CMS error for those function right now. CMS Error
       303 "operation not supported" is translated to Error UNKNOWN. *)
    let try_set_incoming incoming_itype callback itype =
      try incoming_itype s callback;
      with G.Error G.UNKNOWN | G.Error G.NOTSUPPORTED ->
        printf "Sorry, incoming_%s notifications aren't supported by \
	  your phone.%!" itype;
    in
    try_set_incoming (fun s m -> G.incoming_sms s m)
      incoming_sms_callback "sms";
    try_set_incoming (fun s m -> G.incoming_call s m)
      incoming_call_callback "call";
    print_newline ();
    (* Busy waiting to keep communication with phone *)
    while true do
      printf "\r%s%!" (string_of_signal_quality (G.Info.signal_quality s));
      Unix.sleep 1;
    done
  with G.Error e -> printf "Error: %s\n" (G.string_of_error e)
(* TODO: Add a trap to disconnect... *)

