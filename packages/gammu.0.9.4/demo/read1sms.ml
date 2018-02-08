(* Simple demo/test file that prints some informations and the first sms. *)

open Args_demo
open Utils_tests
open Printf

let () =
  try
    let s = Gammu.make () in
    prepare_phone s;
    while true do
      print_string "Enter folder: ";
      let folder = int_of_string (read_line ()) in
      print_string "Enter message number: ";
      let message_number = int_of_string (read_line ()) in
      let multi_sms = Gammu.SMS.get s ~folder ~message_number in
      print_multi_sms multi_sms;
    done
  with G.Error e -> printf "Error: %s\n" (G.string_of_error e)

