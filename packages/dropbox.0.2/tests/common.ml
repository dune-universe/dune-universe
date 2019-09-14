open Printf
open Lwt
module D = Dropbox_lwt_unix

(** Execute the function [f], passing the standard command line
    arguments to it. *)
let run ?(args=[]) f =
  let id = ref "" in
  let secret = ref "" in
  let token = ref "" in
  let specs = [
      "--id", Arg.Set_string id,
      " The client ID (found in the Dropbox App Console)";
      "--secret", Arg.Set_string secret,
      " The client secret (found in the Dropbox App Console)";
      "--token", Arg.Set_string token,
      " The generated access token (found in the Dropbox App Console)";
    ] in
  let specs = Arg.align (args @ specs) in
  let anon_args = ref [] in
  let anon s = anon_args := s :: !anon_args in
  let usage_msg = Filename.basename Sys.argv.(0) ^ " [options]" in
  Arg.parse specs anon usage_msg;
  let anon_args = List.rev !anon_args in
  let main =
    if !token = "" then (
      if !id = "" then (
        eprintf "Set --id with the App key found at \
                 https://www.dropbox.com/developers/apps\n";
        exit 1
      );
      if !secret = "" then (
        eprintf "Set --secret with the App secret found at \
                 https://www.dropbox.com/developers/apps\n";
        exit 1
      );
      let u = D.OAuth2.authorize ~id:!id (`Code None) in
      printf "1. Go to: %s\n" (Uri.to_string u);
      printf "2. Click \"Allow\" (you might have to log in first).\n";
      printf "3. Copy the authorization code:\n";
      let code = String.trim(read_line()) in
      D.OAuth2.token code ~id:!id ~secret:!secret >>= fun token ->
      f (D.session token) anon_args
    )
    else
      f (D.session !token) anon_args in
  try  Lwt_main.run main
  with Dropbox.Error e ->
    eprintf "Error: %s\n" (Dropbox.string_of_error e)
