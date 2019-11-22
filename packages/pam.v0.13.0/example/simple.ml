open! Core
open Pam

let prompt_input msg ~echo =
  let term_set attr = Unix.Terminal_io.tcsetattr attr Unix.stdin ~mode:TCSANOW in
  let term_attr = Unix.Terminal_io.tcgetattr Unix.stdin in
  let old_echo = term_attr.c_echo in
  printf "%s%!" msg;
  term_attr.c_echo <- echo;
  term_set term_attr;
  let input = In_channel.input_line In_channel.stdin in
  printf "\n";
  term_attr.c_echo <- old_echo;
  term_set term_attr;
  input
;;

let conv msgs =
  let responses =
    List.map msgs ~f:(fun r ->
      match r with
      | Error msg ->
        eprintf "Error: %s\n" msg;
        Pam_conv.Response.create ~resp:None
      | Ok m ->
        let msg = Pam_conv.Message.message m in
        (match Pam_conv.Message.style m with
         | PAM_PROMPT_ECHO_OFF ->
           let resp = prompt_input msg ~echo:false in
           Pam_conv.Response.create ~resp
         | PAM_PROMPT_ECHO_ON ->
           let resp = prompt_input msg ~echo:true in
           Pam_conv.Response.create ~resp
         | PAM_ERROR_MSG ->
           eprintf "%s%!" msg;
           Pam_conv.Response.create ~resp:None
         | PAM_TEXT_INFO ->
           printf "%s%!" msg;
           Pam_conv.Response.create ~resp:None))
  in
  In_channel.close In_channel.stdin;
  Ok responses
;;

let set_pam_service_item ~ctx item =
  let item_type = Pam_item_type.PAM_SERVICE in
  Pam.pam_get_item ctx ~item_type
  |> Or_error.map ~f:(fun s ->
    printf "Before update, PAM_SERVICE=%s\n" (Option.value_exn s))
  |> Or_error.bind ~f:(fun () -> Pam.pam_set_item ctx ~item_type ~item)
  |> Or_error.bind ~f:(fun () -> Pam.pam_get_item ctx ~item_type)
  |> Or_error.map ~f:(fun s ->
    printf "After update, PAM_SERVICE=%s\n" (Option.value_exn s))
;;

let main ~service ~user () =
  match Pam.pam_start ~service ~user ~conv with
  | Error e -> eprintf !"%{Error#hum}\n" e
  | Ok c ->
    Pam.pam_authenticate c ~flags:[]
    |> Or_error.bind ~f:(fun () -> Pam.pam_acct_mgmt c ~flags:[])
    |> Or_error.bind ~f:(fun () -> Pam.pam_setcred c ~flag:PAM_ESTABLISH_CRED)
    |> Or_error.bind ~f:(fun () -> Pam.pam_open_session c ~flags:[])
    |> Or_error.bind ~f:(fun () -> set_pam_service_item ~ctx:c ("temp-" ^ service))
    |> Or_error.bind ~f:(fun () -> set_pam_service_item ~ctx:c service)
    |> Or_error.bind ~f:(fun () -> Pam.pam_close_session c ~flags:[])
    |> Or_error.bind ~f:(fun () -> Pam.pam_end c)
    |> (function
      | Error e -> eprintf !"%{Error#hum}\n" e
      | Ok () -> printf "pam session succeeds\n")
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"simple example"
    [%map_open
      let user = flag "user" (required string) ~doc:"username"
      and service = flag "service" (required string) ~doc:"pam service name" in
      main ~service ~user]
;;

let () = Command.run command
