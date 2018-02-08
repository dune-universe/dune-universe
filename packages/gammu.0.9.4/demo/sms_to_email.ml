(* Sample program to be run as (say) a cron job to send an email for
   each SMS.  Use the unix command "mail" to send emails (see the
   [mailto] function) to change that. *)

open Printf
open Scanf
module G = Gammu
module SMS = G.SMS

type config = { gammurc: string;
                pin : string;  (* "" for none *)
                folder : int;
                email : string;
              }

let config_file =
  ref(try Filename.concat (Sys.getenv "HOME") ".config/sms"
      with _ -> ".sms")

(* Global config. variable (for ease of use). *)
let config = ref { gammurc = "";
                   pin = "";
                   folder = 0;
                   email = "" }

let spec = [
  ("--gammurc", Arg.String(fun rc -> config := { !config with gammurc = rc}),
   "<file> Force gammurc file path (override autodetection).");
  ("--folder", Arg.Int(fun i -> config := { !config with folder = i }),
   sprintf "<number> folder to check for SMS (default: %i)" !config.folder);
  ("--config", Arg.Set_string config_file,
   sprintf "<file> file used for configuration (default: %s)" !config_file);
]

let parse_config () =
  try
    let fh = Scanning.open_in !config_file in
    while not(Scanning.end_of_input fh) do
      try bscanf fh "pin = %S " (fun p -> config := { !config with pin = p})
      with Scan_failure _ ->
        bscanf fh "%_s@\n" (); (* skip line *)
    done;
    Scanning.close_in fh
  with Sys_error _ -> ()


(* Simple function to send an email. *)
let mailto ?(date="") ~subject msg =
  let msg_file, fh = Filename.open_temp_file "sms" ".txt" in
  output_string fh msg;
  output_string fh "\n";
  close_out fh;
  let mail = sprintf "mail -a \"From: SMS <SMS@localhost>\" \
                      -s %S %S" subject !config.email in
  let mail = if date = "" then mail
             else sprintf "%s -a \"Date: %s\"" mail date in
  ignore(Sys.command (mail ^ " < " ^ msg_file));
  Sys.remove msg_file

let mail_error ?(subject="ERROR") err =
  mailto ~subject:("SMS to email: " ^ subject) err

let get_security_status s =
  try Gammu.get_security_status s
  with Gammu.Error Gammu.UNKNOWNRESPONSE -> Gammu.SEC_None

let check_sec_status_and_do s f =
  (* If a security code must be entered, send an email to the user. *)
  let email pin =
    mail_error ~subject:pin
               (sprintf "The GSM card requires that you enter your %s." pin) in
  match get_security_status s with
  | Gammu.SEC_None -> f()
  | Gammu.SEC_SecurityCode -> email "Security code"
  | Gammu.SEC_Pin ->
     if !config.pin = "" then email "PIN"
     else (
       Gammu.enter_security_code s ~code_type:Gammu.SEC_Pin ~code:!config.pin;
       Unix.sleep 2;
       match get_security_status s with
       | Gammu.SEC_None -> ()
       | _ ->
          let msg = sprintf "sms_to_email entered the PIN %s (found in %s) but \
                             the card does not acknowledge it.  Check the PIN \
                             is correct or your card will be blocked."
                            !config.pin !config_file in
          mail_error ~subject:"WRONG PIN" msg
     )
  | Gammu.SEC_Pin2 -> email "PIN2"
  | Gammu.SEC_Puk -> email "PUK"
  | Gammu.SEC_Puk2 -> email "PUK2"
  | Gammu.SEC_Phone -> email "Phone code"
  | Gammu.SEC_Network -> email "Network code"

(* Processed (linked) SMS, ready to send by email. *)
type message = {
  date: G.DateTime.t;
  from: string;  (** number the SMS was sent from. *)
  text: string;  (** body (i.e. content) of the message. *)
}

let message_of_sms multi_sms =
  let sms = multi_sms.(0) in
  let date = sms.SMS.date_time
  and from = sms.SMS.number in
  if sms.SMS.udh_header.SMS.udh = SMS.No_udh then
    (* There's no udh so text is raw in the sms message. *)
    { date; from; text = sms.SMS.text }
  else
    (* There's an udh so we have to decode the sms. *)
    let info = (SMS.decode_multipart multi_sms).SMS.entries in
    let texts = Array.map (fun i -> i.SMS.buffer) info in
    { date; from; text =  String.concat "" (Array.to_list texts) }


let concat_sms sms =
  assert(sms <> []);
  let cmp_sms s1 s2 =
    let open SMS in
    compare s1.(0).udh_header.part_number s2.(0).udh_header.part_number in
  let sms = List.sort cmp_sms sms in
  let msg = List.map message_of_sms sms in
  let msg1 = List.hd msg in
  { date = msg1.date;  from = msg1.from;
    text = String.concat "" (List.map (fun m -> m.text) msg) }

(* Put together the messages that were splitted into different SMS. *)
let rec link_sms all_sms =
  match all_sms with
  | [] -> []
  | sms :: tl ->
     (* Use the presence of an ID to group messages. *)
     let open SMS in
     let hd = sms.(0).udh_header in
     if hd.udh = ConcatenatedMessages then
       let same_id s = s.(0).udh_header.id8bit = hd.id8bit in
       let msg_sms, other = List.partition same_id all_sms in
       concat_sms msg_sms :: link_sms other
     else if hd.udh = ConcatenatedMessages16bit then
       let same_id s = s.(0).udh_header.id16bit = hd.id16bit in
       let msg_sms, other = List.partition same_id all_sms in
       concat_sms msg_sms :: link_sms other
     else
       message_of_sms sms :: link_sms tl

let email_messages msgs =
  List.iter (fun msg ->
             let date = G.DateTime.os_date_time msg.date in
             mailto ~subject:(sprintf "SMS from %s on %s" msg.from date)
                    msg.text
            ) msgs


let read_all_sms s folder =
  let on_err loc e =
    mail_error (sprintf "Failed to get SMS next to location %i: %s\n%!"
                        loc (Gammu.string_of_error e));
    Unix.sleep 1 in
  try
    let all_sms = G.SMS.fold s ~folder ~on_err (fun l m -> m :: l) [] in
    (* Filter out SMS that are already read. *)
    let all_sms = List.filter (fun s -> s.(0).SMS.state = SMS.Unread) all_sms in
    email_messages (link_sms all_sms)
  with
  | G.Error G.NOTSUPPORTED ->
     mail_error "Sorry but your phone doesn't support GetNext."
  | G.Error G.NOTIMPLEMENTED ->
     mail_error "Sorry but GetNext is not implemented."


let () =
  let usage_msg = sprintf "Usage: %s [options] <to email>" Sys.argv.(0) in
  let anon s = config := { !config with email = s } in
  Arg.parse spec anon usage_msg;
  parse_config ();
  if !config.email = "" then (Arg.usage spec usage_msg; exit 1);
  try
    let s = Gammu.make () in
    Gammu.connect s;
    check_sec_status_and_do s (fun () -> read_all_sms s !config.folder)
  with Gammu.Error e -> printf "Error: %s\n" (Gammu.string_of_error e)

