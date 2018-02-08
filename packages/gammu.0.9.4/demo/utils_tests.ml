open Printf

module G = Gammu
module SMS = Gammu.SMS
module Info = Gammu.Info

let string_of_sms_status =
  function
  | SMS.Sent -> "Sent"
  | SMS.Unsent -> "Unsent"
  | SMS.Read -> "Read"
  | SMS.Unread -> "Unread"

let print_multi_sms multi_sms =
  let sms = multi_sms.(0) in
  printf "Folder: %d, " sms.SMS.folder;
  printf "Message number: %d " sms.SMS.message_number;
  printf "(%sin inbox)\n" (if sms.SMS.inbox_folder then "" else "not ");
  printf "Number: %s\n" sms.SMS.number;
  printf "Date and time: %s\n"
    (G.DateTime.os_date_time sms.SMS.date_time);
  printf "Date and time (SMSC): %s\n"
    (G.DateTime.os_date_time sms.SMS.smsc_time);
  printf "Status : %s\n" (string_of_sms_status sms.SMS.state);
  if sms.SMS.udh_header.SMS.udh = SMS.No_udh then
    (* There's no udh so text is raw in the sms message. *)
    printf "%s" sms.SMS.text
  else (
    SMS.(let hd = sms.udh_header in
         match hd.udh with
         | ConcatenatedMessages ->
            printf "UDH: linked SMS (%i/%i), id: %i\n"
                   hd.part_number hd.all_parts hd.id8bit
         | ConcatenatedMessages16bit ->
            printf "UDH: linked SMS (%i/%i), id 16 bits: %i\n"
                   hd.part_number hd.all_parts hd.id16bit
         | _  -> ());
    (* There's an udh so we have to decode the sms. *)
    let multi_info = SMS.decode_multipart multi_sms
    and print_info info = print_string info.SMS.buffer in
    Array.iter print_info multi_info.SMS.entries
  );
  printf "\n\n%!"

let string_of_signal_quality signal =
  sprintf "Signal Strength = %d, %d%%, bit error rate = %d%%"
    signal.Info.signal_strength
    signal.Info.signal_percent
    signal.Info.bit_error_rate

let string_of_info_gprs = function
  | Info.Detached -> "Detached"
  | Info.Attached -> "Attached"
  | Info.Unknown_gprs -> "Unknown"

let string_of_network_state = function
  | Info.HomeNetwork -> "Home"
  | Info.NoNetwork -> "No Network"
  | Info.RoamingNetwork -> "Roaming"
  | Info.RegistrationDenied -> "Registration Denied"
  | Info.Unknown_network -> "Unknown status"
  | Info.RequestingNetwork -> "Requesting"

let string_of_memory_type = function
  | G.ME -> "Internal memory of the mobile equipment"
  | G.SM -> "SIM card memory"
  | G.ON -> "Own numbers"
  | G.DC -> "Dialled calls"
  | G.RC -> "Received calls"
  | G.MC -> "Missed calls"
  | G.MT -> "Combined ME and SIM phonebook"
  | G.FD -> "Fixed dial"
  | G.VM -> "Voice mailbox"
  | G.SL -> "Sent SMS logs"
  | G.QD -> "Quick dialing choices"

let string_of_folder folder =
  let folder_box =
    match folder.SMS.box with
    | SMS.Inbox -> "Inbox"
    | SMS.Outbox -> "Outbox"
  and folder_memory = string_of_memory_type folder.SMS.folder_memory in
  sprintf "%s (%s) in %s" folder.SMS.name folder_box folder_memory
