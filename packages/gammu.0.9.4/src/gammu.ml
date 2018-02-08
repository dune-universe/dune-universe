(* File: gammu.ml

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Noémie Meunier <Noemie.Meunier@student.umons.ac.be>
     Pierre Hauweele <Pierre.Hauweele@student.umons.ac.be>

     WWW: http://math.umons.ac.be/an/software/

   This library is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details. *)

(* Initialize the C library *)
external c_init : unit -> unit = "caml_gammu_init"
let () = c_init ()


(************************************************************************)
(* Error handling *)

(* WARNING: Keep the order in sync with gammu-error.h *)
type error =
  (* ERR_NONE = 1, not useful here (not an error) *)
  | DEVICEOPENERROR     (** Error during opening device *)
  | DEVICELOCKED        (** Device locked *)
  | DEVICENOTEXIST      (** Device does not exits *)
  | DEVICEBUSY          (** Device is busy *)
  | DEVICENOPERMISSION  (** No permissions to open device *)
  | DEVICENODRIVER      (** No driver installed for a device *)
  | DEVICENOTWORK       (** Device doesn't seem to be working *)
  | DEVICEDTRRTSERROR   (** Error during setting DTR/RTS in device *)
  (* 10 (number from gammu-error.h) *)
  | DEVICECHANGESPEEDERROR (** Error during changing speed in device *)
  | DEVICEWRITEERROR    (** Error during writing device *)
  | DEVICEREADERROR     (** Error during reading device *)
  | DEVICEPARITYERROR   (** Can't set parity on device *)
  | TIMEOUT             (** Command timed out *)
  | FRAMENOTREQUESTED   (** Frame handled, but not requested in this moment *)
  | UNKNOWNRESPONSE     (** Response not handled by gammu *)
  | UNKNOWNFRAME        (** Frame not handled by gammu *)
  | UNKNOWNCONNECTIONTYPESTRING (** Unknown connection type given by user *)
  | UNKNOWNMODELSTRING  (** Unknown model given by user *)
  (* 20 *)
  | SOURCENOTAVAILABLE  (** Some functions not compiled in your OS *)
  | NOTSUPPORTED        (** Not supported by phone *)
  | EMPTY               (** Empty entry or transfer end. *)
  | SECURITYERROR       (** Not allowed *)
  | INVALIDLOCATION     (** Too high or too low location... *)
  | NOTIMPLEMENTED      (** Function not implemented *)
  | FULL                (** Memory is full *)
  | UNKNOWN             (** Unknown response from phone *)
  | CANTOPENFILE        (** Error during opening file *)
  | MOREMEMORY          (** More memory required *)
  (* 30 *)
  | PERMISSION          (** No permission *)
  | EMPTYSMSC           (** SMSC number is empty *)
  | INSIDEPHONEMENU     (** Inside phone menu - can't make something *)
  | NOTCONNECTED        (** Phone NOT connected - can't make something *)
  | WORKINPROGRESS      (** Work in progress *)
  | PHONEOFF            (** Phone is disabled and connected to charger *)
  | FILENOTSUPPORTED    (** File format not supported by Gammu *)
  | BUG                 (** Found bug in implementation or phone *)
  | CANCELED            (** Action was canceled by user *)
  | NEEDANOTHERANSWER   (** Inside Gammu: phone module need to send
                            another answer frame *)
  (* 40 *)
  | OTHERCONNECTIONREQUIRED (** You need other connection for
                                this operation. *)
  | WRONGCRC            (** Wrong CRC *)
  | INVALIDDATETIME     (** Invalid date/time *)
  | MEMORY              (** Phone memory error, maybe it is read only *)
  | INVALIDDATA         (** Invalid data given to phone *)
  | FILEALREADYEXIST    (** File with specified name already exist *)
  | FILENOTEXIST        (** File with specified name doesn't exist *)
  | SHOULDBEFOLDER      (** You have to give folder (not file) name *)
  | SHOULDBEFILE        (** You have to give file (not folder) name *)
  | NOSIM               (** Cannot access SIM card *)
  (* 50 *)
  | GNAPPLETWRONG       (** Invalid gnapplet version *)
  | FOLDERPART          (** Only part of folders listed *)
  | FOLDERNOTEMPTY      (** Folder is not empty *)
  | DATACONVERTED       (** Data were converted *)
  | UNCONFIGURED        (** Gammu is not configured. *)
  | WRONGFOLDER         (** Wrong folder selected (eg. for SMS). *)
  | PHONE_INTERNAL      (** Internal phone error (phone got crazy). *)
  | WRITING_FILE        (** Could not write to a file (on local filesystem). *)
  | NONE_SECTION        (** No such section exists. *)
  | USING_DEFAULTS      (** Using default values. *)
  (* 60 *)
  | CORRUPTED           (** Corrupted data returned by phone. *)
  | BADFEATURE          (** Bad feature string. *)
  | DISABLED            (** Some functions not compiled in your OS *)
  | SPECIFYCHANNEL      (** Bluetooth configuration requires channel option. *)
  | NOTRUNNING          (** Service is not running. *)
  | NOSERVICE           (** Service setup is missing. *)
  | BUSY                (** Command failed. Try again. *)
  | COULDNT_CONNECT     (** Cannot connect to server. *)
  | COULDNT_RESOLVE     (** Cannot resolve host name. *)
  | GETTING_SMSC        (** Failed to get SMSC number from phone. *)
  (* 70 *)
  | ABORTED
  | INSTALL_NOT_FOUND
  | READ_ONLY
  | NETWORK_ERROR
  (* Errors specific to the OCaml bindings (not present in Gammu): *)
  | INI_KEY_NOT_FOUND   (** Pair section/value not found in INI file. *)
  | COULD_NOT_DECODE    (** Decoding SMS Message failed. *)
  | INVALID_CONFIG_NUM  (** Invalid config number. *)

external string_of_error : error -> string = "caml_gammu_GSM_ErrorString"

exception Error of error

let string_for_uncaught_Error = function
  | DEVICEOPENERROR     -> "Gammu.Error(DEVICEOPENERROR)"
  | DEVICELOCKED        -> "Gammu.Error(DEVICELOCKED)"
  | DEVICENOTEXIST      -> "Gammu.Error(DEVICENOTEXIST)"
  | DEVICEBUSY          -> "Gammu.Error(DEVICEBUSY)"
  | DEVICENOPERMISSION  -> "Gammu.Error(DEVICENOPERMISSION)"
  | DEVICENODRIVER      -> "Gammu.Error(DEVICENODRIVER)"
  | DEVICENOTWORK       -> "Gammu.Error(DEVICENOTWORK)"
  | DEVICEDTRRTSERROR   -> "Gammu.Error(DEVICEDTRRTSERROR)"
  | DEVICECHANGESPEEDERROR -> "Gammu.Error(DEVICECHANGESPEEDERROR)"
  | DEVICEWRITEERROR    -> "Gammu.Error(DEVICEWRITEERROR)"
  | DEVICEREADERROR     -> "Gammu.Error(DEVICEREADERROR)"
  | DEVICEPARITYERROR   -> "Gammu.Error(DEVICEPARITYERROR)"
  | TIMEOUT             -> "Gammu.Error(TIMEOUT)"
  | FRAMENOTREQUESTED   -> "Gammu.Error(FRAMENOTREQUESTED)"
  | UNKNOWNRESPONSE     -> "Gammu.Error(UNKNOWNRESPONSE)"
  | UNKNOWNFRAME        -> "Gammu.Error(UNKNOWNFRAME)"
  | UNKNOWNCONNECTIONTYPESTRING-> "Gammu.Error(UNKNOWNCONNECTIONTYPESTRING)"
  | UNKNOWNMODELSTRING  -> "Gammu.Error(UNKNOWNMODELSTRING)"
  | SOURCENOTAVAILABLE  -> "Gammu.Error(SOURCENOTAVAILABLE)"
  | NOTSUPPORTED        -> "Gammu.Error(NOTSUPPORTED)"
  | EMPTY               -> "Gammu.Error(EMPTY)"
  | SECURITYERROR       -> "Gammu.Error(SECURITYERROR)"
  | INVALIDLOCATION     -> "Gammu.Error(INVALIDLOCATION)"
  | NOTIMPLEMENTED      -> "Gammu.Error(NOTIMPLEMENTED)"
  | FULL                -> "Gammu.Error(FULL)"
  | UNKNOWN             -> "Gammu.Error(UNKNOWN)"
  | CANTOPENFILE        -> "Gammu.Error(CANTOPENFILE)"
  | MOREMEMORY          -> "Gammu.Error(MOREMEMORY)"
  | PERMISSION          -> "Gammu.Error(PERMISSION)"
  | EMPTYSMSC           -> "Gammu.Error(EMPTYSMSC)"
  | INSIDEPHONEMENU     -> "Gammu.Error(INSIDEPHONEMENU)"
  | NOTCONNECTED        -> "Gammu.Error(NOTCONNECTED)"
  | WORKINPROGRESS      -> "Gammu.Error(WORKINPROGRESS)"
  | PHONEOFF            -> "Gammu.Error(PHONEOFF)"
  | FILENOTSUPPORTED    -> "Gammu.Error(FILENOTSUPPORTED)"
  | BUG                 -> "Gammu.Error(BUG)"
  | CANCELED            -> "Gammu.Error(CANCELED)"
  | NEEDANOTHERANSWER   -> "Gammu.Error()"
  | OTHERCONNECTIONREQUIRED-> "Gammu.Error(NEEDANOTHERANSWER)"
  | WRONGCRC            -> "Gammu.Error(WRONGCRC)"
  | INVALIDDATETIME     -> "Gammu.Error(INVALIDDATETIME)"
  | MEMORY              -> "Gammu.Error(MEMORY)"
  | INVALIDDATA         -> "Gammu.Error(INVALIDDATA)"
  | FILEALREADYEXIST    -> "Gammu.Error(FILEALREADYEXIST)"
  | FILENOTEXIST        -> "Gammu.Error(FILENOTEXIST)"
  | SHOULDBEFOLDER      -> "Gammu.Error(SHOULDBEFOLDER)"
  | SHOULDBEFILE        -> "Gammu.Error(SHOULDBEFILE)"
  | NOSIM               -> "Gammu.Error(NOSIM)"
  | GNAPPLETWRONG       -> "Gammu.Error(GNAPPLETWRONG)"
  | FOLDERPART          -> "Gammu.Error(FOLDERPART)"
  | FOLDERNOTEMPTY      -> "Gammu.Error(FOLDERNOTEMPTY)"
  | DATACONVERTED       -> "Gammu.Error(DATACONVERTED)"
  | UNCONFIGURED        -> "Gammu.Error(UNCONFIGURED)"
  | WRONGFOLDER         -> "Gammu.Error(WRONGFOLDER)"
  | PHONE_INTERNAL      -> "Gammu.Error(PHONE_INTERNAL)"
  | WRITING_FILE        -> "Gammu.Error(WRITING_FILE)"
  | NONE_SECTION        -> "Gammu.Error(NONE_SECTION)"
  | USING_DEFAULTS      -> "Gammu.Error(USING_DEFAULTS)"
  | CORRUPTED           -> "Gammu.Error(CORRUPTED)"
  | BADFEATURE          -> "Gammu.Error(BADFEATURE)"
  | DISABLED            -> "Gammu.Error(DISABLED)"
  | SPECIFYCHANNEL      -> "Gammu.Error(SPECIFYCHANNEL)"
  | NOTRUNNING          -> "Gammu.Error(NOTRUNNING)"
  | NOSERVICE           -> "Gammu.Error(NOSERVICE)"
  | BUSY                -> "Gammu.Error(BUSY)"
  | COULDNT_CONNECT     -> "Gammu.Error(COULDNT_CONNECT)"
  | COULDNT_RESOLVE     -> "Gammu.Error(COULDNT_RESOLVE)"
  | GETTING_SMSC        -> "Gammu.Error(GETTING_SMSC)"
  | ABORTED             -> "Gammu.Error(ABORTED)"
  | INSTALL_NOT_FOUND   -> "Gammu.Error(INSTALL_NOT_FOUND)"
  | READ_ONLY           -> "Gammu.Error(READ_ONLY)"
  | NETWORK_ERROR       -> "Gammu.Error(NETWORK_ERROR)"
  | INI_KEY_NOT_FOUND   -> "Gammu.Error(INI_KEY_NOT_FOUND)"
  | COULD_NOT_DECODE    -> "Gammu.Error(COULD_NOT_DECODE)"
  | INVALID_CONFIG_NUM  -> "Gammu.Error(INVALID_CONFIG_NUM)"

let () =
  Callback.register_exception "Gammu.GSM_Error" (Error DEVICEOPENERROR);
  (* Register a custom exception printer to have the error displayed
     symbolically (not as an integer).
     CANVAS: http://caml.inria.fr/mantis/view.php?id=5040 *)
  let printer = function
    | Error e -> Some(string_for_uncaught_Error e)
    | _ -> None in
  Printexc.register_printer printer


(************************************************************************)
(* Debugging handling *)

module Debug =
struct

  type info

  external get_global : unit -> info = "caml_gammu_GSM_GetGlobalDebug"

  let global = get_global ()

  external set_global : info -> bool -> unit
    = "caml_gammu_GSM_SetDebugGlobal"

  external set_output : info -> out_channel -> unit
    = "caml_gammu_GSM_SetDebugFileDescriptor"

  external set_level : info -> string -> unit
    = "caml_gammu_GSM_SetDebugLevel"

end


(************************************************************************)
(* State machine types *)

type t

type config = {
  model : string;
  debug_level : string;
  device : string;
  connection : string;
  sync_time : bool;
  lock_device : bool;
  debug_file : string;
  start_info : bool;
  use_global_debug_file : bool;
  text_reminder : string;
  text_meeting : string;
  text_call : string;
  text_birthday : string;
  text_memo : string;
}

type connection_type =
  | BUS2
  | FBUS2
  | FBUS2DLR3
  | DKU2AT
  | DKU2PHONET
  | DKU5FBUS2
  | ARK3116FBUS2
  | FBUS2PL2303
  | FBUS2BLUE
  | FBUS2IRDA
  | PHONETBLUE
  | AT
  | BLUEGNAPBUS
  | IRDAOBEX
  | IRDAGNAPBUS
  | IRDAAT
  | IRDAPHONET
  | BLUEFBUS2
  | BLUEAT
  | BLUEPHONET
  | BLUEOBEX
  | FBUS2USB
  | NONE


(************************************************************************)
(* INI files *)

module INI =
struct
  type section_node
  type sections = {
    head : section_node;
    unicode : bool;
  }

  external _read : string -> bool -> section_node = "caml_gammu_INI_ReadFile"
  let read ?(unicode=false) file_name =
    { head = _read file_name unicode;
      unicode = unicode; }

  external _find_gammurc_force : string -> section_node
    = "caml_gammu_GSM_FindGammuRC_force"
  external _find_gammurc : unit -> section_node = "caml_gammu_GSM_FindGammuRC"
  let of_gammurc ?path () =
    let s_node = match path with
      | None -> _find_gammurc ()
      | Some path -> _find_gammurc_force path
    in
    (* TODO: Check if can set a better unicode flag. *)
    { head = s_node; unicode=false; }

  external _config_of_ini : section_node -> int -> config
    = "caml_gammu_GSM_ReadConfig"

  let config cfg_info num =
    _config_of_ini cfg_info.head num

  external _get_value : section_node -> string -> string -> bool -> string
    = "caml_gammu_INI_GetValue"
  let get_value file_info ~section ~key =
    _get_value file_info.head section key file_info.unicode

end


(************************************************************************)
(* State machine *)

external get_debug : t -> Debug.info = "caml_gammu_GSM_GetDebug"

external _init_locales : string -> unit = "caml_gammu_GSM_InitLocales"
external _init_default_locales : unit -> unit
  = "caml_gammu_GSM_InitDefaultLocales"
let init_locales ?path () = match path with
  | None -> _init_default_locales ()
  | Some path -> _init_locales path

external alloc_state_machine : unit -> t = "caml_gammu_GSM_AllocStateMachine"

external _get_config : t -> int -> config = "caml_gammu_GSM_GetConfig"
let get_config ?(num=(-1)) s = _get_config s num

external push_config : t -> config -> unit = "caml_gammu_push_config"

external remove_config : t -> unit = "caml_gammu_remove_config"

external length_config : t -> int = "caml_gammu_GSM_GetConfigNum"

let load_gammurc ?path ?(section=0) s =
  let ini = INI.of_gammurc ?path () in
  let cfg = INI.config ini section in
  push_config s cfg

let make ?section () =
  let s = alloc_state_machine() in
  load_gammurc ?section s;
  s

external _connect : t -> int -> unit= "caml_gammu_GSM_InitConnection"
external _connect_log : t -> int -> (string -> unit) -> unit
  = "caml_gammu_GSM_InitConnection_Log"

let connect ?log ?(replies=3) s = match log with
  | None -> _connect s replies
  | Some log_func -> _connect_log s replies log_func

external disconnect : t -> unit = "caml_gammu_GSM_TerminateConnection"

external is_connected : t -> bool = "caml_gammu_GSM_IsConnected"

external get_used_connection : t -> connection_type =
  "caml_gammu_GSM_GetUsedConnection"

external _read_device : t -> bool -> int = "caml_gammu_GSM_ReadDevice"
let read_device ?(wait_for_reply=true) s =
  _read_device s wait_for_reply


(************************************************************************)
(* Security related operations with phone *)

type security_code_type =
  | SEC_SecurityCode
  | SEC_Pin
  | SEC_Pin2
  | SEC_Puk
  | SEC_Puk2
  | SEC_None
  | SEC_Phone
  | SEC_Network

external enter_security_code : t ->
  code_type:security_code_type -> code:string -> unit =
  "caml_gammu_GSM_EnterSecurityCode"

external get_security_status : t -> security_code_type =
  "caml_gammu_GSM_GetSecurityStatus"


(************************************************************************)
(* Informations on the phone *)
module Info =
struct
  type battery_charge = {
    battery_type : battery_type;
    battery_capacity : int;
    battery_percent : int;
    charge_state : charge_state;
    battery_voltage : int;
    charge_voltage : int;
    charge_current : int;
    phone_current : int;
    battery_temperature : int;
    phone_temperature : int;
  }
  and charge_state =
    | BatteryPowered
    | BatteryConnected
    | BatteryCharging
    | BatteryNotConnected
    | BatteryFull
    | PowerFault
  and battery_type =
    | Unknown_battery
    | NiMH
    | LiIon
    | LiPol

  type firmware = {
    version : string;
    ver_date : string;
    ver_num : float;
  }

  type phone_model = {
    model : string;
    number : string;
    irda : string;
  (* features : feature list; *)
  }

  type network = {
    cid : string;
    code : string;
    state : network_state;
    lac : string;
    name : string;
    gprs : gprs_state;
    packet_cid : string;
    packet_state : network_state;
    packet_lac : string;
  }
  and gprs_state =
    | Detached
    | Attached
    | Unknown_gprs
  and network_state =
    | HomeNetwork
    | NoNetwork
    | RoamingNetwork
    | RegistrationDenied
    | Unknown_network
    | RequestingNetwork

  type signal_quality = {
    signal_strength : int;
    signal_percent : int;
    bit_error_rate : int;
  }

  external network_code_name : string -> string
    = "caml_gammu_GSM_GetNetworkName"

  external country_code_name : string -> string
    = "caml_gammu_GSM_GetCountryName"

  external battery_charge : t -> battery_charge
    = "caml_gammu_GSM_GetBatteryCharge"

  external firmware : t -> firmware = "caml_gammu_GSM_GetFirmWare"

  external hardware : t -> string = "caml_gammu_GSM_GetHardware"

  external imei : t -> string = "caml_gammu_GSM_GetIMEI"

  external manufacture_month : t -> string
    = "caml_gammu_GSM_GetManufactureMonth"

  external manufacturer : t -> string = "caml_gammu_GSM_GetManufacturer"

  external model : t -> string = "caml_gammu_GSM_GetModel"

  external model_info : t -> phone_model = "caml_gammu_GSM_GetModelInfo"

  external network_info : t -> network = "caml_gammu_GSM_GetNetworkInfo"

  external product_code : t -> string = "caml_gammu_GSM_GetProductCode"

  external signal_quality : t -> signal_quality
    = "caml_gammu_GSM_GetSignalQuality"

end


(************************************************************************)
(* Date and time *)

module DateTime =
struct

  type t = {
    timezone : int;
    second : int;
    minute : int;
    hour : int;
    day : int;
    month : int;
    year : int;
  }

  let compare d1 d2 =
    (* Rough integer representation of the GMT date and time. *)
    let int_of_date d =
      d.year * 32140800 + d.month * 2678400 + d.day * 86400
      + d.hour * 3600 + d.minute * 60 + d.second
      - d.timezone
    in
    compare (int_of_date d1) (int_of_date d2)

  let ( = ) d1 d2 = compare d1 d2 = 0
  let ( <> ) d1 d2 = compare d1 d2 <> 0
  let ( < ) d1 d2 = compare d1 d2 < 0
  let ( > ) d1 d2 = compare d1 d2 > 0
  let ( <= ) d1 d2 = compare d1 d2 <= 0
  let ( >= ) d1 d2 = compare d1 d2 >= 0

  external check_date : t -> bool = "caml_gammu_GSM_CheckDate"

  external check_time : t -> bool = "caml_gammu_GSM_CheckTime"

  let check t = check_date t && check_time t

  external os_date : t -> string = "caml_gammu_GSM_OSDate"

  external _os_date_time : t -> bool -> string = "caml_gammu_GSM_OSDateTime"

  let os_date_time ?(timezone=false) dt = _os_date_time dt timezone

end


(************************************************************************)
(* Memory *)

type memory_type =
  | ME (** Internal memory of the mobile equipment *)
  | SM (** SIM card memory *)
  | ON (** Own numbers *)
  | DC (** Dialled calls *)
  | RC (** Received calls *)
  | MC (** Missed calls *)
  | MT (** Combined ME and SIM phonebook *)
  | FD (** Fixed dial *)
  | VM (** Voice mailbox *)
  | SL (** Sent SMS logs *)
  | QD (** Quick dialing choices *)

type binary_picture

type memory_entry = {
  memory_type : memory_type; (** Used memory for phonebook entry. *)
  location : int; (** Used location for phonebook entry. *)
  entries : sub_memory_entry array; (** Values of SubEntries. *)
}
and sub_memory_entry = {
  entry_type : entry_type; (** Type of entry. *)
  voice_tag : int; (** Voice dialling tag. *)
  sms_list : int array;
  call_length : int;
  add_error : error; (** During adding SubEntry Gammu can return here info,
                         if it was done OK. *)
}
and entry_type =
| Number_General of string     (** General number. *)
| Number_Mobile of string      (** Mobile number. *)
| Number_Work of string        (** Work number. *)
| Number_Fax of string         (** Fax number. *)
| Number_Home of string        (** Home number. *)
| Number_Pager of string       (** Pager number. *)
| Number_Other of string       (** Other number. *)
| Text_Note of string          (** Note. *)
| Text_Postal of string        (** Complete postal address. *)
| Text_Email of string         (** Email. *)
| Text_Email2 of string
| Text_URL of string           (** URL *)
| Date of DateTime.t           (** Date and time of last call. *)
| Caller_Group of int          (** Caller group. *)
| Text_Name of string          (** Name. *)
| Text_LastName of string      (** Last name. *)
| Text_FirstName of string     (** First name. *)
| Text_Company of string       (** Company. *)
| Text_JobTitle of string      (** Job title. *)
| Category of string option    (** Category. *)
| Private of int               (** Whether entry is private. *)
| Text_StreetAddress of string (** Street address. *)
| Text_City of string          (** City. *)
| Text_State of string         (** State. *)
| Text_Zip of string           (** Zip code. *)
| Text_Country of string       (** Country. *)
| Text_Custom1 of string       (** Custom information 1. *)
| Text_Custom2 of string       (** Custom information 2. *)
| Text_Custom3 of string       (** Custom information 3. *)
| Text_Custom4 of string       (** Custom information 4. *)
| RingtoneID of int            (** Ringtone ID. *)
| PictureID of int             (** Picture ID. *)
| Text_UserID of string        (** User ID. *)
| CallLength of int            (** Length of call. *)
| Text_LUID of string (** LUID - Unique Identifier used for synchronisation. *)
| LastModified of DateTime.t   (** Date of last modification. *)
| Text_NickName of string      (** Nick name. *)
| Text_FormalName of string    (** Formal name. *)
| Text_WorkStreetAddress of string (** Work street address. *)
| Text_WorkCity of string      (** Work city. *)
| Text_WorkState of string     (** Work state. *)
| Text_WorkZip of string       (** Work zip code. *)
| Text_WorkCountry of string   (** Work country. *)
| Text_WorkPostal of string    (** Complete work postal address. *)
| Text_PictureName of string   (** Picture name (on phone filesystem). *)
| PushToTalkID of string       (** Push-to-talk ID. *)
| Number_Messaging of string   (** Favorite messaging number. *)
| Photo of binary_picture      (** Photo. *)
| Number_Mobile_Home of string (** Home mobile number. *)
| Number_Mobile_Work of string (** Work mobile number. *)


(************************************************************************)
(* Messages *)

module SMS =
struct

  type state = Sent | Unsent | Read | Unread

  (* udh_type would be more appropriate but we want to be close to
     libgammu choices. *)
  type udh =
  | No_udh
  | ConcatenatedMessages
  | ConcatenatedMessages16bit
  | DisableVoice
  | DisableFax
  | DisableEmail
  | EnableVoice
  | EnableFax
  | EnableEmail
  | VoidSMS
  | NokiaRingtone
  | NokiaRingtoneLong
  | NokiaOperatorLogo
  | NokiaOperatorLogoLong
  | NokiaCallerLogo
  | NokiaWAP
  | NokiaWAPLong
  | NokiaCalendarLong
  | NokiaProfileLong
  | NokiaPhonebookLong
  | UserUDH
  | MMSIndicatorLong
  and udh_header = {
    udh : udh;
    udh_text : string;
    id8bit : int;
    id16bit : int;
    part_number : int;
    all_parts : int;
  }

  type format = Pager | Fax | Email | Text

  type validity = Not_available | Relative of char

  type smsc = {
    smsc_location : int;
    smsc_name : string;
    smsc_number : string;
    validity : validity;
    format : format;
    default_number : string;
  }

  type message_type =
  | Deliver
  | Status_Report
  | Submit

  type coding =
  | Unicode_No_Compression
  | Unicode_Compression
  | Default_No_Compression
  | Default_Compression
  | Eight_bit

  type message = {
    replace : char;
    reject_duplicates : bool;
    udh_header : udh_header;
    number : string;
    other_numbers : string array;
    smsc : smsc;
    memory : memory_type;
    message_number : int;
    folder : int;
    inbox_folder : bool;
    state : state;
    nokia_name : string;
    text : string;
    pdu : message_type;
    coding : coding;
    date_time : DateTime.t;
    smsc_time : DateTime.t;
    delivery_status : char;
    reply_via_same_smsc : bool;
    sms_class : char;
    message_reference : char;
  }

  type multi_sms = message array

  let default_received =
    { replace = '\x00';
      reject_duplicates = false;
      udh_header =
        { udh = No_udh; udh_text = ""; id8bit = 0;
          id16bit = 0; part_number = 0; all_parts = 0 };
      number = "";
      other_numbers = [||];
      smsc =
        {smsc_location = 1; smsc_name = "";
         smsc_number = ""; validity = Not_available;
         format = Text; default_number = ""};
      memory = SM;
      message_number = 0;
      folder = 1;
      inbox_folder = true;
      state = Unread;
      nokia_name = "";
      text = "";
      pdu = Deliver;
      coding = Default_No_Compression;
      date_time =
        { DateTime.timezone = 0; DateTime.second = 0;
          DateTime.minute = 0; DateTime.hour = 0;
          DateTime.day = 0; DateTime.month = 0;
          DateTime.year = 0 };
      smsc_time =
        { DateTime.timezone = 0; DateTime.second = 0;
          DateTime.minute = 0; DateTime.hour = 0;
          DateTime.day = 0; DateTime.month = 0;
          DateTime.year = 0 };
      delivery_status = '\x00';
      reply_via_same_smsc = false;
      sms_class = '\x00';
      message_reference = '\x00' }

  external get : t -> folder:int -> message_number:int -> multi_sms =
    "caml_gammu_GSM_GetSMS"

  external _get_next : t -> location:int -> folder:int -> bool -> multi_sms
    = "caml_gammu_GSM_GetNextSMS"

  let rec fold_loop s location folder n ~retries retries_num on_err f acc =
    if n = 0 then acc
    else (
      try
        let multi_sms =
          if location = -1 then
            (* Start from the beginning of the folder. *)
            _get_next s ~location:0 ~folder true
          else
            (* Get next location, folder need to be 0 because the
               location carries the folder in its representation. *)
            _get_next s ~location ~folder:0 false
        in
        (* Not a tail recursive call but the number of SMS messages is
           assumed to be small: *)
        fold_loop s multi_sms.(0).message_number folder (n - 1)
                  ~retries 0 on_err f (f acc multi_sms)
      with
      | Error EMPTY -> acc (* There's no next SMS message *)
      | Error (UNKNOWN | CORRUPTED as e) ->
        on_err location e;
        if retries_num = retries then
          (* Continue with next message. *)
          fold_loop s (location + 1) folder n ~retries 0 on_err f acc
        else
          (* Retry retrieval. *)
          fold_loop s location folder n ~retries (retries_num + 1) on_err f acc
    )

  let fold s ?(folder=0) ?(n=(-1)) ?(retries=2) ?(on_err=(fun _ _ -> ())) f a =
    fold_loop s (-1) folder n ~retries 0 on_err f a

  external set : t -> message -> int * int = "caml_gammu_GSM_SetSMS"

  external add : t -> message -> int * int = "caml_gammu_GSM_AddSMS"

  external send : t -> message -> unit = "caml_gammu_GSM_SendSMS"

  type folder = {
    box : folder_box;
    folder_memory : memory_type;
    name : string;
  }
  and folder_box = Inbox | Outbox

  external folders : t -> folder array = "caml_gammu_GSM_GetSMSFolders"

  type memory_status = {
    sim_unread : int;
    sim_used : int;
    sim_size : int;
    templates_used : int;
    phone_unread : int;
    phone_used : int;
    phone_size : int;
  }

  external get_status : t -> memory_status = "caml_gammu_GSM_GetSMSStatus"

  external set_incoming_sms : t -> bool -> unit
    = "caml_gammu_GSM_SetIncomingSMS"

  external _delete : t -> int -> int -> unit = "caml_gammu_GSM_DeleteSMS"
  let delete s ~folder ~message_number =
    _delete s message_number folder

  type multipart_info = {
    unicode_coding : bool;
    info_class : int;
    replace_message : char;
    unknown : bool;
    entries : info array;
  }
  and info = {
    id : encode_part_type_id;
    nbr : int;
    (* TODO: use <encode_part_type_id> of ... as [id] and handle the following
       fields used by the [id] type. *)
    (* ringtone : ringtone; (* NYI *)
       bitmap : multi_bitmap; (* NYI *)
       bookmark : wap_bookmark; (* NYI *)
       settings : wap_settings; (* NYI *)
       mms_indicator : mms_indicator; (* NYI *)
       phonebook : memory_entry; *)
    (* calendar : calendar_entry; (* NYI *)
       todo : todo_entry; (* NYI *)
       file : file; (* NYI *) *)
    protected : bool;
    buffer : string;
    (* TODO:?? use a variant type for alignment ? *)
    left : bool;
    right : bool;
    center : bool;
    large : bool;
    small : bool;
    bold : bool;
    italic : bool;
    underlined : bool;
    strikethrough : bool;
    ringtone_notes : int;
  }
  and encode_part_type_id =
    | Text (* of Nothing *)
    | ConcatenatedTextLong (* of Nothing *)
    | ConcatenatedAutoTextLong (* of Nothing *)
    | ConcatenatedTextLong16bit (* of Nothing *)
    | ConcatenatedAutoTextLong16bit (* of Nothing *)
    | NokiaProfileLong (* of multi_bitmap *)
    | NokiaPictureImageLong
    | NokiaScreenSaverLong
    | NokiaRingtone
    | NokiaRingtoneLong
    | NokiaOperatorLogo
    | NokiaOperatorLogoLong
    | NokiaCallerLogo
    | NokiaWAPBookmarkLong
    | NokiaWAPSettingsLong
    | NokiaMMSSettingsLong
    | NokiaVCARD10Long (* of mem_entry (* Phonebook *) *)
    | NokiaVCARD21Long
    | NokiaVCALENDAR10Long
    | NokiaVTODOLong
    | VCARD10Long
    | VCARD21Long
    | DisableVoice
    | DisableFax
    | DisableEmail
    | EnableVoice
    | EnableFax
    | EnableEmail
    | VoidSMS
    | EMSSound10
    | EMSSound12
    | EMSSonyEricssonSound
    | EMSSound10Long
    | EMSSound12Long
    | EMSSonyEricssonSoundLong
    | EMSPredefinedSound
    | EMSPredefinedAnimation
    | EMSAnimation
    | EMSFixedBitmap
    | EMSVariableBitmap
    | EMSVariableBitmapLong
    | MMSIndicatorLong
    | WAPIndicatorLong
    | AlcatelMonoBitmapLong
    | AlcatelMonoAnimationLong
    | AlcatelSMSTemplateName
    | SiemensFile

  external _decode_multipart :
    Debug.info -> multi_sms -> bool -> multipart_info
      = "caml_gammu_GSM_DecodeMultiPartSMS"

  let decode_multipart ?debug ?(ems=true) multp_mess =
    let di = match debug with
      | None -> Debug.global
      | Some s_di -> s_di
    in
    _decode_multipart di multp_mess ems

end


(************************************************************************)
(* Calls *)

module Call =
struct

  type call = {
    status : status;
    call_id : int option;
    number : string;
  }
  and status =
    | Incoming
    | Outgoing
    | Started
    | Ended
    | RemoteEnded of int
    | LocalEnded
    | Established
    | Held
    | Resumed
    | Switched

end


(************************************************************************)
(* Events *)

external enable_incoming_sms : t -> bool -> unit
  = "caml_gammu_GSM_SetIncomingSMS"

external _incoming_sms : t -> (SMS.message -> unit) -> unit
  = "caml_gammu_GSM_SetIncomingSMSCallback"

let incoming_sms ?(enable=true) s f =
  _incoming_sms s f;
  enable_incoming_sms s enable

external enable_incoming_call : t -> bool -> unit
  = "caml_gammu_GSM_SetIncomingCall"

external _incoming_call : t -> (Call.call -> unit) -> unit
  = "caml_gammu_GSM_SetIncomingCallCallback"

let incoming_call ?(enable=true) s f =
  _incoming_call s f;
  enable_incoming_call s enable

