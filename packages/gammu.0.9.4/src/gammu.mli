(* File: gammu.mli

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


(** Interface to the gammu library (libGammu) to manage data in your
    cell phone such as contacts, calendar and messages.

    NOTE: Strings used by libGammu often have a maximum allowed
    length. Strings too long will be trimmed before being passed to
    libGammu (this library considers OCaml strings as immutable).

    NOTE: This library is {i not} thread safe. *)


(************************************************************************)
(** {2 Error handling} *)

(** Possible errors. *)
type error =
  | DEVICEOPENERROR     (** Error during opening device *)
  | DEVICELOCKED        (** Device locked *)
  | DEVICENOTEXIST      (** Device does not exist *)
  | DEVICEBUSY          (** Device is busy *)
  | DEVICENOPERMISSION  (** No permissions to open device *)
  | DEVICENODRIVER      (** No driver installed for a device *)
  | DEVICENOTWORK       (** Device doesn't seem to be working *)
  | DEVICEDTRRTSERROR   (** Error during setting DTR/RTS in device *)
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
  | ABORTED             (** Operation aborted. *)
  | INSTALL_NOT_FOUND   (** Installation data not found. *)
  | READ_ONLY           (** Entry is read only. *)
  | NETWORK_ERROR       (** Network error. *)
  (* Errors specific to the OCaml bindings (not present in Gammu): *)
  | INI_KEY_NOT_FOUND   (** Pair section/value not found in INI file. *)
  | COULD_NOT_DECODE    (** Decoding SMS Message failed. *)
  | INVALID_CONFIG_NUM  (** Invalid config number. *)

val string_of_error : error -> string
(** [string_of_error e] returns a textual description of the error [e]. *)

exception Error of error
(** May be raised by any of the functions of this module to indicate an
    error. *)


(************************************************************************)
(** {2 Debugging settings} *)

module Debug :
sig

  type info
  (** Value holding the debugging preferences. *)

  val global : info
  (** Global debug settings. *)

  val set_global : info -> bool -> unit
  (** [set_global di] enables the use of the global debugging
      configuration for [di].  Has no effect on the global debug
      configuration itself. *)

  val set_output : info -> out_channel -> unit
  (** [set_debug_output di channel] sets output channel of [di] to
      [channel]. *)

  val set_level : info -> string -> unit
  (** [set_debug_level di level] sets debug level on [di] according to
      [level].

      [level] must be one of :
      {ul
      {li nothing - no debugging output}
      {li text - transmission dump in text format}
      {li textall - all possible info in text format}
      {li textalldate - all possible info in text format, with time stamp}
      {li errors - errors in text format}
      {li errorsdate - errors in text format, with time stamp}
      {li binary - transmission dump in binary format}}
  *)
end


(************************************************************************)
(** {2 State machine} *)

type t
(** Value holding information about phone connection (called a "state
    machine"). *)

(** Configuration of the state machine.  *)
type config = {
  model : string;
  (** Model from config file. Leave it empty for autodetection. Or define a
      phone model to force the phone model and bypass automatic phone model
      detection. *)
  debug_level : string;        (** Debug level. See {!Gammu.Debug.set_level}
                                   for acceptable level strings. *)
  device : string;             (** Device name from config file such as "com2"
                                   or "/dev/ttyS1". *)
  connection : string;         (** Connection type as string  *)
  sync_time : bool;            (** Synchronize time on startup?  *)
  lock_device : bool;          (** Lock device ? (Unix, ignored on Windows) *)
  debug_file : string;         (** Name of debug file  *)
  start_info : bool;           (** Display something during start ?  *)
  use_global_debug_file : bool; (** Should we use global debug file?  *)
  text_reminder : string;      (** Text for reminder calendar entry category
                                   in local language  *)
  text_meeting : string;       (** Text for meeting calendar entry category
                                   in local language  *)
  text_call : string;          (** Text for call calendar entry category
                                   in local language  *)
  text_birthday : string;      (** Text for birthday calendar entry category
                                   in local language  *)
  text_memo : string;          (** Text for memo calendar entry
                                   category in local language *)
(* phone_features : feature list (** NYI Phone features override. *) *)
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

val get_debug : t -> Debug.info
(** Gets debug information for state machine. *)

val init_locales : ?path:string -> unit -> unit
(** Initializes locales. This sets up things needed for proper string
    conversion from local charset as well as initializes gettext based
    translation.  This module is automatically initialized the same
    way [init_locales()] would do.

    @param path Path to gettext translation. If not set, compiled in
    default is used. *)

val make : ?section: int -> unit -> t
(** Make a new clean state machine.  It is automatically configured
    using {!load_gammurc}.  If you want to configure it yourself, use
    {!push_config} to supersede the configuration with the one of your
    choice.

    @param section section number of the gammurc file to read. See
    {!Gammu.INI.config} for details. *)

val get_config : ?num:int -> t -> config
(** [get_config s ~num] gets gammu configuration from state machine [s],
    where [num] is the number of the section to read, starting from
    zero. If not specified, the currently used one is returned. *)

val push_config : t -> config -> unit
(** [push_config s cfg] push the configuration [cfg] on top of the
    configuration stack of [s].

    Gammu tries each configuration, from the bottom to the top of the stack,
    in order to connect. *)

val remove_config : t -> unit
(** [remove_config s] remove the top configuration from the config stack of
    [s]. *)

val length_config : t -> int
(** @return length of the configuration stack of the state machine. i.e
    the number of active configurations. *)

val load_gammurc : ?path:string -> ?section:int -> t -> unit
(** Automaticaly finds the gammurc file (see {!Gammu.INI.of_gammurc}),
    read it and push the config in the state machine.

    @param path force the use of a custom path instead of the autodetected one
    (default: autodetection is performed).

    @param section section number of the gammurc file to read. See
    {!Gammu.INI.config} for details. *)

val connect : ?log:(string -> unit) -> ?replies:int -> t -> unit
(** Initiates connection.

    IMPORTANT: do not forget to call disconnect when done as otherwise the
    connection may be prematurely terminated. In fact, the problem is that if
    you have no reference to the state machine left, the GC may free it and by
    the same time terminate your connection.

    @param log logging function.

    @param replies number of replies to wait for on each request (default: 3).

    @raise UNCONFIGURED if no configuration was set. *)

val disconnect : t -> unit

val is_connected : t -> bool

val get_used_connection : t -> connection_type

val read_device : ?wait_for_reply:bool -> t -> int
(** Attempts to read data from phone. Thus can be used for getting status
    of incoming events, which would not be found out without polling
    device.

    @return the number of read bytes.

    @param wait_for_reply whether to wait for some event (default true). *)


(************************************************************************)
(** {2 INI files} *)

(** These functions parse ini file and make them available in easily
    accessible manner. *)
module INI : sig

  (*type entry (* Useless, never used and abstract *) *)
  (* TODO:?? section is in fact a node of a doubly-linked list. Should
     this be reflected on the interface ? Along with FIXME in [read],
     store the unicode flag in the abstract [section] type or expose it to
     public interface ? *)
  type sections

  val read : ?unicode:bool -> string -> sections
  (** [read fname] reads INI data from the file [fname].

      @param unicode Whether file should be treated as unicode encoded
      (default = [false], beware that unicode handling is somewhat buggy in
      libGammu). *)

  val of_gammurc : ?path:string -> unit -> sections
  (** Finds and reads gammu configuration file.  The search order depends on
      platform.  On POSIX systems it looks for ~/.gammurc and then for
      /etc/gammurc and also follows freedesktop.org/XDG specifications and
      reads ~/.config/gammu/config (in versions > 1.28.92 of libGammu), on
      Windows for gammurc in Application data folder, then in home and last
      fallback is in current directory.

      @param path force the use of a custom path instead of the autodetected
      one (default: autodetection is performed).

      Raises [Error CANTOPENFILE] if no gammu rc file can be found.
      Raises [Error FILENOTSUPPORTED] if first found gammu rc file is
      not valid. *)

  val config : sections -> int -> config
  (** [read_config section num] processes and returns gammu configuration
      represented by the [num]th section of the INI file representation
      [section].  Beware that [num]th section is in facts the section named
      "gammu[num]" in the file itself. *)

  val get_value : sections -> section:string -> key:string -> string
  (** @return value of the INI file entry. *)
end


(************************************************************************)
(** {2 Security related operations with phone. } *)

(** Definition of security codes. *)
type security_code_type =
  | SEC_SecurityCode (** Security code. *)
  | SEC_Pin     (** PIN. *)
  | SEC_Pin2    (** PIN 2. *)
  | SEC_Puk     (** PUK. *)
  | SEC_Puk2    (** PUK 2. *)
  | SEC_None    (** Code not needed. *)
  | SEC_Phone   (** Phone code needed. *)
  | SEC_Network (** Network code needed. *)

val enter_security_code :
  t -> code_type:security_code_type -> code:string -> unit
(** Enter security code (PIN, PUK,...). *)

val get_security_status : t -> security_code_type
(** Query whether some security code needs to be entered. *)


(************************************************************************)
(** {2 Informations on the phone} *)

(** Informations on the phone. *)
module Info :
sig

  type battery_charge = {
    battery_type : battery_type; (** Battery type. *)
    battery_capacity : int;      (** Remaining battery capacity (in mAh). *)
    battery_percent : int;       (** Remaining battery capacity in
                                     percent, -1 = unknown. *)
    charge_state : charge_state; (** Charge state. *)
    battery_voltage : int;       (** Current battery voltage (in mV). *)
    charge_voltage : int;        (** Voltage from charger (in mV). *)
    charge_current : int;        (** Current from charger (in mA). *)
    phone_current : int;         (** Phone current consumption (in mA). *)
    battery_temperature : int;   (** Battery temperature
                                     (in degrees Celsius). *)
    phone_temperature : int;     (** Phone temperature (in degrees Celsius). *)
  }
  and charge_state =
    | BatteryPowered      (** Powered from battery *)
    | BatteryConnected    (** Powered from AC, battery connected *)
    | BatteryCharging     (** Powered from AC, battery is charging *)
    | BatteryNotConnected (** Powered from AC, no battery *)
    | BatteryFull         (** Powered from AC, battery is fully charged *)
    | PowerFault          (** Power failure  *)
  and battery_type =
    | Unknown_battery     (** Unknown battery *)
    | NiMH        (** NiMH battery *)
    | LiIon       (** Lithium Ion battery *)
    | LiPol       (** Lithium Polymer battery *)

  type firmware = {
    version : string;
    ver_date : string;
    ver_num : float;
  }

  (** Model identification, used for finding phone features. *)
  type phone_model = {
    model : string;          (** Model as returned by phone *)
    number : string;         (** Identification by Gammu *)
    irda : string;           (** Model as used over IrDA *)
  (* features : feature list; (** NYI List of supported features *)*)
  }

  (** Current network informations *)
  type network = {
    cid : string;                 (** Cell ID (CID) *)
    code : string;                (** GSM network code *)
    state : network_state;        (** Status of network logging. *)
    lac : string;                 (** LAC (Local Area Code) *)
    name : string;                (** Name of current netwrok as returned
                                      from phone (or empty) *)
    gprs : gprs_state;            (** GRPS state *)
    packet_cid : string;          (** Cell ID (CID) for packet network *)
    packet_state : network_state; (** Status of network logging
                                      for packet data. *)
    packet_lac : string;          (** LAC (Local Area Code)
                                      for packet network *)
  }
  and gprs_state =
    | Detached
    | Attached
    | Unknown_gprs
  and network_state =
    | HomeNetwork          (** Home network for used SIM card. *)
    | NoNetwork            (** No network available for used SIM card. *)
    | RoamingNetwork       (** SIM card uses roaming. *)
    | RegistrationDenied   (** Network registration denied
                               - card blocked or expired or disabled. *)
    | Unknown_network      (** Unknown network status. *)
    | RequestingNetwork    (** Network explicitely requested by user. *)

  (** Information about signal quality, all these should be -1 when
      unknown. *)
  type signal_quality = {
    signal_strength : int;
    signal_percent : int;  (* Signal strength in percent. *)
    bit_error_rate : int;  (* Bit error rate in percent.  *)
  }

  val network_code_name : string -> string
  (** [network_code_name code] returns the name the network designed by the
      code [code], of the form "\[0-9\]\{3\} \[0-9\]\{2\}". *)

  val country_code_name : string -> string
  (** [country_code_name code] returns the name of the country designed by the
      code [code], of the form "\[0-9\]\{3\}" (the first 3 digits of the
      network code). *)

  val battery_charge : t -> battery_charge
  (** @return information about battery charge and phone charging state. *)

  val firmware : t -> firmware

  val hardware : t -> string

  val imei : t -> string
  (** @return IMEI (International Mobile Equipment Identity) / Serial
      Number *)

  val manufacture_month : t -> string

  val manufacturer : t -> string

  val model : t -> string

  val model_info : t -> phone_model

  val network_info : t -> network

  val product_code : t -> string

  val signal_quality : t -> signal_quality

end


(************************************************************************)
(** {2 Date and time} *)

(** Date and time handling. *)
module DateTime :
sig

  type t = {
    timezone : int; (** The difference between local time and GMT in seconds *)
    second : int;
    minute : int;
    hour : int;
    day : int;
    month : int;    (** January = 1, February = 2, etc. *)
    year : int;     (** 4 digits year number. *)
  } (** Date and time type. *)

  val compare : t -> t -> int
  (** [compare d1 d2] returns [0] if [d1] is the same date and time as [d2], a
      negative integer if [d1] is before [d2], and a positive integer if [d1]
      is after [d2]. *)

  (** The usual six comparison operators (to benefit from local open,
      i.e. write [Gammu.DateTime.(d1 < d2)] instead of
      [Gammu.DateTime.compare d1 d2 < 0]. *)
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool

  val check_date : t -> bool
  (** Checks whether date is valid. This does not check time, see [check_time]
      for this. *)

  val check_time : t -> bool
  (** Checks whether time is valid. This does not check date, see [check_date]
      for this. *)

  val check : t -> bool
  (** Checks whether both date and time are valid (with [check_date]
      and [check_time]). *)

  val os_date : t -> string
  (** Converts date from timestamp to string according to OS settings. *)

  val os_date_time : ?timezone:bool -> t -> string
  (** Converts timestamp to string according to OS settings.

      @param timezone Whether to include time zone (default false). *)

end


(************************************************************************)
(** {2 Memory} *)

(** Defines ID for various phone and SIM memories.  Phone modules can
    translate them to values specific for concrete models.  Two letter
    codes (excluding VM and SL) are from GSM 07.07. *)
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

(** Type of specific phonebook entry. *)
type entry_type =
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

(** One value of phonebook memory entry. *)
type sub_memory_entry = {
  entry_type : entry_type; (** Type of entry, with data. *)
  voice_tag : int; (** Voice dialling tag. *)
  sms_list : int array;
  call_length : int;
  add_error : error; (** During adding SubEntry Gammu can return here info,
                         if it was done OK. *)
}

type memory_entry = {
  memory_type : memory_type; (** Used memory for phonebook entry. *)
  location : int; (** Used location for phonebook entry. *)
  entries : sub_memory_entry array; (** Values of SubEntries. *)
} (** Value for saving phonebook entries. *)


(************************************************************************)
(** {2 Messages} *)

(** SMS messages manipulation.  *)
module SMS : sig

  (** Status of SMS message. *)
  type state = Sent | Unsent | Read | Unread

  (** Types of UDH (User Data Header). *)
  type udh =
  | No_udh                    (** Simple message, content in [SMS.text] *)
  | ConcatenatedMessages      (** Linked SMS. *)
  | ConcatenatedMessages16bit (** Linked SMS with 16 bit reference. *)
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

  (** Structure for User Data Header. *)
  type udh_header = {
    udh : udh;          (** UDH type. *)
    udh_text : string;  (** UDH text. *)
    id8bit : int;       (** 8-bit ID, when required (<= 0 otherwise). *)
    id16bit : int;      (** 16-bit ID, when required (<= 0 otherwise). *)
    part_number : int;  (** Number of current part. *)
    all_parts : int;    (** Total number of parts. *)
  }

  (** Format of SMS messages.  See GSM 03.40 section 9.2.3.9. *)
  type format = Pager | Fax | Email | Text

  (* Representation of the whole GSM_SMSValidity struct (2 enums in libGammu).
     The relative validity period is in fact a char, other values than those
     from GSM_ValidityPeriod can be given. *)
  type validity = Not_available | Relative of char
  (** Validity lengths for SMS messages.  See GSM 03.40 section
      9.2.3.12.1 for meanings. *)

  (* TODO: Add conversion function between relative validity period char
     and its representation.*)

  (** SMSC (SMS Center) *)
  type smsc = {
    smsc_location : int;     (** Number of the SMSC on SIM *)
    smsc_name : string;      (** Name of the SMSC *)
    smsc_number : string;    (** SMSC phone number *)
    validity : validity;     (** Validity of SMS messges. *)
    format : format;         (** Format of sent SMS messages. *)
    default_number : string; (** Default recipient number.
                                 In old DCT3 ignored. *)
  }

  type message_type =
    | Deliver           (** SMS in Inbox. *)
    | Status_Report     (** Delivery Report *)
    | Submit            (** SMS for sending or in Outbox  *)

  type coding =
    | Unicode_No_Compression     (** Unicode *)
    | Unicode_Compression
    | Default_No_Compression     (** Default GSM alphabet. *)
    | Default_Compression
    | Eight_bit                  (** 8-bit.  *)

  type message = {
    replace : char;          (** Message to be replaced. *)
    reject_duplicates : bool;(** Whether to reject duplicates. *)
    udh_header : udh_header; (** UDH (User Data Header) *)
    number : string;         (** Sender or recipient number. *)
    other_numbers : string array;
    smsc : smsc;             (** SMS Center *)
    memory : memory_type;    (** For saved SMS: where exactly
                                 it's saved (SIM/phone). *)
    message_number : int;    (** For saved SMS: location of SMS in memory. *)
    folder : int;            (** For saved SMS: number of folder,
                                 where SMS is saved. *)
    inbox_folder : bool;   (** For saved SMS: whether SMS is really in Inbox. *)
    state : state;           (** Status (read, unread,...) of SMS message. *)
    nokia_name : string;     (** Name in Nokia with SMS memory (6210/7110, etc.)
                                 Ignored in other. *)
    text : string;           (** Text for SMS. May be encoded as plain
                                 8bit ASCII or as UTF8 according to the
                                 [coding] field. *)
    pdu : message_type;      (** Type of message. *)
    coding : coding;         (** Type of coding. *)
    date_time : DateTime.t;  (** Date and time, when SMS was saved or sent. *)
    smsc_time : DateTime.t;  (** Date of SMSC response in DeliveryReport
                                 messages. *)
    delivery_status : char;  (** In delivery reports: status. *)
    reply_via_same_smsc : bool; (** Indicates whether "Reply via same
                                    center" is set. *)
    sms_class : char;        (** SMS class (0 is flash SMS, 1 is normal one). *)
    message_reference : char; (** Message reference. *)
  }

  type multi_sms = message array
  (** Multiple SMS messages, used for Smart Messaging 3.0/EMS. *)

  val default_received : message
  (** Empty message with default values needed for saving a received SMS. *)

  val get : t -> folder:int -> message_number:int -> multi_sms
  (** Read a SMS message. *)

  val fold : t -> ?folder:int -> ?n:int -> ?retries:int ->
    ?on_err:(int -> error -> unit) -> ('a -> multi_sms -> 'a) -> 'a -> 'a
  (** [fold s f a] fold SMS messages through the function [f] with [a] as
      initial value, iterating trough SMS' *and* folders).

      This function uses the GSM_GetNextSMS function from
      libGammu.  This might be faster for some phones than using
      {!Gammu.SMS.get} for each message.

      Please note that this command may not mark the messages as read in
      the phone.  To make sure they are, call {!Gammu.SMS.get}.

      @param folder specifies the folder from where to start folding SMS
      (default = 0, the first one).

      @param n how many messages are to be folded (at maximum).  If
      negative, the fold goes over all messages from the beginning of the
      given folder and higher numbered ones (default = -1).

      @param retries how many times to retry (first try not counted in) the
      retrieval of one message in case of error [UNKNOWN] or
      [CORRUPTED]. TODO: On some phones (symbian/gnapgen), it may just loop
      forever (since the location argument is ignored in their
      driver). (default = 2)

      @param on_err function to handle errors [UNKNOWN] or [CORRUPTED]. The
      location of the SMS message for which the next one failed to be read and
      the error are given (default: does nothing).

      @raise NOTIMPLEMENTED if GetNext function is not implemented in libGammu
      for the currently used phone.

      @raise NOTSUPPORTED if the mechanism is not supported by the phone. *)

  val set : t -> message -> int * int
  (** [set s sms] sets [sms] at the specified location and folder (given in
      {!SMS.message} representation). And returns a couple for folder and
      location really set (after transformation). *)

  val add : t -> message -> int * int
  (** [add s sms] adds [sms] to the folder specified in the [folder]
      field of [sms] and returns the couple folder and location where
      the message was stored (folder may be transformed). The location
      fields of [sms] are ignored when adding SMS, put whatever you
      want there. *)

  val send : t -> message -> unit
  (** [send s sms] sends the [sms]. *)

  type folder = {
    box : folder_box;            (** Whether it is inbox or outbox. *)
    folder_memory : memory_type; (** Where exactly it's saved. *)
    name : string;               (** Name of the folder. *)
  }
  and folder_box = Inbox | Outbox

  val folders : t -> folder array
  (** @return SMS folders information. *)

  (** Status of SMS memory. *)
  type memory_status = {
    sim_unread : int;     (** Number of unread messages on SIM. *)
    sim_used : int;       (** Number of saved messages
                              (including unread) on SIM. *)
    sim_size : int;       (** Number of possible messages on SIM. *)
    templates_used : int; (** Number of used templates
                              (62xx/63xx/7110/etc.). *)
    phone_unread : int;   (** Number of unread messages in phone. *)
    phone_used : int;     (** Number of saved messages in phone. *)
    phone_size : int;     (** Number of possible messages on phone. *)
  }

  val get_status : t -> memory_status
  (** Get information about SMS memory
      (read/unread/size of memory for both SIM and
      phone). *)

  val set_incoming_sms : t -> bool -> unit
  (** Enable/disable notification on incoming SMS. *)

  val delete : t -> folder:int -> message_number:int -> unit
  (** Deletes SMS (SMS location and folder must be set). *)


  (** ID during packing SMS for Smart Messaging 3.0, EMS and other *)
  type encode_part_type_id =
    | Text                  (** 1 text SMS. *)
    | ConcatenatedTextLong  (** Contacenated SMS, when longer than 1 SMS. *)
    | ConcatenatedAutoTextLong (** Contacenated SMS, auto Default/Unicode
                                   coding. *)
    | ConcatenatedTextLong16bit
    | ConcatenatedAutoTextLong16bit
    | NokiaProfileLong      (** Nokia profile = Name, Ringtone, ScreenSaver *)
    | NokiaPictureImageLong (** Nokia Picture Image + (text) *)
    | NokiaScreenSaverLong  (** Nokia screen saver + (text) *)
    | NokiaRingtone         (** Nokia ringtone - old SM2.0 format, 1 SMS *)
    | NokiaRingtoneLong     (** Nokia ringtone contacenated, when very long *)
    | NokiaOperatorLogo     (** Nokia 72x14 operator logo, 1 SMS *)
    | NokiaOperatorLogoLong (** Nokia 72x14 op logo or 78x21 in 2 SMS *)
    | NokiaCallerLogo       (** Nokia 72x14 caller logo, 1 SMS *)
    | NokiaWAPBookmarkLong  (** Nokia WAP bookmark in 1 or 2 SMS *)
    | NokiaWAPSettingsLong  (** Nokia WAP settings in 2 SMS *)
    | NokiaMMSSettingsLong  (** Nokia MMS settings in 2 SMS *)
    | NokiaVCARD10Long     (** Nokia VCARD 1.0 (only name and default number) *)
    | NokiaVCARD21Long      (** Nokia VCARD 2.1 (all numbers + text) *)
    | NokiaVCALENDAR10Long  (** Nokia VCALENDAR 1.0 (can be in few sms) *)
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
    | EMSSound10            (** IMelody 1.0 *)
    | EMSSound12            (** IMelody 1.2 *)
    | EMSSonyEricssonSound (** IMelody without header, SonyEricsson extension *)
    | EMSSound10Long        (** IMelody 1.0 with UPI. *)
    | EMSSound12Long        (** IMelody 1.2 with UPI. *)
    | EMSSonyEricssonSoundLong (** IMelody without header with UPI. *)
    | EMSPredefinedSound
    | EMSPredefinedAnimation
    | EMSAnimation
    | EMSFixedBitmap        (** Fixed bitmap of size 16x16 or 32x32. *)
    | EMSVariableBitmap
    | EMSVariableBitmapLong
    | MMSIndicatorLong      (** MMS message indicator. *)
    | WAPIndicatorLong
    | AlcatelMonoBitmapLong (** Variable bitmap with black and white
                                colors *)
    | AlcatelMonoAnimationLong (** Variable animation with black and white
                                   colors *)
    | AlcatelSMSTemplateName
    | SiemensFile           (** Siemens OTA *)

  (** SMS information, like type, text, text properties, etc... *)
  type info = {
    id : encode_part_type_id;
    nbr : int;
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

 (** Multipart SMS Information *)
  type multipart_info = {
    unicode_coding : bool;
    info_class : int;
    replace_message : char;
    unknown : bool;
    entries : info array;
  }

  val decode_multipart : ?debug:Debug.info -> ?ems:bool ->
    multi_sms -> multipart_info
  (** [decode_multipart sms] Decode the multi part SMS [sms] to
      "readable" format.  [sms] is modified.  Return a
      {!Gammu.SMS.multipart_info} associated.

      @param debug log according to debug settings from [di]. If not
      specified, use the one returned by {!Gammu.Debug.global}.

      @param ems whether to use EMS (Enhanced Messaging Service)
      (default true). *)
end

(************************************************************************)
(** {2 Calls} *)

(** Call entries manipulation. *)
module Call : sig

  (** Status of call. *)
  type status =
    | Incoming    (** Somebody calls to us. *)
    | Outgoing    (** We call somewhere. *)
    | Started     (** Call started. *)
    | Ended       (** End of call from unknown side. *)
    | RemoteEnded of int (** End of call from remote side.
                             Parameter is status code. *)
    | LocalEnded  (** End of call from our side. *)
    | Established (** Call established. Waiting for answer or dropping. *)
    | Held        (** Call held. *)
    | Resumed     (** Call resumed. *)
    | Switched    (** We switch to call. *)

  (** Call information. *)
  type call = {
    status : status;       (** Call status. *)
    call_id : int option;  (** Call ID, None when not available. *)
    number : string;       (** Remote phone number. *)
  }

end

(************************************************************************)
(** {2 Events} *)

val incoming_sms : ?enable:bool -> t -> (SMS.message -> unit) -> unit
(** [incoming_sms s f] register [f] as callback function in the event of an
    incoming SMS.

    @param enable whether to enable notifications or not. (default = true) *)

val enable_incoming_sms : t -> bool -> unit
(** [enable_incoming_sms t enable] enable incoming sms events or not,
    according to [enable]. *)

val incoming_call : ?enable:bool -> t -> (Call.call -> unit) -> unit
(** [incoming_call s f] register [f] as callback function in the event of
    incoming call.

    @param enable whether to enable notifications or not. (default = true) *)

val enable_incoming_call : t -> bool -> unit
(** [enable_incoming_call t enable] enable incoming call events or not,
    according to [enable]. *)

