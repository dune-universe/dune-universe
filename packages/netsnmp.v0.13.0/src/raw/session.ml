type t
type netsnmp_session

module Snmp_version = struct
  type t = 
    | Version_1
    | Version_2c
    | Version_3
end

module Snmp_sec_auth_proto = struct
 type t = 
   | Ignore
   | UsmHMACMD5AuthProtocol
end

type snmp_session = {
    version : Snmp_version.t
  ; retries : int
  ; timeout : int        
  ; peername : string
  ; localname : string
  ; local_port : int
  ; community : string
  ; securityName : string
  ; securityAuthProto : Snmp_sec_auth_proto.t
  ; securityAuthPassword : string
}

external snmp_sess_init : unit -> netsnmp_session = "caml_snmp_sess_init"
external snmp_sess_open_c : netsnmp_session -> snmp_session -> t = "caml_snmp_sess_open"
external snmp_sess_close : t -> unit = "caml_snmp_sess_close"
external snmp_sess_synch_response_c
  :  t
  -> Pdu.t
  -> ( Oid.t * ASN1_value.t ) list = "caml_snmp_sess_synch_response"

let snmp_sess_open ~netsnmp_session ~version ~retries ~timeout ~peername ~localname
  ~local_port ~community ~securityName ~securityAuthProto ~securityAuthPassword () =
  let snmp_session = {
    version 
  ; retries 
  ; timeout 
  ; peername 
  ; localname 
  ; local_port 
  ; community 
  ; securityName 
  ; securityAuthProto 
  ; securityAuthPassword 
  }
  in
    snmp_sess_open_c netsnmp_session snmp_session

let snmp_sess_synch_response t pdu =
  snmp_sess_synch_response_c t pdu |> List.rev
