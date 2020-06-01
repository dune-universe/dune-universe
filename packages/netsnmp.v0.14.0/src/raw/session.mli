(** Session handling including sending and receiving PDUs from remote peers. See
    the snmp_sess_open(3) manual page for details
*)

type t
type netsnmp_session

(** Supported SNMP versions
*)
module Snmp_version : sig
  type t =
    | Version_1
    | Version_2c
    | Version_3
end

(** Supported SNMP authentication protocols for V3
 *)
module Snmp_sec_auth_proto : sig
 type t =
   | Ignore
   | UsmHMACMD5AuthProtocol
end

(** [snmp_sess_init] - create a netsnmp session ready to open. This function is
    not thread safe as it loads and parses mibs especially on the first call.
    The value returned can be discarded once used to open the session with
    snmp_sess_open.  *)
val snmp_sess_init : unit -> netsnmp_session

(** [snmp_sess_open] creates a session between the client and host and returns a handle.
    Raises [Failure] if the C API fails.  The parameters are as follows:
- [netsnmp_session - the value returned from snmp_sess_init
- [version - snmp version
- [retries] - Number of retries before timeout.
- [timeout] - Number of uS until first timeout, then exponential backoff
- [peername] - name or address of default peer (may include transport specifier and/or port number)
- [localname] - My Domain name or dotted IP address, "" for default
- [local_port] - My UDP port number, 0 for default, picked randomly
- [community] - V1/V2c community for outgoing requests - ignore by v3.
- [securityName] - V3 user name
- [securityAuthProto] - V3 auth protocol, this will be converted to the correct oid
- [securityAuthPassword] - V3 password, will be converted to securityAuthKey
    *)
val snmp_sess_open
   : netsnmp_session:netsnmp_session
  -> version:Snmp_version.t
  -> retries:int
  -> timeout:int
  -> peername:string
  -> localname:string
  -> local_port:int
  -> community:string
  -> securityName:string
  -> securityAuthProto:Snmp_sec_auth_proto.t
  -> securityAuthPassword:string
  -> unit
  -> t

(** [snmp_sess_close] shuts down the connection and frees resources.  *)
val snmp_sess_close : t -> unit

(** snmp_sess_synch_response t pdu] sends a pdu and returns the response.
    Note that this is completely synchronous. Raises the following exceptions
 - [Netsnmp_exceptions.Response_error (err, msg)] for general connection issues,
   [err] is the error ([Netsnmp_pdu_error.t]) and [msg] the associated error
      string
 - [Netsnmp_exceptions.Request_timeout]
 - [Netsnmp_exceptions.General_error (syserr, snmperr, errmsg)] for general errors.
      [syserr] is the system error, [snmperr] is the SNMP error ([Netsnmp_error.t])
      and [errmsg] is the error message based on the error codes.
    Note that the PDU will be freed in all cases

*)
val snmp_sess_synch_response
 :  t
 -> Pdu.t
 -> ( Oid.t * ASN1_value.t ) list
