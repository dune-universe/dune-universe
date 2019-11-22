(** exceptions raised by the net-snmp interface library
 *)
exception Not_found of string
exception System_error of string
exception Out_of_memory
exception Response_error of (Pdu_error.t * string)

(** General_error (os_err, snmp_error, error_string)
    [os_error] is the value of the system errno and [error_string] a string
    interpretation of both [os_err] and [snmp_error] *)
exception General_error of (int * Netsnmp_error.t * string) (* os_err, snmp_error, error_string *)
exception Request_timeout
