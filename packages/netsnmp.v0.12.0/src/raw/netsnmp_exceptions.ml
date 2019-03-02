exception Not_found of string
exception System_error of string
exception Out_of_memory
exception Response_error of (Pdu_error.t * string)
exception General_error of (int * Netsnmp_error.t * string) (* os_err, snmp_error, error_string *)
exception Request_timeout

let () =
  Callback.register_exception "Netsnmp_error_not_found" (Not_found "");
  Callback.register_exception "Netsnmp_error_system" (System_error "");
  Callback.register_exception "Netsnmp_out_of_memory" Out_of_memory;
  Callback.register_exception "Netsnmp_response_error"
    (Response_error (Pdu_error.SNMP_ERR_NOERROR, ""));
  Callback.register_exception "Netsnmp_general_error"
    (General_error (0, Netsnmp_error.SNMPERR_SUCCESS, ""));
  Callback.register_exception "Netsnmp_request_timeout" Request_timeout
