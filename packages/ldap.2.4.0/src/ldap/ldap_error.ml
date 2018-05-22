open Ldap_types

let err2string code =
  match code with
      `SUCCESS -> "`SUCCESS"
    | `OPERATIONS_ERROR -> "`OPERATIONS_ERROR"
    | `PROTOCOL_ERROR -> "`PROTOCOL_ERROR"
    | `TIMELIMIT_EXCEEDED -> "`TIMELIMIT_EXCEEDED"
    | `SIZELIMIT_EXCEEDED -> "`SIZELIMIT_EXCEEDED"
    | `COMPARE_FALSE -> "`COMPARE_FALSE"
    | `COMPARE_TRUE -> "`COMPARE_TRUE"
    | `AUTH_METHOD_NOT_SUPPORTED -> "`AUTH_METHOD_NOT_SUPPORTED"
    | `STRONG_AUTH_REQUIRED -> "`STRONG_AUTH_REQUIRED"
    | `REFERRAL -> "`REFERRAL"
    | `ADMINLIMIT_EXCEEDED -> "`ADMINLIMIT_EXCEEDED"
    | `UNAVAILABLE_CRITICAL_EXTENSION -> "`UNAVAILABLE_CRITICAL_EXTENSION"
    | `CONFIDENTIALITY_REQUIRED -> "`CONFIDENTIALITY_REQUIRED"
    | `SASL_BIND_IN_PROGRESS -> "`SASL_BIND_IN_PROGRESS"
    | `NO_SUCH_ATTRIBUTE -> "`NO_SUCH_ATTRIBUTE"
    | `UNDEFINED_TYPE -> "`UNDEFINED_TYPE"
    | `INAPPROPRIATE_MATCHING -> "`INAPPROPRIATE_MATCHING"
    | `CONSTRAINT_VIOLATION -> "`CONSTRAINT_VIOLATION"
    | `TYPE_OR_VALUE_EXISTS -> "`TYPE_OR_VALUE_EXISTS"
    | `INVALID_SYNTAX -> "`INVALID_SYNTAX"
    | `NO_SUCH_OBJECT -> "`NO_SUCH_OBJECT"
    | `ALIAS_PROBLEM -> "`ALIAS_PROBLEM"
    | `INVALID_DN_SYNTAX -> "`INVALID_DN_SYNTAX"
    | `ALIAS_DEREF_PROBLEM -> "`ALIAS_DEREF_PROBLEM"
    | `INAPPROPRIATE_AUTH -> "`INAPPROPRIATE_AUTH"
    | `INVALID_CREDENTIALS -> "`INVALID_CREDENTIALS"
    | `INSUFFICIENT_ACCESS -> "`INSUFFICIENT_ACCESS"
    | `BUSY -> "`BUSY"
    | `UNAVAILABLE -> "`UNAVAILABLE"
    | `UNWILLING_TO_PERFORM -> "`UNWILLING_TO_PERFORM"
    | `LOOP_DETECT -> "`LOOP_DETECT"
    | `NAMING_VIOLATION -> "`NAMING_VIOLATION"
    | `OBJECT_CLASS_VIOLATION -> "`OBJECT_CLASS_VIOLATION"
    | `NOT_ALLOWED_ON_NONLEAF -> "`NOT_ALLOWED_ON_NONLEAF"
    | `NOT_ALLOWED_ON_RDN -> "`NOT_ALLOWED_ON_RDN"
    | `ALREADY_EXISTS -> "`ALREADY_EXISTS"
    | `NO_OBJECT_CLASS_MODS -> "`NO_OBJECT_CLASS_MODS"
    | `LOCAL_ERROR -> "`LOCAL_ERROR"
    | `SERVER_DOWN -> "`SERVER_DOWN"
    | `OTHER -> "`OTHER"
    | _ -> raise (LDAP_Decoder "invalid error code")

let ldap_strerror msg ldaperror =
  match ldaperror with
      LDAP_Failure (code, error, {ext_matched_dn=mdn;ext_referral=refs}) ->
        "LDAP_Failure (" ^
        (String.concat ", "
           [(err2string code);
            "\"" ^ (String.concat ": "
                      (List.filter
                         (function "" -> false | _ -> true)
                         [error; msg])) ^ "\"";
            "{ext_matched_dn = " ^ "\"" ^ mdn ^ "\"; ext_referral = " ^
              (match refs with
                   Some lst -> "[" ^ (String.concat "; " lst) ^ "]"
                 | None -> "None") ^ "})"])
    | _ -> failwith "not an ldap error"

let ldap_perror error ldaperror =
  prerr_endline (ldap_strerror error ldaperror)
