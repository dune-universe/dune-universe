open Ldap_types

(** given an ldap error code return a string describing it *)
val err2string :
  [> `ADMINLIMIT_EXCEEDED
   | `ALIAS_DEREF_PROBLEM
   | `ALIAS_PROBLEM
   | `ALREADY_EXISTS
   | `AUTH_METHOD_NOT_SUPPORTED
   | `BUSY
   | `COMPARE_FALSE
   | `COMPARE_TRUE
   | `CONFIDENTIALITY_REQUIRED
   | `CONSTRAINT_VIOLATION
   | `INAPPROPRIATE_AUTH
   | `INAPPROPRIATE_MATCHING
   | `INSUFFICIENT_ACCESS
   | `INVALID_CREDENTIALS
   | `INVALID_DN_SYNTAX
   | `INVALID_SYNTAX
   | `LOCAL_ERROR
   | `LOOP_DETECT
   | `NAMING_VIOLATION
   | `NOT_ALLOWED_ON_NONLEAF
   | `NOT_ALLOWED_ON_RDN
   | `NO_OBJECT_CLASS_MODS
   | `NO_SUCH_ATTRIBUTE
   | `NO_SUCH_OBJECT
   | `OBJECT_CLASS_VIOLATION
   | `OPERATIONS_ERROR
   | `OTHER
   | `PROTOCOL_ERROR
   | `REFERRAL
   | `SASL_BIND_IN_PROGRESS
   | `SERVER_DOWN
   | `SIZELIMIT_EXCEEDED
   | `STRONG_AUTH_REQUIRED
   | `SUCCESS
   | `TIMELIMIT_EXCEEDED
   | `TYPE_OR_VALUE_EXISTS
   | `UNAVAILABLE
   | `UNAVAILABLE_CRITICAL_EXTENSION
   | `UNDEFINED_TYPE
   | `UNWILLING_TO_PERFORM ] ->
  string

(** return a string with a human readable description of an LDAP_Failure exception *)
val ldap_strerror : string -> exn -> string

(** print to stderr a string with a human readable description of an LDAP_Failure exception *)
val ldap_perror : string -> exn -> unit
