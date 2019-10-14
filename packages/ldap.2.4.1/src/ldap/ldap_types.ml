(* Common data types from rfc 2251 used throughout the library

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)

(** Common data types used by ocamldap. Most of these types are taken
  from the ASN.1 specification for LDAP as defined in rfc2251 @see
  <http://www.ietf.org/rfc/rfc2251.txt> rfc2251*)

(** An encoding error has occurred, the argument contains a
  description of the error This is likely a bug, so it should be
  reported *)
exception LDAP_Encoder of string

(** A decoding error has occurred, the argument contains a description
  of the error. This MAY be a bug, but it may also be that the server
  you are talking to is non standard. Please report these right away in
  any case.*)
exception LDAP_Decoder of string

type ldap_resultcode = [
    `SUCCESS
  | `OPERATIONS_ERROR
  | `PROTOCOL_ERROR
  | `TIMELIMIT_EXCEEDED
  | `SIZELIMIT_EXCEEDED
  | `COMPARE_FALSE
  | `COMPARE_TRUE
  | `AUTH_METHOD_NOT_SUPPORTED
  | `STRONG_AUTH_REQUIRED
  | `REFERRAL
  | `ADMINLIMIT_EXCEEDED
  | `UNAVAILABLE_CRITICAL_EXTENSION
  | `CONFIDENTIALITY_REQUIRED
  | `SASL_BIND_IN_PROGRESS
  | `NO_SUCH_ATTRIBUTE
  | `UNDEFINED_TYPE
  | `INAPPROPRIATE_MATCHING
  | `CONSTRAINT_VIOLATION
  | `TYPE_OR_VALUE_EXISTS
  | `INVALID_SYNTAX
  | `NO_SUCH_OBJECT
  | `ALIAS_PROBLEM
  | `INVALID_DN_SYNTAX
  | `IS_LEAF
  | `ALIAS_DEREF_PROBLEM
  | `INAPPROPRIATE_AUTH
  | `INVALID_CREDENTIALS
  | `INSUFFICIENT_ACCESS
  | `BUSY
  | `UNAVAILABLE
  | `UNWILLING_TO_PERFORM
  | `LOOP_DETECT
  | `NAMING_VIOLATION
  | `OBJECT_CLASS_VIOLATION
  | `NOT_ALLOWED_ON_NONLEAF
  | `NOT_ALLOWED_ON_RDN
  | `ALREADY_EXISTS
  | `NO_OBJECT_CLASS_MODS
  | `AFFECTS_MULTIPLE_DSAS
  | `OTHER
  | `SERVER_DOWN
  | `LOCAL_ERROR
  | `ENCODING_ERROR
  | `DECODING_ERROR
  | `TIMEOUT
  | `AUTH_UNKNOWN
  | `FILTER_ERROR
  | `USER_CANCELLED
  | `PARAM_ERROR
  | `NO_MEMORY
  | `CONNECT_ERROR
  | `NOT_SUPPORTED
  | `CONTROL_NOT_FOUND
  | `NO_RESULTS_RETURNED
  | `MORE_RESULTS_TO_RETURN
  | `CLIENT_LOOP
  | `REFERRAL_LIMIT_EXCEEDED
  | `UNKNOWN_ERROR of int ]

type ldap_result = {
  result_code: ldap_resultcode;
  matched_dn: string;
  error_message: string;
  ldap_referral: (string list) option;
}

(** extended information to return with the LDAP_Failure
  exception. Contains the remaining values which are defined by the
  protocol ext_matched_dn: the matched dn. Commonly set by
  `NO_SUCH_OBJECT. ext_referral: a list of ldapurls returned by the
  server when you attempted to do a write operation. If you use
  Ldap_ooclient with referrals set to follow you will never see this*)
type ldap_ext_return = {
  ext_matched_dn: string;
  ext_referral: string list option;
}

(** The exception raised to indicate all types of failure in the
  higher level libraries Ldap_funclient, and Ldap_ooclient. example
  [LDAP_Failure (`NO_SUCH_OBJECT, "no such object",
  {ext_matched_dn=Some "o=csun";ext_referral=None})] *)
exception LDAP_Failure of ldap_resultcode * string * ldap_ext_return

type saslCredentials = {
  sasl_mechanism: string;
  sasl_credentials: string option;
}

type authentication = Simple of string
                      | Sasl of saslCredentials

type bind_request = {
  bind_version: int;
  bind_name: string;
  bind_authentication: authentication;
}

type bind_response = {
  bind_result: ldap_result;
  bind_serverSaslCredentials: string option;
}

type attribute = {
  attr_type: string;
  attr_vals: string list;
}

type dn = attribute list

(** the type used to encode and decode a search entry. Also the type
  returned by search_s and search_a in Ldap_funclient *)
type search_result_entry = {
  sr_dn: string;
  sr_attributes: attribute list;
}

(** a type defining the scope of a search filter *)
type search_scope = [ `BASE (** search only at the base *)
                    | `ONELEVEL (** search one level below the base *)
                    | `SUBTREE (** search the entire tree under the base *)]

type alias_deref = [ `NEVERDEREFALIASES
                   | `DEREFINSEARCHING
                   | `DEREFFINDINGBASE
                   | `DEREFALWAYS ]

type attribute_value_assertion = {
  attributeDesc: string;
  assertionValue: string;
}

type matching_rule_assertion = {
  matchingRule: string option;
  ruletype: string option;
  matchValue: string;
  dnAttributes: bool; (* default false *)
}

type substring_component = { (* at least one must be specified *)
  substr_initial: string list;
  substr_any: string list;
  substr_final: string list;
}

type substring_filter = {
  attrtype: string;
  substrings: substring_component;
}

type filter = [ `And of filter list
              | `Or of filter list
              | `Not of filter
              | `EqualityMatch of attribute_value_assertion
              | `Substrings of substring_filter
              | `GreaterOrEqual of attribute_value_assertion
              | `LessOrEqual of attribute_value_assertion
              | `Present of string
              | `ApproxMatch of attribute_value_assertion
              | `ExtensibleMatch of matching_rule_assertion ]

type search_request = {
  baseObject: string;
  scope: search_scope;
  derefAliases: alias_deref;
  sizeLimit: int32;
  timeLimit: int32;
  typesOnly: bool;
  filter: filter;
  s_attributes: string list;
}

type modify_optype = [ `ADD
                     | `DELETE
                     | `REPLACE ]

type modify_op = {
  mod_op: modify_optype;
  mod_value: attribute;
}

type modify_request = {
  mod_dn: string;
  modification: modify_op list
}

type modify_dn_request = {
  modn_dn: string;
  modn_newrdn: string;
  modn_deleteoldrdn: bool;
  modn_newSuperior: string option
}

type compare_request = {
  cmp_dn: string;
  cmp_ava: attribute_value_assertion;
}

type extended_request = {
  ext_requestName: string;
  ext_requestValue: string option;
}

type extended_response = {
  ext_result: ldap_result;
  ext_responseName: string option;
  ext_response: string option;
}

type protocol_op = Bind_request of bind_request
                   | Bind_response of bind_response
                   | Unbind_request
                   | Search_request of search_request
                   | Search_result_entry of search_result_entry
                   | Search_result_reference of string list
                   | Search_result_done of ldap_result
                   | Modify_request of modify_request
                   | Modify_response of ldap_result
                   | Add_request of search_result_entry
                   | Add_response of ldap_result
                   | Delete_request of string
                   | Delete_response of ldap_result
                   | Modify_dn_request of modify_dn_request
                   | Modify_dn_response of ldap_result
                   | Compare_request of compare_request
                   | Compare_response of ldap_result
                   | Abandon_request of Int32.t
                   | Extended_request of extended_request
                   | Extended_response of extended_response

type paged_results_control_value = {
  size: int;
  cookie: string;
}

type control_details =
  [`Paged_results_control of paged_results_control_value
  |`Unknown_value of string ]

type ldap_control = {
  criticality: bool;
  control_details: control_details;
}

type ldap_controls = ldap_control list

type ldap_message = {
  messageID: Int32.t;
  protocolOp: protocol_op;
  controls: ldap_controls option;
}

type con_mech = [ `SSL
                | `PLAIN ]

type ldap_url = {
  url_mech: con_mech;
  url_host: string option;
  url_port: string option;
  url_dn: string option;
  url_attributes: (string list) option;
  url_scope: search_scope option;
  url_filter: filter option;
  url_ext: ((bool * string * string) list) option;
}

(** see draft-zeilenga-ldap-grouping-xx Ldap grouping is a way of
    telling the server that a set of ldap operations is related, its most
    interesting application is transactions across multiple objects.
    This draft is not yet implemented by any present day ldap server *)
type ldap_grouping_type = [ `LDAP_GROUP_TXN ]

(** a cookie that is sent with every ldap operation which is part of a
    group *)
type ldap_grouping_cookie
