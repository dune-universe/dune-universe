#include <net-snmp/net-snmp-config.h>
#include <net-snmp/definitions.h>

int snmperr_to_ml_variant(int err)
{
  switch (err) {
    case SNMPERR_SUCCESS:                       return 0;
    case SNMPERR_GENERR:                        return 1;
    case SNMPERR_BAD_LOCPORT:                   return 2;
    case SNMPERR_BAD_ADDRESS:                   return 3;
    case SNMPERR_BAD_SESSION:                   return 4;
    case SNMPERR_TOO_LONG:                      return 5;
    case SNMPERR_NO_SOCKET:                     return 6;
    case SNMPERR_V2_IN_V1:                      return 7;
    case SNMPERR_V1_IN_V2:                      return 8;
    case SNMPERR_BAD_REPEATERS:                 return 9;
    case SNMPERR_BAD_REPETITIONS:               return 10;
    case SNMPERR_BAD_ASN1_BUILD:                return 11;
    case SNMPERR_BAD_SENDTO:                    return 12;
    case SNMPERR_BAD_PARSE:                     return 13;
    case SNMPERR_BAD_VERSION:                   return 14;
    case SNMPERR_BAD_SRC_PARTY:                 return 15;
    case SNMPERR_BAD_DST_PARTY:                 return 16;
    case SNMPERR_BAD_CONTEXT:                   return 17;
    case SNMPERR_BAD_COMMUNITY:                 return 18;
    case SNMPERR_NOAUTH_DESPRIV:                return 19;
    case SNMPERR_BAD_ACL:                       return 20;
    case SNMPERR_BAD_PARTY:                     return 21;
    case SNMPERR_ABORT:                         return 22;
    case SNMPERR_UNKNOWN_PDU:                   return 23;
    case SNMPERR_TIMEOUT:                       return 24;
    case SNMPERR_BAD_RECVFROM:                  return 25;
    case SNMPERR_BAD_ENG_ID:                    return 26;
    case SNMPERR_BAD_SEC_NAME:                  return 27;
    case SNMPERR_BAD_SEC_LEVEL:                 return 28;
    case SNMPERR_ASN_PARSE_ERR:                 return 29;
    case SNMPERR_UNKNOWN_SEC_MODEL:             return 30;
    case SNMPERR_INVALID_MSG:                   return 31;
    case SNMPERR_UNKNOWN_ENG_ID:                return 32;
    case SNMPERR_UNKNOWN_USER_NAME:             return 33;
    case SNMPERR_UNSUPPORTED_SEC_LEVEL:         return 34;
    case SNMPERR_AUTHENTICATION_FAILURE:        return 35;
    case SNMPERR_NOT_IN_TIME_WINDOW:            return 36;
    case SNMPERR_DECRYPTION_ERR:                return 37;
    case SNMPERR_SC_GENERAL_FAILURE:            return 38;
    case SNMPERR_SC_NOT_CONFIGURED:             return 39;
    case SNMPERR_KT_NOT_AVAILABLE:              return 40;
    case SNMPERR_UNKNOWN_REPORT:                return 41;
    case SNMPERR_USM_GENERICERROR:              return 42;
    case SNMPERR_USM_UNKNOWNSECURITYNAME:       return 43;
    case SNMPERR_USM_UNSUPPORTEDSECURITYLEVEL:  return 44;
    case SNMPERR_USM_ENCRYPTIONERROR:           return 45;
    case SNMPERR_USM_AUTHENTICATIONFAILURE:     return 46;
    case SNMPERR_USM_PARSEERROR:                return 47;
    case SNMPERR_USM_UNKNOWNENGINEID:           return 48;
    case SNMPERR_USM_NOTINTIMEWINDOW:           return 49;
    case SNMPERR_USM_DECRYPTIONERROR:           return 50;
    case SNMPERR_NOMIB:                         return 51;
    case SNMPERR_RANGE:                         return 52;
    case SNMPERR_MAX_SUBID:                     return 53;
    case SNMPERR_BAD_SUBID:                     return 54;
    case SNMPERR_LONG_OID:                      return 55;
    case SNMPERR_BAD_NAME:                      return 56;
    case SNMPERR_VALUE:                         return 57;
    case SNMPERR_UNKNOWN_OBJID:                 return 58;
    case SNMPERR_NULL_PDU:                      return 59;
    case SNMPERR_NO_VARS:                       return 60;
    case SNMPERR_VAR_TYPE:                      return 61;
    case SNMPERR_MALLOC:                        return 62;
    case SNMPERR_KRB5:                          return 63;
    case SNMPERR_PROTOCOL:                      return 64;
    case SNMPERR_OID_NONINCREASING:             return 65;
    case SNMPERR_JUST_A_CONTEXT_PROBE:          return 66;
    default:                                    return 1;
  }
}

int snmp_pdu_error_status_to_ml_variant(int err)
{
  switch (err) {
    case SNMP_ERR_NOERROR:                       return 0;
    case SNMP_ERR_TOOBIG:                        return 1;
    case SNMP_ERR_NOSUCHNAME:                    return 2;
    case SNMP_ERR_BADVALUE:                      return 3;
    case SNMP_ERR_READONLY:                      return 4;
    case SNMP_ERR_GENERR:                        return 5;
    case SNMP_ERR_NOACCESS:                      return 6;
    case SNMP_ERR_WRONGTYPE:                     return 7;
    case SNMP_ERR_WRONGLENGTH:                   return 8;
    case SNMP_ERR_WRONGENCODING:                 return 9;
    case SNMP_ERR_WRONGVALUE:                    return 10;
    case SNMP_ERR_NOCREATION:                    return 11;
    case SNMP_ERR_INCONSISTENTVALUE:             return 12;
    case SNMP_ERR_RESOURCEUNAVAILABLE:           return 13;
    case SNMP_ERR_COMMITFAILED:                  return 14;
    case SNMP_ERR_UNDOFAILED:                    return 15;
    case SNMP_ERR_AUTHORIZATIONERROR:            return 16;
    case SNMP_ERR_NOTWRITABLE:                   return 17;
    default:                                     return 5;
  }
}
