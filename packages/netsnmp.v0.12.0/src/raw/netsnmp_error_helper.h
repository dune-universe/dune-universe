#ifndef __NETSNMP_ERROR_HELPER_H
#define __NETSNMP_ERROR_HELPER_H

int snmperr_to_ml_variant(int err);
int snmp_pdu_error_status_to_ml_variant(int err);

#endif /* __NETSNMP_ERROR_HELPER_H */
