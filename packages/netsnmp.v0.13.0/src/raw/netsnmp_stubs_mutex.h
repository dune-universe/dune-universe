#ifndef _INCLUDED_NETSNMP_STUBS_MUTEX
#define _INCLUDED_NETSNMP_STUBS_MUTEX

#include <net-snmp/net-snmp-config.h>
#include <net-snmp/mib_api.h>
#include <net-snmp/pdu_api.h>
#include <net-snmp/session_api.h>

/** Netsnmp_stubs_mutex provides a thread safe low level interface to net-snmp's API.
 *  The functions guard library calls that arn't thread safe with a mutex.
 *  For more complete documentation see the Net-SNMP manual pages for the 
 *  function without the _mutex suffix */

/** Mib functions. See the the add_mibdir(3) man page unless otherwise noted */

/** [netsnmp_init_mib_mutex] initialises the net-snmp mib library and must be called before
    {i most} MIB functions, see per function documentation for exceptions.
    More information can be found in the net-snmp manual page: init_mib(3) */
void netsnmp_init_mib_mutex(void);

/** [shutdown_mib_mutex] cleans up and free memory assocated with the net-snmp mib library.
    Don't call any MIB functions afterwards. See shutdown_mib(3) for more details.  */
void shutdown_mib_mutex(void);

/** [add_mibdir_mutex] adds a directory to the list of directories to search when loading
    MIB modules.  This must be called {i after} [netsnmp_init_mib_mutex] even though the man
    page says the opposite. Raises [Netsnmp_exceptions.Not_found_mutex] if the directory
    is missing.  See add_mibdir(3) for more details */
int add_mibdir_mutex(const char *);

/** The netsnmp [netsnmp_read_module_mutex] and read_mib functions do not provide reliable
    error reporting, sigh.  See netsnmp_read_module(3), read_mib(3) and read_all_mibs(3)
    for more details */
struct tree *netsnmp_read_module_mutex(const char *);
struct tree *read_mib_mutex(const char *);
struct tree *read_all_mibs_mutex(void);

/** [add_module_replacement_mutex].  See add_module_replacement(3) for more details */
void add_module_replacement_mutex(const char *, const char *, const char *, int);

/** [snmp_set_mib_errors_mutex] and [snmp_set_mib_warnings] set the error and warning
    level. Note that the netsnmp libraries have a bad habit of outputing errors
    even when these are set to 0. See snmp_set_mib_errors(3) and snmp_set_mib_warnings(3)
    for more details
 */
void snmp_set_mib_warnings_mutex(int);
void snmp_set_mib_errors_mutex(int);

/** [snmp_set_save_descriptions_mutex] causes the descriptions in the MIB modules to
    be loaded as well. This {i must} be called before [netsnmp_init_mib_mutex].
    This function influences the output of the print functions as well as
    the snprint_* ones.  See snmp_set_save_descriptions(3) for more details */
void snmp_set_save_descriptions_mutex(int);

/** [read_objid_mutex] raises an [Netsnmp_exceptions.Not_found] exception on failure.
    See read_objid(3) for more details */
int read_objid_mutex(const char *, oid *, size_t *);
oid *snmp_parse_oid_mutex(const char *, oid *, size_t *);

/** [get_module_node_mutex] raises an [Netsnmp_exceptions.Not_found] exception on failure
    See get_module_node(3) for more details */
int get_module_node_mutex(const char *, const char *, oid *, size_t *);

/** [get_node_mutex] raises an [Netsnmp_exceptions.Not_found] exception on failure.
    See get_module_node(3) for more details */
int get_node_mutex(const char *, oid *, size_t *);

/** [print_mib_mutex] takes a file descriptor unlike the C API, flush any output
    before calling these functions to avoid output ordering issues.
    See print_mib(3) for more details
*/
void print_mib_mutex(FILE * fp);
/** [fprint_objid_mutex] takes a file descriptor unlike the C API.
    See fprint_objid(3) for more details */

void fprint_objid_mutex(FILE * fp, const oid * objid, size_t objidlen);
/** [snprint_objid_mutex] converts a Oid.t to the textual representation. See snprint_objid(3) for more details
*/
int snprint_objid_mutex(char *buf, size_t buf_len, const oid * objid, size_t objidlen);


/* PDU related */

/* [snprint_description_mutex] converts a Oid.t to the textual representation including the
   additional information from the mib entry.  See snprint_description(3) for more details */
int snprint_description_mutex(char *buf, size_t buf_len, oid * objid, size_t objidlen, int width);


/** [snmp_pdu_create_mutex] creates a PDU of the specified type, the associated
    memory is not part of the OCaml heap. */
netsnmp_pdu *snmp_pdu_create_mutex(int type);
void snmp_free_pdu_mutex(netsnmp_pdu *pdu);

netsnmp_pdu *snmp_clone_pdu_mutex(netsnmp_pdu *pdu);

/** [snmp_add_null_var_mutex] adds an oid to a pdu with a null value.
    Used to build a request PDU */
netsnmp_variable_list *snmp_add_null_var_mutex(netsnmp_pdu *pdu, const oid * name, size_t name_length);

/** [snmp_add_variable_mutex] adds an oid and value to a pdu
    Used to build a request PDU */
netsnmp_variable_list *snmp_pdu_add_variable_mutex(netsnmp_pdu *pdu,
  const oid *name, size_t name_length, u_char type, const void *value, size_t len);


/* Session related */

/** [snmp_sess_init_mutex] - create a netsnmp session ready to open. This function is
    not thread safe as it loads and parses mibs especially on the first call.
    The value returned can be discarded once used to open the session with
    snmp_sess_open.  */
void snmp_sess_init_mutex(netsnmp_session *);

/** [snmp_sess_open_mutex] creates a session between the client and host and returns a handle. */
void *snmp_sess_open_mt(netsnmp_session *);

/** [snmp_sess_close_mutex] shuts down the connection and frees resources.  */
int snmp_sess_close_mt(void *);

/** snmp_sess_synch_response_mutex t pdu] sends a pdu and returns the response.
    Note that this is completely synchronous. */
int snmp_sess_synch_response_mt(void *, netsnmp_pdu *, netsnmp_pdu **);
#endif

