#include <net-snmp/net-snmp-config.h>
#include <net-snmp/mib_api.h>
#include <net-snmp/pdu_api.h>
#include <net-snmp/session_api.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <pthread.h>
#include "netsnmp_stubs_mutex.h"

/** This file interfaces to the underlying library with *_mutex or _mt versions of
 *  the library functions.  A single mutex is used to prevent multiple threads
 *  from accessing functions that are not thread safe (*_mutex versions). The *_mt
 *  versions are thread safe and do not assert the lock
 *
 *  See http://net-snmp.sourceforge.net/docs/README.thread.html for more details
 */

static pthread_mutex_t lib_lock = PTHREAD_MUTEX_INITIALIZER;

static void netsnmp_stubs_mutex_lock(void)
{
  pthread_mutex_lock(&lib_lock);
}

static void netsnmp_stubs_mutex_unlock(void)
{
  pthread_mutex_unlock(&lib_lock);
}

void netsnmp_init_mib_mutex(void)
{
  netsnmp_stubs_mutex_lock();
  netsnmp_init_mib();
  netsnmp_stubs_mutex_unlock();
}

void shutdown_mib_mutex(void)
{
  netsnmp_stubs_mutex_lock();
  shutdown_mib();
  netsnmp_stubs_mutex_unlock();
}

int add_mibdir_mutex(const char *path)
{
  int res;

  netsnmp_stubs_mutex_lock();
  res = add_mibdir(path);
  netsnmp_stubs_mutex_unlock();
  return res;
}

struct tree *netsnmp_read_module_mutex(const char *name)
{
  struct tree *res;

  netsnmp_stubs_mutex_lock();
  res = netsnmp_read_module(name);
  netsnmp_stubs_mutex_unlock();
  return res;
}

struct tree *read_mib_mutex(const char *filename)
{
  struct tree *res;

  netsnmp_stubs_mutex_lock();
  res = read_mib(filename);
  netsnmp_stubs_mutex_unlock();
  return res;
}

struct tree *read_all_mibs_mutex(void)
{
  struct tree *res;

  netsnmp_stubs_mutex_lock();
  res = read_all_mibs();
  netsnmp_stubs_mutex_unlock();
  return res;
}

void add_module_replacement_mutex(const char *old_module, const char *new_module, const char *tag, int len)
{
  netsnmp_stubs_mutex_lock();
  add_module_replacement(old_module, new_module, tag, len);
  netsnmp_stubs_mutex_unlock();
}

void snmp_set_mib_warnings_mutex(int level)
{
  netsnmp_stubs_mutex_lock();
  snmp_set_mib_warnings(level);
  netsnmp_stubs_mutex_unlock();
}

void snmp_set_save_descriptions_mutex(int save)
{
  netsnmp_stubs_mutex_lock();
  snmp_set_save_descriptions(save);
  netsnmp_stubs_mutex_unlock();
}

void snmp_set_mib_errors_mutex(int level)
{
  netsnmp_stubs_mutex_lock();
  snmp_set_mib_errors(level);
  netsnmp_stubs_mutex_unlock();
}

int read_objid_mutex(const char *input, oid *objid, size_t *objidlen)
{
  int res;

  netsnmp_stubs_mutex_lock();
  res = read_objid(input, objid, objidlen);
  netsnmp_stubs_mutex_unlock();
  return res;
}

oid *snmp_parse_oid_mutex(const char *input, oid *objid, size_t *objidlen)
{
  oid *res;

  netsnmp_stubs_mutex_lock();
  res = snmp_parse_oid(input, objid, objidlen);
  netsnmp_stubs_mutex_unlock();
  return res;
}

int get_module_node_mutex(const char *name, const char *module, oid *objid, size_t *objidlen)
{
  int res;

  netsnmp_stubs_mutex_lock();
  res = get_module_node(name, module, objid, objidlen);
  netsnmp_stubs_mutex_unlock();
  return res;
}

int get_node_mutex(const char *name, oid *objid, size_t *objidlen)
{
  int res;

  netsnmp_stubs_mutex_lock();
  res = get_node(name, objid, objidlen);
  netsnmp_stubs_mutex_unlock();
  return res;
}

void print_mib_mutex(FILE * fp)
{
  netsnmp_stubs_mutex_lock();
  print_mib(fp);
  netsnmp_stubs_mutex_unlock();
}

void fprint_objid_mutex(FILE * fp, const oid * objid, size_t objidlen)
{
  netsnmp_stubs_mutex_lock();
  fprint_objid(fp, objid, objidlen);
  netsnmp_stubs_mutex_unlock();
}

int snprint_objid_mutex(char *buf, size_t buf_len, const oid * objid, size_t objidlen)
{
  int res;

  netsnmp_stubs_mutex_lock();
  res = snprint_objid(buf, buf_len, objid, objidlen);
  netsnmp_stubs_mutex_unlock();
  return res;
}

int snprint_description_mutex(char *buf, size_t buf_len, oid * objid, size_t objidlen, int width)
{
  int res;

  netsnmp_stubs_mutex_lock();
  res = snprint_description(buf, buf_len, objid, objidlen, width);
  netsnmp_stubs_mutex_unlock();
  return res;
}

netsnmp_pdu *snmp_pdu_create_mutex(int type)
{
  netsnmp_pdu *res;

  netsnmp_stubs_mutex_lock();
  res = snmp_pdu_create(type);
  netsnmp_stubs_mutex_unlock();
  return res;
}

void snmp_free_pdu_mutex(struct snmp_pdu *pdu)
{
  netsnmp_stubs_mutex_lock();
  snmp_free_pdu(pdu);
  netsnmp_stubs_mutex_unlock();
}

netsnmp_pdu *snmp_clone_pdu_mutex(netsnmp_pdu *pdu)
{
  netsnmp_pdu *res;

  netsnmp_stubs_mutex_lock();
  res = snmp_clone_pdu(pdu);
  netsnmp_stubs_mutex_unlock();
  return res;
}

netsnmp_variable_list *snmp_add_null_var_mutex(netsnmp_pdu *pdu, const oid * name, size_t name_length)
{
  netsnmp_variable_list *res;
  
  netsnmp_stubs_mutex_lock();
  res = snmp_add_null_var(pdu, name, name_length);
  netsnmp_stubs_mutex_unlock();
  return res;
}

netsnmp_variable_list *snmp_pdu_add_variable_mutex(
  netsnmp_pdu *pdu, const oid *name, size_t name_length, u_char type, const void *value, size_t len)
{
  netsnmp_variable_list *res;
  
  netsnmp_stubs_mutex_lock();
  res = snmp_pdu_add_variable(pdu, name, name_length, type, value, len);
  netsnmp_stubs_mutex_unlock();
  return res;
}

void snmp_sess_init_mutex(netsnmp_session *session)
{
  netsnmp_stubs_mutex_lock();
  snmp_sess_init(session);
  netsnmp_stubs_mutex_unlock();
}

void *snmp_sess_open_mt(netsnmp_session *session)
{
  void *res;

  /* This is thread safe */
  res = snmp_sess_open(session);
  return res;
}

int snmp_sess_close_mt(void *handle)
{
  int res;

  /* This is thread safe */
  res = snmp_sess_close(handle);
  return res;
}

int snmp_sess_synch_response_mt(void *handle, netsnmp_pdu *pdu, netsnmp_pdu **response)
{
  int res;

  /* This is thread safe */
  res = snmp_sess_synch_response(handle, pdu, response);
  return res;
}
