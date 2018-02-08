/* File: gammu_stubs.c

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     Noémie Meunier <Noemie.Meunier@student.umons.ac.be>
     Pierre Hauweele <Pierre.Hauweele@student.umons.ac.be>

     WWW: http://math.umons.ac.be/an/software/

   This library is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details. */


#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/signals.h>

#include <gammu.h>

#include "gammu_stubs.h"
#include "io.h"


/************************************************************************/
/* Init */

CAMLexport
void caml_gammu_init(value vunit)
{
  /* Check for versions consistency between build time and runtime. */
  if (strcmp(GetGammuVersion(), GAMMU_VERSION)) {
    char msg[128];
    if (snprintf(msg, 128, "Gammu: used version of Gammu (%s) does not " \
                 "match the version of the OCaml bindings (%s).",         \
                 GetGammuVersion(), GAMMU_VERSION) < 0)
      caml_failwith("Gammu: versions inconsistency.");
    else
      caml_failwith(msg);
  }

  /* noalloc */
  global_debug = GSM_GetGlobalDebug();
  /* Initialize gettext. */
  GSM_InitLocales(NULL);
}


/************************************************************************/
/* Utils functions and macros. */

char *dup_String_val(value v)
{
  char *ret;

  ret = strdup(String_val(v));
  if (!ret)
    caml_raise_out_of_memory();

  return ret;
}

/* Currently unused.
static value option_val(value voption, gboolean *some)
{
  CAMLparam1(voption);
  if (Is_long(voption)) {
    *some = FALSE;
    return 0;
  }

  *some = TRUE;
  CAMLreturn(Field(voption, 1));
}
*/

static value val_Some(value vsome)
{
  CAMLparam1(vsome);
  CAMLlocal1(res);

  res = caml_alloc(1, 0);
  Store_field(res, 0, vsome);

  CAMLreturn(res);
}

#if GAMMU_VERSION_NUM < 12792
static gboolean is_true(const char *str)
{
  if (strcasecmp(str, "true") == 0) return TRUE;
  if (strcasecmp(str, "yes") == 0) return TRUE;
  if (strcasecmp(str, "1") == 0) return TRUE;
  return FALSE;
}

static char *yesno_bool(gboolean b)
{
  return (b) ? "yes" : "no";
}
#endif


/************************************************************************/
/* Error handling */

/* raise [Error] if the error code doesn't indicate no error. */
static void caml_gammu_raise_Error(int err)
{
  static value *exn = NULL;

  switch (err) {
  case ERR_NONE:
  case ERR_USING_DEFAULTS:
    /* only a warning, not fatal. */
    break;
  default:
    if (exn == NULL) {
      /* First time around, look up by name */
      exn = caml_named_value("Gammu.GSM_Error");
    }
    caml_raise_with_arg(*exn, VAL_GSM_ERROR(err));
  }
}

CAMLexport
value caml_gammu_GSM_ErrorString(value verr)
{
  CAMLparam1(verr);
  int err = GSM_ERROR_VAL(verr);
  const char *msg;
  switch (err) {
  case ERR_INI_KEY_NOT_FOUND:
    msg = "Pair section/value not found in INI file.";
    break;
  case ERR_COULD_NOT_DECODE:
    msg = "Decoding SMS Message failed.";
    break;
  case ERR_INVALID_CONFIG_NUM:
    msg = "Invalid config number.";
    break;
  default:
    if (err < ERR_LAST_VALUE)
      msg = GSM_ErrorString(err);
    else {
      /* Error defined in a newer version of Gammu */
      msg = "Error not defined in Gammu " GAMMU_VERSION;
    }
  }
  assert(msg != NULL);
  CAMLreturn(caml_copy_string(msg));
}


/************************************************************************/
/* Debugging handling */

/* A Debug.info is either the global_debug pointer or the associated state
   machine.

   So, to convert a value to a GSM_Debug_Info we first have to determine if it
   is the global_debug or the state machine associated. And for the later,
   access the debug info through the underlying state machine. */
static GSM_Debug_Info *GSM_Debug_Info_val(value vdi)
{
  if ((GSM_Debug_Info *) vdi == global_debug)
    return global_debug;
  return GSM_GetDebug(GSM_STATEMACHINE_VAL(vdi));
}

CAMLexport
value caml_gammu_GSM_GetGlobalDebug(value vunit)
{
  CAMLparam1(vunit);
  CAMLreturn(VAL_GSM_DEBUG_INFO(global_debug));
}

CAMLexport
value caml_gammu_GSM_SetDebugGlobal(value vdi, value vglobal)
{
  CAMLparam2(vdi, vglobal);
  GSM_SetDebugGlobal(Bool_val(vglobal), GSM_Debug_Info_val(vdi));
  CAMLreturn(Val_unit);
}

/* TODO: It should work on Windows, but have to check. */
static FILE *FILE_val(value vchannel, const char *mode)
{
  int fd;
  FILE *res;
  struct channel *channel = Channel(vchannel);
  assert(channel != NULL);

  /* Duplicate channel's file descriptor so that the user can close the
     channel without affecting us and inversely. */
  fd = dup(channel->fd);
  if (fd == -1)
    /* TODO: raise exception */
    return NULL;
  res = fdopen(fd, mode);

  return res;
}

CAMLexport
value caml_gammu_GSM_SetDebugFileDescriptor(value vdi, value vchannel)
{
  CAMLparam2(vdi, vchannel);
  GSM_Error error;
  error = GSM_SetDebugFileDescriptor(FILE_val(vchannel, "a"),
                                     TRUE, // file descr is closable
                                     GSM_Debug_Info_val(vdi));
  caml_gammu_raise_Error(error);
  CAMLreturn(Val_unit);
 }

CAMLexport
value caml_gammu_GSM_SetDebugLevel(value vdi, value vlevel)
{
  CAMLparam2(vdi, vlevel);

  if (!GSM_SetDebugLevel(String_val(vlevel), GSM_Debug_Info_val(vdi)))
    caml_invalid_argument("Gammu.set_debug_level: "             \
                          "invalid debug level identifier.");
  CAMLreturn(Val_unit);
}


/************************************************************************/
/* INI files */

static void caml_gammu_ini_section_finalize(value vini_section)
{
  SHOUT_DBG("Finalize INI Section.");
  INI_Free(INI_SECTION_VAL(vini_section));
}

static value alloc_INI_Section()
{
  CAMLparam0();
  CAMLlocal1(res);
  /* TODO: Can alloc_custom fail ? */
  res = alloc_custom(&caml_gammu_ini_section_ops, sizeof(INI_Section *),
                     1, 100);
  CAMLreturn(res);
}

static value Val_INI_Section(INI_Section *ini_section)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = alloc_INI_Section();
  INI_SECTION_VAL(res) = ini_section;

  CAMLreturn(res);
}

CAMLexport
value caml_gammu_INI_ReadFile(value vfilename, value vunicode)
{
  CAMLparam2(vfilename, vunicode);
  INI_Section *cfg;
  GSM_Error error;
  char *filename;
  gboolean Unicode = Bool_val(vunicode);

  filename = dup_String_val(vfilename);
  caml_enter_blocking_section(); /* release global lock */
  error = INI_ReadFile(filename, Unicode, &cfg);
  caml_leave_blocking_section(); /* acquire global lock */
  free(filename);
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_INI_Section(cfg));
}

CAMLexport
value caml_gammu_INI_GetValue(value vfile_info, value vsection, value vkey,
                              value vunicode)
{
  CAMLparam4(vfile_info, vsection, vkey, vunicode);
  CAMLlocal1(res);
  unsigned char *val;
  gboolean unicode = Bool_val(vunicode);

  val = INI_GetValue(INI_SECTION_VAL(vfile_info),
                     (unsigned char *) String_val(vsection),
                     (unsigned char *) String_val(vkey),
                     unicode);
  if (val == NULL)
    caml_gammu_raise_Error(ERR_INI_KEY_NOT_FOUND);

  if (unicode)
    res = CAML_COPY_USTRING(val);
  else
    res = caml_copy_string((char *) val);
  CAMLreturn(res);
}


/************************************************************************/
/* State machine */

static void caml_gammu_state_machine_finalize(value s)
{
  State_Machine *state_machine = STATE_MACHINE_VAL(s);

  GSM_FreeStateMachine(state_machine->sm);
  /* Allow GC to collect the callback closure value now. */
  if (state_machine->incoming_SMS_callback)
    caml_remove_global_root(&(state_machine->incoming_SMS_callback));
  if (state_machine->incoming_Call_callback)
    caml_remove_global_root(&(state_machine->incoming_Call_callback));

  free(state_machine);
}

static value Val_GSM_Config(const GSM_Config *config)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(14, 0);
  Store_field(res, 0, caml_copy_string(config->Model));
  Store_field(res, 1, caml_copy_string(config->DebugLevel));
  Store_field(res, 2, caml_copy_string(config->Device));
  Store_field(res, 3, caml_copy_string(config->Connection));
#if GAMMU_VERSION_NUM >= 12792
  Store_field(res, 4, Val_bool(config->SyncTime));
  Store_field(res, 5, Val_bool(config->LockDevice));
  Store_field(res, 7, Val_bool(config->StartInfo));
#else
  /* for GAMMU_VERSION_NUM <= 12400, those are strings. */
  Store_field(res, 4, Val_bool(is_true(config->SyncTime)));
  Store_field(res, 5, Val_bool(is_true(config->LockDevice)));
  Store_field(res, 7, Val_bool(is_true(config->StartInfo)));
#endif
  Store_field(res, 6, caml_copy_string(config->DebugFile));
  Store_field(res, 8, Val_bool(config->UseGlobalDebugFile));
  Store_field(res, 9, caml_copy_string(config->TextReminder));
  Store_field(res, 10, caml_copy_string(config->TextMeeting));
  Store_field(res, 11, caml_copy_string(config->TextCall));
  Store_field(res, 12, caml_copy_string(config->TextBirthday));
  Store_field(res, 13, caml_copy_string(config->TextMemo));
  /* (+) GSM_Features PhoneFeatures */

  CAMLreturn(res);
}

/* Set values of config according to those from vconfig. */
static void GSM_Config_val(GSM_Config *config, value vconfig)
{
  CPY_TRIM_STRING_VAL(config->Model, Field(vconfig, 0));
  CPY_TRIM_STRING_VAL(config->DebugLevel, Field(vconfig, 1));
  CPY_STRING_VAL(config->Device, Field(vconfig, 2));
  CPY_STRING_VAL(config->Connection, Field(vconfig, 3));
#if GAMMU_VERSION_NUM >= 12792
  config->SyncTime = Bool_val(Field(vconfig, 4));
  config->LockDevice = Bool_val(Field(vconfig, 5));
  config->StartInfo = Bool_val(Field(vconfig, 7));
#else
  /* for GAMMU_VERSION_NUM <= 12400, those are strings.
     we don't know about versions between 12400 and 12792 */
  config->SyncTime = yesno_bool(Bool_val(Field(vconfig, 4)));
  config->LockDevice = yesno_bool(Bool_val(Field(vconfig, 5)));
  config->StartInfo = yesno_bool(Bool_val(Field(vconfig, 7)));
#endif
  CPY_STRING_VAL(config->DebugFile, Field(vconfig, 6));
  config->UseGlobalDebugFile = Bool_val(Field(vconfig, 8));
  CPY_TRIM_STRING_VAL(config->TextReminder, Field(vconfig, 9));
  CPY_TRIM_STRING_VAL(config->TextMeeting, Field(vconfig, 10));
  CPY_TRIM_STRING_VAL(config->TextCall, Field(vconfig, 11));
  CPY_TRIM_STRING_VAL(config->TextBirthday, Field(vconfig, 12));
  CPY_TRIM_STRING_VAL(config->TextMemo, Field(vconfig, 13));
}

CAMLexport
value caml_gammu_GSM_GetDebug(value s)
{
  CAMLparam1(s);

  CAMLreturn(VAL_GSM_DEBUG_INFO(s));
}

CAMLexport
value caml_gammu_GSM_InitLocales(value vpath)
{
  CAMLparam1(vpath);

  GSM_InitLocales(String_val(vpath));

  CAMLreturn(Val_unit);
}

CAMLexport
value caml_gammu_GSM_InitDefaultLocales()
{
  CAMLparam0();

  GSM_InitLocales(NULL);

  CAMLreturn(Val_unit);
}

CAMLexport
value caml_gammu_GSM_AllocStateMachine(value vunit)
{
  CAMLparam1(vunit);
  CAMLlocal2(res, vsm);
  State_Machine *state_machine;
  GSM_StateMachine *sm;

  state_machine = malloc(sizeof(State_Machine));
  if(!state_machine)
    caml_raise_out_of_memory();
  sm = GSM_AllocStateMachine();
  if(!sm) {
    free(state_machine);
    caml_raise_out_of_memory();
  }

  state_machine->sm = sm;
  state_machine->log_function = 0;
  state_machine->incoming_SMS_callback = 0;
  state_machine->incoming_Call_callback = 0;

  res = alloc_custom(&caml_gammu_state_machine_ops,
                     sizeof(State_Machine *), 1, 100);
  STATE_MACHINE_VAL(res) = state_machine;

  CAMLreturn(res);
}

CAMLexport
value caml_gammu_GSM_FindGammuRC_force(value vpath)
{
  CAMLparam1(vpath);
  char *path;
  INI_Section *res;
  GSM_Error error;

  path = dup_String_val(vpath);
  caml_enter_blocking_section(); /* release global lock */
  error = GSM_FindGammuRC(&res, path);
  caml_leave_blocking_section(); /* acquire global lock */
  free(path);
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_INI_Section(res));
}

CAMLexport
value caml_gammu_GSM_FindGammuRC(value vunit)
{
  CAMLparam1(vunit);
  INI_Section *res;
  GSM_Error error;

  caml_enter_blocking_section();
  error = GSM_FindGammuRC(&res, NULL);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_INI_Section(res));
}

CAMLexport
value caml_gammu_GSM_ReadConfig(value vcfg_info, value vnum)
{
  CAMLparam2(vcfg_info, vnum);
  GSM_Config cfg;
  INI_Section *cfg_info;
  GSM_Error error;
  int num = Int_val(vnum);

  cfg_info = INI_SECTION_VAL(vcfg_info);
  /* Initialize those strings, because the first thing GSM_ReadConfig tries to
     do is freeing them. */
  cfg.Device = malloc(1);
  cfg.Connection = malloc(1);
  cfg.DebugFile = malloc(1);
#if GAMMU_VERSION_NUM < 12700
  cfg.SyncTime = malloc(1);
  cfg.LockDevice = malloc(1);
  cfg.StartInfo = malloc(1);
#endif

  caml_enter_blocking_section(); /* release global lock */
  error = GSM_ReadConfig(cfg_info, &cfg, num);
  caml_leave_blocking_section(); /* acquire global lock */
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_GSM_Config(&cfg));
}

CAMLexport
value caml_gammu_GSM_GetConfig(value s, value vnum)
{
  CAMLparam2(s, vnum);
  GSM_StateMachine *sm = GSM_STATEMACHINE_VAL(s);
  /* number of the configuration to get. */
  int num = Int_val(vnum);
  /* number of active configurations. */
  int cfg_num = GSM_GetConfigNum(sm);
  GSM_Config *cfg = GSM_GetConfig(sm, num);

  if (cfg == NULL || num >= cfg_num)
    caml_gammu_raise_Error(ERR_INVALID_CONFIG_NUM);

  CAMLreturn(Val_GSM_Config(cfg));
}

CAMLexport
value caml_gammu_push_config(value s, value vcfg)
{
  CAMLparam2(s, vcfg);
  GSM_StateMachine *sm = GSM_STATEMACHINE_VAL(s);
  int cfg_num = GSM_GetConfigNum(sm);
  GSM_Config *dest_cfg;

  /* We have to check that we don't push too much configs. If we have pushed
     enough configs to fill the stack, we'll set cfg_num to -1. */
  if (cfg_num != -1) {
    dest_cfg = GSM_GetConfig(sm, cfg_num);
    GSM_Config_val(dest_cfg, vcfg);
    /* GSM_SetConfigNum downsets the config num to maximum number of configs
       allowed. So, if the number of configs doesn't change, it's because
       we've reached max. */
    GSM_SetConfigNum(sm, cfg_num + 1);
    if (GSM_GetConfigNum(sm) == cfg_num)
      GSM_SetConfigNum(sm, -1);
  } else {
    /* Too many configs (more than MAX_CONFIG_NUM+1 (=6),
       unfortunately this const is not exported) */
    caml_gammu_raise_Error(ERR_INVALID_CONFIG_NUM);
  }

  CAMLreturn(Val_unit);
}

CAMLexport
value caml_gammu_remove_config(value s)
{
  CAMLparam1(s);
  GSM_StateMachine *sm = GSM_STATEMACHINE_VAL(s);
  int cfg_num = GSM_GetConfigNum(sm);

  if (cfg_num > 0)
    GSM_SetConfigNum(sm, cfg_num - 1);
  else
    /* Empty stack, can't remove */
    caml_gammu_raise_Error(ERR_INVALID_CONFIG_NUM);

  CAMLreturn(Val_unit);
}

CAMLexport
value caml_gammu_GSM_GetConfigNum(value s)
{
  CAMLparam1(s);
  CAMLreturn(Val_int( GSM_GetConfigNum(GSM_STATEMACHINE_VAL(s)) ));
}

CAMLexport
value caml_gammu_GSM_InitConnection(value vs, value vreply_num)
{
  CAMLparam2(vs, vreply_num);
  GSM_Error error;
  GSM_StateMachine* s = GSM_STATEMACHINE_VAL(vs);
  int ReplyNum = Int_val(vreply_num);

  caml_enter_blocking_section(); /* release global lock */
  error = GSM_InitConnection(s, ReplyNum);
  caml_leave_blocking_section(); /* acquire global lock */
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_unit);
}

static void log_function_callback(const char *text, void *data)
{
  CAMLparam0();
  CAMLlocal1(f);

  /* The caml function value was saved as user data for the callback. */
  f = *((value *) data);
  caml_callback(f, caml_copy_string(text));

  CAMLreturn0;
}

CAMLexport
value caml_gammu_GSM_InitConnection_Log(value s, value vreply_num,
                                       value vlog_func)
{
  CAMLparam3(s, vreply_num, vlog_func);
  GSM_Error error;
  State_Machine *state_machine = STATE_MACHINE_VAL(s);
  int ReplyNum = Int_val(vreply_num);

  REGISTER_SM_GLOBAL_ROOT(state_machine, log_function, vlog_func);
  caml_enter_blocking_section(); /* release global lock */
  error = GSM_InitConnection_Log(state_machine->sm, ReplyNum,
                                 log_function_callback, (void *) &vlog_func);
  caml_leave_blocking_section(); /* acquire global lock */
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_unit);
}

CAMLexport
value caml_gammu_GSM_TerminateConnection(value s)
{
  CAMLparam1(s);
  State_Machine *state_machine = STATE_MACHINE_VAL(s);
  GSM_Error error;

  /* Allow the GC to free the log function callback. And we don't unregister
   * the incomings callbacks since the user might re-init the connection
   * later (with the same callbacks). */
  UNREGISTER_SM_GLOBAL_ROOT(state_machine, log_function);
  caml_enter_blocking_section();
  error = GSM_TerminateConnection(state_machine->sm);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_unit);
}

CAMLexport
value caml_gammu_GSM_IsConnected(value s)
{
  CAMLparam1(s);

  CAMLreturn(Val_bool(GSM_IsConnected(GSM_STATEMACHINE_VAL(s))));
}

CAMLexport
value caml_gammu_GSM_GetUsedConnection(value s)
{
  CAMLparam1(s);
  GSM_StateMachine *sm = GSM_STATEMACHINE_VAL(s);
  GSM_ConnectionType conn_type;

  if (!GSM_IsConnected(sm))
    caml_gammu_raise_Error(ERR_NOTCONNECTED);

  conn_type = GSM_GetUsedConnection(sm);
  SHOUT_DBG("GSM_GetUsedConnection call made: conn_type = %i", (int) conn_type);

  CAMLreturn(VAL_GSM_CONNECTIONTYPE(conn_type));
}

CAMLexport
value caml_gammu_GSM_ReadDevice(value s, value vwait_for_reply)
{
  CAMLparam2(s, vwait_for_reply);
  GSM_StateMachine *sm;
  gboolean wait_for_reply;
  int read_bytes;

  sm = GSM_STATEMACHINE_VAL(s);
  wait_for_reply = Bool_val(vwait_for_reply);

  caml_enter_blocking_section();
  read_bytes = GSM_ReadDevice(sm, wait_for_reply);
  caml_leave_blocking_section();
  /* Bug in GSM_ReadDevice, the function already checks for connection, but
     one can't make the difference between a GSM not connected or 33 bytes
     read. This bug has been fixed in 1.28.92, it returns (-1) in that
     case. */
#if GAMMU_VERSION_NUM >= 12892
  if (read_bytes == -1)
#else
  if (!GSM_IsConnected(sm))
#endif
    caml_gammu_raise_Error(ERR_NOTCONNECTED);

  CAMLreturn(Val_int(read_bytes));
}


/************************************************************************/
/* Security related operations with phone */

CAMLexport
value caml_gammu_GSM_EnterSecurityCode(value s, value vcode_type, value vcode)
{
  CAMLparam2(s, vcode);
  GSM_StateMachine *sm;
  GSM_SecurityCode security_code;
  GSM_Error error;

  sm = GSM_STATEMACHINE_VAL(s),
  security_code.Type = GSM_SECURITYCODETYPE_VAL(vcode_type);
  CPY_TRIM_STRING_VAL(security_code.Code, vcode);

  caml_enter_blocking_section();
#if GAMMU_VERSION_NUM >= 12991
  error = GSM_EnterSecurityCode(sm, &security_code);
#else
  error = GSM_EnterSecurityCode(sm, security_code);
#endif
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_unit);
}

CAMLexport
value caml_gammu_GSM_GetSecurityStatus(value s)
{
  CAMLparam1(s);
  GSM_StateMachine *sm;
  GSM_SecurityCodeType status;
  GSM_Error error;

  sm = GSM_STATEMACHINE_VAL(s);

  caml_enter_blocking_section();
  error = GSM_GetSecurityStatus(sm, &status);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  CAMLreturn(VAL_GSM_SECURITYCODETYPE(status));
}


/************************************************************************/
/* Informations on the phone */

static value Val_GSM_BatteryCharge(GSM_BatteryCharge *battery_charge)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(10, 0);
  Store_field(res, 0, VAL_GSM_BATTERYTYPE(battery_charge->BatteryType));
  Store_field(res, 1, Val_int(battery_charge->BatteryCapacity));
  Store_field(res, 2, Val_int(battery_charge->BatteryPercent));
  Store_field(res, 3, VAL_GSM_CHARGESTATE(battery_charge->ChargeState));
  Store_field(res, 4, Val_int(battery_charge->BatteryVoltage));
  Store_field(res, 5, Val_int(battery_charge->ChargeVoltage));
  Store_field(res, 6, Val_int(battery_charge->ChargeCurrent));
  Store_field(res, 7, Val_int(battery_charge->PhoneCurrent));
  Store_field(res, 8, Val_int(battery_charge->BatteryTemperature));
  Store_field(res, 9, Val_int(battery_charge->PhoneTemperature));

  CAMLreturn(res);
}

static value Val_GSM_PhoneModel(GSM_PhoneModel *phone_model)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(3, 0);
  Store_field(res, 0, caml_copy_string(phone_model->model));
  Store_field(res, 1, caml_copy_string(phone_model->number));
  Store_field(res, 2, caml_copy_string(phone_model->irdamodel));

  CAMLreturn(res);
}

static value Val_GSM_NetworkInfo(GSM_NetworkInfo *network)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(9, 0);
#if GAMMU_VERSION_NUM < 12793
  /* Wrong type. Fixed in gammu commit e59c881b "These are regullar
   * chars." included in 1.27.93. */
  Store_field(res, 0, caml_copy_string((char *) network->CID));
  Store_field(res, 3, caml_copy_string((char *) network->LAC));
#else
  Store_field(res, 0, caml_copy_string(network->CID));
  Store_field(res, 3, caml_copy_string(network->LAC));
#endif
  Store_field(res, 1, caml_copy_string(network->NetworkCode));
  Store_field(res, 2, VAL_GSM_NETWORKINFO_STATE(network->State));
  Store_field(res, 4, CAML_COPY_USTRING(network->NetworkName));
  /* Some fields weren't present yet in older versions, give them unknown
     values */
#if GAMMU_VERSION_NUM >= 12792
  Store_field(res, 5, VAL_GSM_GPRS_STATE(network->GPRS));
#else
  Store_field(res, 5, Val_int(2) /* grps_state = Unknown */);
#endif
#if GAMMU_VERSION_NUM >= 12796
  Store_field(res, 6, caml_copy_string(network->PacketCID));
  Store_field(res, 7, VAL_GSM_NETWORKINFO_STATE(network->PacketState));
  Store_field(res, 8, caml_copy_string(network->PacketLAC));
#else
  Store_field(res, 6, caml_copy_string("Unknown"));
  Store_field(res, 7, Val_int(4) /* network_state = Unknown */);
  Store_field(res, 8, caml_copy_string("Unknown"));
#endif

  CAMLreturn(res);
}

static value Val_GSM_SignalQuality(GSM_SignalQuality *signal_quality)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(3, 0);
  Store_field(res, 0, Val_int(signal_quality->SignalStrength));
  Store_field(res, 1, Val_int(signal_quality->SignalPercent));
  Store_field(res, 2, Val_int(signal_quality->BitErrorRate));

  CAMLreturn(res);
}

CAMLexport
value caml_gammu_GSM_GetNetworkName(value vcode)
{
  CAMLparam1(vcode);
  const unsigned char *name = GSM_GetNetworkName(String_val(vcode));
  CAMLreturn(CAML_COPY_USTRING(name));
}

CAMLexport
value caml_gammu_GSM_GetCountryName(value vcode)
{
  CAMLparam1(vcode);
  const unsigned char *name = GSM_GetCountryName(String_val(vcode));
  CAMLreturn(CAML_COPY_USTRING(name));
}


#define CAML_GAMMU_GSM_TYPE_GET(name)                                   \
  CAMLexport CAML_GAMMU_GSM_TYPE_GET_PROTOTYPE(name)                    \
  {                                                                     \
    CAMLparam1(s);                                                      \
    GSM_##name res;                                                     \
    GSM_Error error;                                                    \
    error = GSM_Get##name(GSM_STATEMACHINE_VAL(s), &res);               \
    caml_gammu_raise_Error(error);                                      \
    CAMLreturn(Val_GSM_##name(&res));                                   \
  }

CAML_GAMMU_GSM_TYPE_GET(BatteryCharge)

CAMLexport
value caml_gammu_GSM_GetFirmWare(value s)
{
  CAMLparam1(s);
  CAMLlocal1(res);
  GSM_StateMachine *sm;
  char val[GSM_MAX_VERSION_LENGTH + 1];
  char date[GSM_MAX_VERSION_DATE_LENGTH + 1];
  double num;
  GSM_Error error;

  sm = GSM_STATEMACHINE_VAL(s);

  caml_enter_blocking_section();
  error = GSM_GetFirmware(sm, val, date, &num);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  res = caml_alloc(3, 0);
  Store_field(res, 0, caml_copy_string(val));
  Store_field(res, 1, caml_copy_string(date));
  Store_field(res, 2, caml_copy_double(num));

  CAMLreturn(res);
}

CAML_GAMMU_GSM_STR_GET(Hardware, BUFFER_LENGTH)

CAML_GAMMU_GSM_STR_GET(IMEI, GSM_MAX_IMEI_LENGTH + 1)

CAML_GAMMU_GSM_STR_GET(ManufactureMonth, BUFFER_LENGTH)

CAML_GAMMU_GSM_STR_GET(Manufacturer, GSM_MAX_MANUFACTURER_LENGTH + 1)

CAML_GAMMU_GSM_STR_GET(Model, GSM_MAX_MODEL_LENGTH + 1)

CAMLexport
value caml_gammu_GSM_GetModelInfo(value s)
{
  CAMLparam1(s);
  GSM_PhoneModel *phone_model;

  phone_model = GSM_GetModelInfo(GSM_STATEMACHINE_VAL(s));

  CAMLreturn(Val_GSM_PhoneModel(phone_model));
}

CAML_GAMMU_GSM_TYPE_GET(NetworkInfo)

CAML_GAMMU_GSM_STR_GET(ProductCode, BUFFER_LENGTH)

CAML_GAMMU_GSM_TYPE_GET(SignalQuality)


/************************************************************************/
/* Date and time */

static GSM_DateTime *GSM_DateTime_val(GSM_DateTime *date_time, value vdate_time)
{
  date_time->Timezone = Int_val(Field(vdate_time, 0));
  date_time->Second = Int_val(Field(vdate_time, 1));
  date_time->Minute = Int_val(Field(vdate_time, 2));
  date_time->Hour = Int_val(Field(vdate_time, 3));
  date_time->Day = Int_val(Field(vdate_time, 4));
  date_time->Month = Int_val(Field(vdate_time, 5));
  date_time->Year = Int_val(Field(vdate_time, 6));

  return date_time;
}

static value Val_GSM_DateTime(GSM_DateTime *date_time)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(7, 0);
  Store_field(res, 0, Val_int(date_time->Timezone));
  Store_field(res, 1, Val_int(date_time->Second));
  Store_field(res, 2, Val_int(date_time->Minute));
  Store_field(res, 3, Val_int(date_time->Hour));
  Store_field(res, 4, Val_int(date_time->Day));
  Store_field(res, 5, Val_int(date_time->Month));
  Store_field(res, 6, Val_int(date_time->Year));

  CAMLreturn(res);
}

CAMLexport
value caml_gammu_GSM_CheckDate(value vdate)
{
  CAMLparam1(vdate);
  GSM_DateTime dt;
  gboolean date_ok;

  date_ok = CheckDate(GSM_DateTime_val(&dt, vdate));

  CAMLreturn(Val_bool(date_ok));
}

CAMLexport
value caml_gammu_GSM_CheckTime(value vdate)
{
  CAMLparam1(vdate);
  GSM_DateTime dt;

  GSM_DateTime_val(&dt, vdate);

  CAMLreturn(Val_bool(CheckTime(&dt)));
}

CAMLexport
value caml_gammu_GSM_OSDate(value vdt)
{
  CAMLparam1(vdt);
  GSM_DateTime dt;
  char *os_date;

  /* TODO: Ask why does OSDate takes value instead of pointer ? */
  GSM_DateTime_val(&dt, vdt);
  os_date = OSDate(dt);

  CAMLreturn(caml_copy_string(os_date));
}

CAMLexport
value caml_gammu_GSM_OSDateTime(value vdt, value vtimezone)
{
  CAMLparam2(vdt, vtimezone);
  GSM_DateTime dt;
  char *os_date_time;

  GSM_DateTime_val(&dt, vdt);
  os_date_time = OSDateTime(dt, Bool_val(vtimezone));

  CAMLreturn(caml_copy_string(os_date_time));
}


/************************************************************************/
/* Memory */

/* Currently unused, needs updating w.r.t. the definitions.
static value Val_GSM_SubMemoryEntry(GSM_SubMemoryEntry *sub_mem_entry)
{
  CAMLparam0();
  CAMLlocal2(res, vsms_list);
  int i;

  Store_field(res, 0, VAL_GSM_ENTRYTYPE(sub_mem_entry->EntryType));
  Store_field(res, 1, Val_GSM_DateTime(&(sub_mem_entry->Date)));
  Store_field(res, 2, Val_int(sub_mem_entry->Number));
  Store_field(res, 3, Val_int(sub_mem_entry->VoiceTag));
  vsms_list = caml_alloc(20, 0);
  for (i=0; i < 20; i++)
    Store_field(vsms_list, i, sub_mem_entry->SMSList[i]);
  Store_field(res, 4, vsms_list);
  Store_field(res, 5, Val_int(sub_mem_entry->CallLength));
  Store_field(res, 6, VAL_GSM_ERROR(sub_mem_entry->AddError));
  Store_field(res, 7, CAML_COPY_USTRING(sub_mem_entry->Text));

  CAMLreturn(res);
}

static value Val_GSM_MemoryEntry(GSM_MemoryEntry *mem_entry)
{
  CAMLparam0();
  CAMLlocal2(res, ventries);
  res = caml_alloc(3, 0);
  int length = mem_entry->EntriesNum;
  int i;

  Store_field(res, 0, VAL_GSM_MEMORYTYPE(mem_entry->MemoryType));
  Store_field(res, 1, Val_int(mem_entry->Location));
  ventries = caml_alloc(length, 0);
  for (i=0; i < length; i++) {
    Store_field(ventries, i, Val_GSM_SubMemoryEntry(&(mem_entry->Entries[i])));
  }
  Store_field(res, 2, ventries);

  CAMLreturn(res);
}
*/

/************************************************************************/
/* Messages */

static GSM_UDHHeader *GSM_UDHHeader_val(GSM_UDHHeader *udh_header,
                                        value vudh_header)
{
  udh_header->Type = GSM_UDH_VAL(Field(vudh_header, 0));
  udh_header->Length = caml_string_length(Field(vudh_header, 1));
  CPY_TRIM_USTRING_VAL(udh_header->Text, Field(vudh_header, 1));
  udh_header->ID8bit = Int_val(Field(vudh_header, 2));
  udh_header->ID16bit = Int_val(Field(vudh_header, 3));
  udh_header->PartNumber = Int_val(Field(vudh_header, 4));
  udh_header->AllParts = Int_val(Field(vudh_header, 5));

  return udh_header;
}

static value Val_GSM_UDHHeader(GSM_UDHHeader *udh_header)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(6, 0);
  Store_field(res, 0, VAL_GSM_UDH(udh_header->Type));
  Store_field(res, 1, CAML_COPY_USTRING(udh_header->Text));
  Store_field(res, 2, Val_int(udh_header->ID8bit));
  Store_field(res, 3, Val_int(udh_header->ID16bit));
  Store_field(res, 4, Val_int(udh_header->PartNumber));
  Store_field(res, 5, Val_int(udh_header->AllParts));

  CAMLreturn(res);
}

static GSM_SMSValidity *GSM_SMSValidity_val(GSM_SMSValidity *validity,
                                            value vvalidity)
{
  if (Is_long(vvalidity)) {
    /* vvalidity : Not_available */
    validity->Format = SMS_Validity_NotAvailable;
    validity->Relative = SMS_VALID_Max_Time;
  }
  else {
    /* vvalidity : Relative of char */
    validity->Format = SMS_Validity_RelativeFormat;
    validity->Relative = Int_val(Field(vvalidity, 0));
  }

  return validity;
}

static value Val_GSM_SMSValidity(GSM_SMSValidity *validity)
{
  CAMLparam0();
  CAMLlocal1(res);

  if (validity->Format == SMS_Validity_NotAvailable) {
    res = Val_int(0);
  }
  else {
    res = caml_alloc(1, 0);
    Store_field(res, 0, Val_int(validity->Relative));
  }

  CAMLreturn(res);
}

static GSM_SMSC *GSM_SMSC_val(GSM_SMSC *smsc, value vsmsc)
{
  smsc->Location = Int_val(Field(vsmsc, 0));
  CPY_TRIM_USTRING_VAL(smsc->Name, Field(vsmsc, 1));
  CPY_TRIM_USTRING_VAL(smsc->Number, Field(vsmsc, 2));
  GSM_SMSValidity_val(&(smsc->Validity), Field(vsmsc, 3));
  smsc->Format = GSM_SMSFORMAT_VAL(Field(vsmsc, 4));
  CPY_TRIM_USTRING_VAL(smsc->DefaultNumber, Field(vsmsc, 5));

  return smsc;
}

static value Val_GSM_SMSC(GSM_SMSC *smsc)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(6, 0);
  Store_field(res, 0, Val_int(smsc->Location));
  Store_field(res, 1, CAML_COPY_USTRING(smsc->Name));
  Store_field(res, 2, CAML_COPY_USTRING(smsc->Number));
  Store_field(res, 3, Val_GSM_SMSValidity(&(smsc->Validity)));
  Store_field(res, 4, VAL_GSM_SMSFORMAT(smsc->Format));
  Store_field(res, 5, CAML_COPY_USTRING(smsc->DefaultNumber));

  CAMLreturn(res);
}

static GSM_SMSMessage *GSM_SMSMessage_val(GSM_SMSMessage *sms, value vsms)
{
  value vother_numbers;
  value vtext;
  int length;
  int i;

  sms->ReplaceMessage = UCHAR_VAL(Field(vsms, 0));
  sms->RejectDuplicates = Bool_val(Field(vsms, 1));
  GSM_UDHHeader_val(&(sms->UDH), Field(vsms, 2));
  CPY_TRIM_USTRING_VAL(sms->Number, Field(vsms, 3));
  vother_numbers = Field(vsms, 4);
  length = Wosize_val(vother_numbers);
  if (length > sizeof(sms->OtherNumbers))
    length = sizeof(sms->OtherNumbers);
  for (i=0; i < length; i++)
    CPY_TRIM_USTRING_VAL(sms->OtherNumbers[i], Field(vother_numbers, i));
  sms->OtherNumbersNum = length;
  GSM_SMSC_val(&(sms->SMSC), Field(vsms, 5));
  sms->Memory = GSM_MEMORYTYPE_VAL(Field(vsms, 6));
  sms->Location = Int_val(Field(vsms, 7));
  sms->Folder = Int_val(Field(vsms, 8));
  sms->InboxFolder = Bool_val(Field(vsms, 9));
  vtext = Field(vsms, 12);
  sms->Length = caml_string_length(vtext);
  sms->State = GSM_SMS_STATE_VAL(Field(vsms, 10));
  CPY_TRIM_USTRING_VAL(sms->Name, Field(vsms, 11));
  CPY_TRIM_USTRING_VAL(sms->Text, vtext);
  sms->PDU = GSM_SMSMESSAGETYPE_VAL(Field(vsms, 13));
  sms->Coding = GSM_CODING_TYPE_VAL(Field(vsms, 14));
  GSM_DateTime_val(&(sms->DateTime), Field(vsms, 15));
  GSM_DateTime_val(&(sms->SMSCTime), Field(vsms, 16));
  sms->DeliveryStatus = UCHAR_VAL(Field(vsms, 17));
  sms->ReplyViaSameSMSC = Bool_val(Field(vsms, 18));
  sms->Class = CHAR_VAL(Field(vsms, 19));
  sms->MessageReference = UCHAR_VAL(Field(vsms, 20));

  return sms;
}

static value Val_GSM_SMSMessage(GSM_SMSMessage *sms)
{
  CAMLparam0();
  CAMLlocal2(res, vother_numbers);
  int length = sms->OtherNumbersNum;
  int i;

  res = caml_alloc(21, 0);
  Store_field(res, 0, VAL_UCHAR(sms->ReplaceMessage));
  Store_field(res, 1, Val_bool(sms->RejectDuplicates));
  Store_field(res, 2, Val_GSM_UDHHeader(&(sms->UDH)));
  Store_field(res, 3, CAML_COPY_USTRING(sms->Number));
  vother_numbers = caml_alloc(length, 0);
  for (i=0; i < length; i++)
    Store_field(vother_numbers, i, CAML_COPY_USTRING(sms->OtherNumbers[i]));
  Store_field(res, 4, vother_numbers);
  Store_field(res, 5, Val_GSM_SMSC(&(sms->SMSC)));
  Store_field(res, 6, VAL_GSM_MEMORYTYPE(sms->Memory));
  Store_field(res, 7, Val_int(sms->Location));
  Store_field(res, 8, Val_int(sms->Folder));
  Store_field(res, 9, Val_bool(sms->InboxFolder));
  Store_field(res, 10, VAL_GSM_SMS_STATE(sms->State));
  Store_field(res, 11, CAML_COPY_USTRING(sms->Name));
  /* FIXME: What to do when sms->Text == NULL ? */
  if (sms->Coding == SMS_Coding_8bit)
    Store_field(res, 12, caml_copy_string((char *) sms->Text));
  else
    Store_field(res, 12, CAML_COPY_USTRING(sms->Text));
  Store_field(res, 13, VAL_GSM_SMSMESSAGETYPE(sms->PDU));
  Store_field(res, 14, VAL_GSM_CODING_TYPE(sms->Coding));
  Store_field(res, 15, Val_GSM_DateTime(&(sms->DateTime)));
  Store_field(res, 16, Val_GSM_DateTime(&(sms->SMSCTime)));
  Store_field(res, 17, VAL_UCHAR(sms->DeliveryStatus));
  Store_field(res, 18, Val_bool(sms->ReplyViaSameSMSC));
  Store_field(res, 19, VAL_CHAR(sms->Class));
  Store_field(res, 20, VAL_UCHAR(sms->MessageReference));

  CAMLreturn(res);
}

static GSM_MultiSMSMessage *GSM_MultiSMSMessage_val(
    value vmulti_sms, GSM_MultiSMSMessage *multi_sms)
{
  int length = Wosize_val(vmulti_sms);;
  int i;
  SHOUT_DBG("Entering function.\nmulti_sms length = %d", length);

  /* We truncate the array if it's too big. TODO: issue a warning ? */
  if (length > sizeof(multi_sms->SMS)) {
    SHOUT_DBG("multi_sms too big, array truncated.");
    length = sizeof(multi_sms->SMS);
  }
  for (i=0; i < length; i++) {
    GSM_SMSMessage_val(&(multi_sms->SMS[i]), Field(vmulti_sms, i));
    SHOUT_DBG("multi_sms->SMS[%d].Number = %s", i,
          DecodeUnicodeConsole(multi_sms->SMS[i].Number));
  }
  multi_sms->Number = length;

  SHOUT_DBG("Leaving function.");
  return multi_sms;
}

static value Val_GSM_MultiSMSMessage(GSM_MultiSMSMessage *multi_sms)
{
  CAMLparam0();
  CAMLlocal1(res);
  int length = multi_sms->Number;
  int i;

  res = caml_alloc(length, 0);
  for (i=0; i < length; i++)
    Store_field(res, i, Val_GSM_SMSMessage(&(multi_sms->SMS[i])));

  CAMLreturn(res);
}

CAMLexport
value caml_gammu_GSM_GetSMS(value s, value vfolder, value vlocation)
{
  CAMLparam3(s, vfolder, vlocation);
  CAMLlocal1(vsms);
  GSM_StateMachine *sm;
  GSM_MultiSMSMessage sms;
  GSM_Error error;
  int i;

  sm = GSM_STATEMACHINE_VAL(s);
  /* Clear SMS structure */
  for (i = 0; i < GSM_MAX_MULTI_SMS; i++)
    GSM_SetDefaultSMSData(&sms.SMS[i]);

  sms.SMS[0].Location = Int_val(vlocation);
  sms.SMS[0].Folder = Int_val(vfolder);
  /* TODO: Is that necessary ? */
  sms.Number = 0;

  caml_enter_blocking_section();
  error = GSM_GetSMS(sm, &sms);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  vsms = Val_GSM_MultiSMSMessage(&sms);
  CAMLreturn(vsms);
}

CAMLexport
value caml_gammu_GSM_GetNextSMS(value s, value vlocation, value vfolder,
                                value vstart)
{
  CAMLparam4(s, vlocation, vfolder, vstart);
  GSM_StateMachine *sm;
  gboolean start;
  GSM_MultiSMSMessage sms;
  GSM_Error error;
  int i;

  sm = GSM_STATEMACHINE_VAL(s);
  start = Bool_val(vstart);
  /* Clear SMS structure */
  for (i = 0; i < GSM_MAX_MULTI_SMS; i++)
    GSM_SetDefaultSMSData(&sms.SMS[i]);

  sms.SMS[0].Location = Int_val(vlocation);
  sms.SMS[0].Folder = Int_val(vfolder);
  /* TODO: Is that necessary ? */
  sms.Number = 0;

  caml_enter_blocking_section();
  error = GSM_GetNextSMS(sm, &sms, start);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_GSM_MultiSMSMessage(&sms));
}

#define CAML_GAMMU_GSM_SETSMS(set)                              \
  CAMLexport                                                    \
  value caml_gammu_GSM_##set##SMS(value s, value vsms)          \
  {                                                             \
    CAMLparam2(s, vsms);                                        \
    CAMLlocal1(res);                                            \
    GSM_Error error;                                            \
    GSM_StateMachine *sm = GSM_STATEMACHINE_VAL(s);             \
    GSM_SMSMessage sms;                                         \
    GSM_SMSMessage_val(&sms, vsms);                             \
    caml_enter_blocking_section();                              \
    error = GSM_##set##SMS(sm, &sms);                           \
    caml_leave_blocking_section();                              \
    caml_gammu_raise_Error(error);                              \
    res = caml_alloc(2, 0);                                     \
    Store_field(res, 0, Val_int(sms.Folder));                   \
    Store_field(res, 1, Val_int(sms.Location));                 \
    CAMLreturn(res);                                            \
  }

CAML_GAMMU_GSM_SETSMS(Set)
CAML_GAMMU_GSM_SETSMS(Add)

CAMLexport
value caml_gammu_GSM_SendSMS(value s, value vsms)
{
  CAMLparam2(s, vsms);
  GSM_Error error;
  GSM_StateMachine *sm = GSM_STATEMACHINE_VAL(s);
  GSM_SMSMessage sms;
  GSM_SMSMessage_val(&sms, vsms);
  caml_enter_blocking_section(); /* release global lock */
  error = GSM_SendSMS(sm, &sms);
  caml_leave_blocking_section(); /* acquire global lock */
  caml_gammu_raise_Error(error);
  CAMLreturn(Val_unit);
}

static value Val_GSM_OneSMSFolder(GSM_OneSMSFolder *folder)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(3, 0);
  Store_field(res, 0, OUTBOX(folder->OutboxFolder));
  Store_field(res, 1, VAL_GSM_MEMORYTYPE(folder->Memory));
  Store_field(res, 2, CAML_COPY_USTRING(folder->Name));
  CAMLreturn(res);
}

CAMLexport
value caml_gammu_GSM_GetSMSFolders(value s)
{
  CAMLparam1(s);
  CAMLlocal1(res);
  GSM_StateMachine *sm;
  GSM_SMSFolders folders;
  GSM_Error error;
  int i;

  sm = GSM_STATEMACHINE_VAL(s);

  /* Get the folders. */
  caml_enter_blocking_section();
  error = GSM_GetSMSFolders(sm, &folders);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  /* Convert it to a an array of SMS.folder values. */
  res = caml_alloc(folders.Number, 0);
  for (i=0; i < folders.Number; i++)
    Store_field(res, i, Val_GSM_OneSMSFolder(&(folders.Folder[i])));

  CAMLreturn(res);
}

static value Val_GSM_SMSMemoryStatus(GSM_SMSMemoryStatus *sms_mem)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(7, 0);
  Store_field(res, 0, Val_int(sms_mem->SIMUnRead));
  Store_field(res, 1, Val_int(sms_mem->SIMUsed));
  Store_field(res, 2, Val_int(sms_mem->SIMSize));
  Store_field(res, 3, Val_int(sms_mem->TemplatesUsed));
  Store_field(res, 4, Val_int(sms_mem->PhoneUnRead));
  Store_field(res, 5, Val_int(sms_mem->PhoneUsed));
  Store_field(res, 6, Val_int(sms_mem->PhoneSize));

  CAMLreturn(res);
}

CAMLexport
value caml_gammu_GSM_GetSMSStatus(value s)
{
  CAMLparam1(s);
  GSM_StateMachine *sm;
  GSM_SMSMemoryStatus status;
  GSM_Error error;

  sm = GSM_STATEMACHINE_VAL(s);

  caml_enter_blocking_section();
  error = GSM_GetSMSStatus(sm, &status);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_GSM_SMSMemoryStatus(&status));
}

CAMLexport
value caml_gammu_GSM_DeleteSMS(value s, value vlocation, value vfolder)
{
  CAMLparam3(s, vlocation, vfolder);
  GSM_StateMachine *sm;
  GSM_SMSMessage sms;
  GSM_Error error;

  sm = GSM_STATEMACHINE_VAL(s);

  sms.Location = Int_val(vlocation);
  sms.Folder = Int_val(vfolder);

  caml_enter_blocking_section();
  error = GSM_DeleteSMS(sm, &sms);
  caml_leave_blocking_section();
  caml_gammu_raise_Error(error);

  CAMLreturn(Val_unit);
}

/* Unused and need update.
static GSM_MultiPartSMSEntry GSM_MultiPartSMSEntry_val(value vmult_part_sms)
{
  GSM_MultiPartSMSEntry mult_part_sms;
  mult_part_sms.ID = GSM_ENCODEMULTIPARTSMSID_VAL(Field(vmult_part_sms, 0));
  mult_part_sms.Number = Int_val(Field(vmult_part_sms, 1));
  mult_part_sms.Phonebook = GSM_MemoryEntry_val(Field(vmult_part_sms, 2));
  mult_part_sms.Protected = Bool_val(Field(vmult_part_sms, 3));
  mult_part_sms.Buffer = (unsigned char*) String_val(Field(vmult_part_sms, 4));
  mult_part_sms.Left = Bool_val(Field(vmult_part_sms, 5));
  mult_part_sms.Right = Bool_val(Field(vmult_part_sms, 6));
  mult_part_sms.Center = Bool_val(Field(vmult_part_sms, 7));
  mult_part_sms.Large = Bool_val(Field(vmult_part_sms, 8));
  mult_part_sms.Small = Bool_val(Field(vmult_part_sms, 9));
  mult_part_sms.Bold = Bool_val(Field(vmult_part_sms, 10));
  mult_part_sms.Italic = Bool_val(Field(vmult_part_sms, 11));
  mult_part_sms.Underlined = Bool_val(Field(vmult_part_sms, 12));
  mult_part_sms.Strikethrough = Bool_val(Field(vmult_part_sms, 13));
  mult_part_sms.RingtoneNotes = Int_val(Field(vmult_part_sms, 14));
  return mult_part_sms;
}
*/

static value Val_GSM_MultiPartSMSEntry(GSM_MultiPartSMSEntry mult_part_sms)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(15, 0);
  Store_field(res, 0, VAL_GSM_ENCODEMULTIPARTSMSID(mult_part_sms.ID));
  Store_field(res, 1, Val_int(mult_part_sms.Number));
  /*Store_field(res, 2, Val_GSM_MemoryEntry(mult_part_sms.Phonebook));*/
  Store_field(res, 2, Val_bool(mult_part_sms.Protected));
  Store_field(res, 3, CAML_COPY_USTRING(mult_part_sms.Buffer));
  Store_field(res, 4, Val_bool(mult_part_sms.Left));
  Store_field(res, 5, Val_bool(mult_part_sms.Right));
  Store_field(res, 6, Val_bool(mult_part_sms.Center));
  Store_field(res, 7, Val_bool(mult_part_sms.Large));
  Store_field(res, 8, Val_bool(mult_part_sms.Small));
  Store_field(res, 9, Val_bool(mult_part_sms.Bold));
  Store_field(res, 10, Val_bool(mult_part_sms.Italic));
  Store_field(res, 11, Val_bool(mult_part_sms.Underlined));
  Store_field(res, 12, Val_bool(mult_part_sms.Strikethrough));
  Store_field(res, 13, Val_int(mult_part_sms.RingtoneNotes));

  CAMLreturn(res);
}

static value Val_GSM_MultiPartSMSInfo(GSM_MultiPartSMSInfo *multipart_sms_info)
{
  CAMLparam0();
  CAMLlocal3(res, ventries, ventry);
  /* Todo: can caml_alloc fail ? */
  res = caml_alloc(5, 0);
  int length = multipart_sms_info->EntriesNum;
  int i;

  Store_field(res, 0, Val_bool(multipart_sms_info->UnicodeCoding));
  Store_field(res, 1, Val_int(multipart_sms_info->Class));
  Store_field(res, 2, VAL_CHAR(multipart_sms_info->ReplaceMessage));
  Store_field(res, 3, Val_bool(multipart_sms_info->Unknown));

  ventries = caml_alloc(length, 0);
  for (i=0; i < length; i++) {
    ventry = Val_GSM_MultiPartSMSEntry(multipart_sms_info->Entries[i]);
    Store_field(ventries, i, ventry);
  }
  Store_field(res, 4, ventries);

  CAMLreturn(res);
}

CAMLexport
value caml_gammu_GSM_DecodeMultiPartSMS(value vdi, value vsms,
                                        value vems)
{
  CAMLparam3(vdi, vsms, vems);
  CAMLlocal1(vmulti_sms);
  SHOUT_DBG("Entering function.");
  GSM_MultiSMSMessage multi_sms;
  GSM_MultiPartSMSInfo info;
  GSM_Debug_Info *di = GSM_Debug_Info_val(vdi);

  GSM_MultiSMSMessage_val(vsms, &multi_sms);

  SHOUT_DBG("multi_sms.SMS[0].UDH.Length = %d", multi_sms.SMS[0].UDH.Length);
  if (!GSM_DecodeMultiPartSMS(di, &info, &multi_sms, Bool_val(vems))) {
    GSM_FreeMultiPartSMSInfo(&info);
    caml_gammu_raise_Error(ERR_COULD_NOT_DECODE);
  }
  SHOUT_DBG("Decoding multi part SMS succeed.");
  vmulti_sms = Val_GSM_MultiPartSMSInfo(&info);

  SHOUT_DBG("Free GSM_MultiPartSMSInfo structure.");
  GSM_FreeMultiPartSMSInfo(&info);

  SHOUT_DBG("Leaving function.");
  CAMLreturn(vmulti_sms);
}


/************************************************************************/
/* Calls */

/* Currently unused.
static GSM_CallStatus GSM_CallStatus_val(value vcall_status, int *arg_int1)
{
  int ret = 0;
  value hash;

  // vcall_status is a variant type maybe parameterized. It is constant if it
  // is an immediate integer.
  if (Is_long(vcall_status)) {
    arg_int1 = 0;
    ret = Int_val(vcall_status);
    if (ret > 3)
      ret += 2;
    else
      ret++;
  } else {
    hash = Field(vcall_status, 0);
    if (hash == caml_hash_RemoteEnded) {
      *arg_int1 = Int_val(Field(vcall_status, 1));
      ret = 5;
    }
  }

  return ret;
}
*/

static value Val_GSM_CallStatus(GSM_CallStatus call_status, int *arg_int1)
{
  CAMLparam0();
  CAMLlocal1(res);

  if (call_status < 5) {
    res = Val_int(call_status - 1);
  } else if (call_status == 5) {
    /* Construct variant type with argument. */
    res = caml_alloc(1, 0);
    Store_field(res, 0, Val_int(arg_int1));
  } else {
    res = Val_int(call_status - 2);
  }

  CAMLreturn(res);
}

/* Currently unused.
static GSM_Call *GSM_Call_val(GSM_Call *call, value vcall)
{

  call->Status = GSM_CallStatus_val(Field(vcall, 0), &(call->StatusCode));
  call->CallID = option_val(Field(vcall, 1), &(call->CallIDAvailable));
  CPY_TRIM_USTRING_VAL(call->PhoneNumber, Field(vcall, 3));

  return call;
}
*/

static value Val_GSM_Call(GSM_Call *call)
{
  CAMLparam0();
  CAMLlocal1(res);

  res = caml_alloc(4, 0);
  Store_field(res, 0, Val_GSM_CallStatus(call->Status, &(call->StatusCode)));
  Store_field(res, 1, call->CallIDAvailable ?
              val_Some(call->CallID) : VAL_NONE);
  Store_field(res, 2, CAML_COPY_USTRING(call->PhoneNumber));

  CAMLreturn(res);
}


/************************************************************************/
/* Events */

#define CAML_GAMMU_GSM_SETINCOMING(name, type)                          \
  CAMLexport                                                            \
  value caml_gammu_GSM_SetIncoming##name(value s, value venable)        \
  {                                                                     \
    CAMLparam2(s, venable);                                             \
    GSM_Error error;                                                    \
    SHOUT_DBG("entering");                                                  \
    error = GSM_SetIncoming##name(GSM_STATEMACHINE_VAL(s),              \
                                  Bool_val(venable));                   \
    SHOUT_DBG("");                                                          \
    caml_gammu_raise_Error(error);                                      \
    SHOUT_DBG("leaving");                                                   \
    CAMLreturn(Val_unit);                                               \
  }                                                                     \
  static void incoming_##name##_callback(GSM_StateMachine *sm,          \
                                         type TYPE_MODIFIER1 t,         \
                                         void *user_data)               \
  {                                                                     \
    CAMLparam0();                                                       \
    CAMLlocal1(f);                                                      \
    SHOUT_DBG("entering");                                                  \
    SHOUT_DBG("f = *%ld", (long) user_data);                                \
    f = *((value *) user_data);                                         \
    caml_callback(f, Val_##type(TYPE_MODIFIER2 t));                     \
    SHOUT_DBG("leaving");                                                   \
    CAMLreturn0;                                                        \
  }                                                                     \
  CAMLexport                                                            \
  value caml_gammu_GSM_SetIncoming##name##Callback(value s, value vf)   \
  {                                                                     \
    CAMLparam2(s, vf);                                                  \
    SHOUT_DBG("entering");                                                  \
    State_Machine *state_machine = STATE_MACHINE_VAL(s);                \
    void *user_data = (void *) &(state_machine->incoming_##name##_callback); \
    SHOUT_DBG("*user_data = %ld", (long) user_data);                        \
    REGISTER_SM_GLOBAL_ROOT(state_machine, incoming_##name##_callback, vf); \
    GSM_SetIncoming##name##Callback(state_machine->sm,                  \
                                    incoming_##name##_callback,         \
                                    user_data);                         \
    SHOUT_DBG("leaving");                                                   \
    CAMLreturn(Val_unit);                                               \
  }

CAML_GAMMU_GSM_SETINCOMING(SMS, GSM_SMSMessage)

CAML_GAMMU_GSM_SETINCOMING(Call, GSM_Call)
