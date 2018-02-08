/* File: gammu_stubs.h

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

#ifndef __GAMMU_STUBS_H__
#define __GAMMU_STUBS_H__

#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <gammu.h>

#define BUFFER_LENGTH 255

/* Check Gammu version. */
#ifndef GAMMU_VERSION_NUM
# ifndef VERSION_NUM
#   error "Can't probe Gammu version !"
# endif
# define GAMMU_VERSION_NUM VERSION_NUM
#endif

#if !( GAMMU_VERSION_NUM == 12400                     \
       || GAMMU_VERSION_NUM == 12601                  \
       || GAMMU_VERSION_NUM == 12792                  \
       || GAMMU_VERSION_NUM == 12800                  \
       || GAMMU_VERSION_NUM == 12900                  \
       || GAMMU_VERSION_NUM == 12991                  \
       || GAMMU_VERSION_NUM == 13100                  \
       || GAMMU_VERSION_NUM == 13190                  \
       || GAMMU_VERSION_NUM == 13300                  \
       || GAMMU_VERSION_NUM == 13606                  \
       || GAMMU_VERSION_NUM == 13805                  \
       || GAMMU_VERSION_NUM == 13900)
# warning "Your version of libGammu was totally not tested. \
If compilation fails, please report your version number \
and if possible attach the error log."
#endif

/* Assume that gammu-types.h deals with glib correctly.
// typedef int gboolean;
But in some versions, it doesn't : */
#if GAMMU_VERSION_NUM < 12601
typedef int gboolean;
#endif
#ifndef FALSE
# define FALSE (0)
#endif
#ifndef TRUE
# define TRUE (!FALSE)
#endif

#ifdef CAML_GAMMU_DEBUG
# define LOCATION_STR "DEBUG: %s:%d In function '%s':\n"
# define LOCATION_ARGS __FILE__, __LINE__, __func__
# define SHOUT_DBG(fmt, ...)                                            \
  do {                                                                  \
    fprintf(stderr, LOCATION_STR fmt "\n", LOCATION_ARGS, ## __VA_ARGS__ ); \
    fflush(stderr);                                                     \
  } while (0)
#else
# define SHOUT_DBG(...)
#endif


/************************************************************************/
/* Init */
GSM_Debug_Info *global_debug;

void caml_gammu_init();


/************************************************************************/
/* Utils functions and macros. */

/* Copy string represented by the value v to dst, and trim if too long. */
#define CPY_TRIM_STRING_VAL(dst, v)                       \
  do {                                                    \
    strncpy(dst, String_val(v), sizeof(dst));             \
    dst[sizeof(dst) - 1] = '\0';                          \
  } while (0)

/* Copy string represented by the value v, unicode encoded, to dst, and trim
 * if too long. */
#define CPY_TRIM_USTRING_VAL(dst, v)                            \
  do {                                                          \
    EncodeUnicode(dst, String_val(v), (sizeof(dst) - 1) / 2);   \
    dst[sizeof(dst) - 1] = '\0';                                \
  } while (0)

char *dup_String_val(value v);

#define CPY_STRING_VAL(dst, v)                  \
  do {                                          \
    /* TODO: Possible memory leak here, check if we can free dst */ \
    dst = strdup(String_val(v));                \
    if (!dst)                                   \
      caml_raise_out_of_memory();               \
  } while (0)

/* Decode unicode strings ((unsigned char *) in gammu) to (char *). */
#define CAML_COPY_USTRING(str) caml_copy_string(DecodeUnicodeString(str))

#define CHAR_VAL(v) ((char) Int_val(v))
#define VAL_CHAR(c) (Val_int(c))
#define UCHAR_VAL(v) ((unsigned char) Int_val(v))
#define VAL_UCHAR(c) (Val_int(c))

/* Currently unused
static value option_val(value voption, gboolean *some);
*/
#define VAL_NONE (Val_int(0))
static value val_Some(value vsome);

#if GAMMU_VERSION_NUM < 12792
static gboolean is_true(const char *str);
static char *yesno_bool(gboolean b);
#endif


/************************************************************************/
/* Error handling */

#if ERR_LAST_VALUE > 75
#error "New errors defined in gammu-error.h, gammu.ml must be updated"
#endif

/* Error codes added by our bindings implementation.
   WARNING: must be in sync with the number of elements in the enumeration
            in gammu.ml
   WARNING: add error messages to caml_gammu_GSM_ErrorString */
typedef enum {
  ERR_INI_KEY_NOT_FOUND = 75, /* number in gammu.ml */
  ERR_COULD_NOT_DECODE,
  ERR_INVALID_CONFIG_NUM
} CAML_GAMMU_Error;

#define GSM_ERROR_VAL(v) (Int_val(v) + 2)
#define VAL_GSM_ERROR(v) Val_int(v - 2)
/* We do not report the first value ERR_NONE = 1 */

static void caml_gammu_raise_Error(int err);

value caml_gammu_GSM_ErrorString(value verr);


/************************************************************************/
/* Debugging handling */

/* A Debug.info value is either the global_debug pointer or the
   associated state machine. */
static GSM_Debug_Info *GSM_Debug_Info_val(value vdi);
/* di can be global_debug or a state machine value. */
#define VAL_GSM_DEBUG_INFO(di) ((value) di)

value caml_gammu_GSM_GetGlobalDebug(value vunit);

value caml_gammu_GSM_SetDebugGlobal(value vdi, value vglobal);

value caml_gammu_GSM_SetDebugFileDescriptor(value vdi, value vchannel);

value caml_gammu_GSM_SetDebugLevel(value vdi, value vlevel);


/************************************************************************/
/* INI files */

#define INI_SECTION_VAL(v) (*((INI_Section **) Data_custom_val(v)))

static void caml_gammu_ini_section_finalize(value vini_section);

static struct custom_operations caml_gammu_ini_section_ops = {
  "ml-gammu.Gammu.ini_section",
  caml_gammu_ini_section_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value alloc_INI_Section();

static value Val_INI_Section(INI_Section *ini_section);

value caml_gammu_INI_ReadFile(value vfilename, value vunicode);

value caml_gammu_INI_GetValue(value vfile_info, value vsection, value vkey,
                              value vunicode);


/************************************************************************/
/* State machine */

/* Define a struct to put, caml side, state machine related stuff in C heap in
   order to deal with GC. */
typedef struct {
  GSM_StateMachine *sm;
  value log_function;
  value incoming_SMS_callback;
  value incoming_Call_callback;
} State_Machine;

#define STATE_MACHINE_VAL(v) (*((State_Machine **) Data_custom_val(v)))
#define GSM_STATEMACHINE_VAL(v) (STATE_MACHINE_VAL(v)->sm)

static void caml_gammu_state_machine_finalize(value s);

static struct custom_operations caml_gammu_state_machine_ops = {
  "ml-gammu.Gammu.state_machine",
  caml_gammu_state_machine_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* TODO: Is it acceptable for a global root to be a pointer to NULL ? If so,
   we would only need to register state_machine->val as global root once at
   state machine allocation. */
#define REGISTER_SM_GLOBAL_ROOT(state_machine, field, v)        \
  do {                                                          \
    state_machine->field = v;                                   \
    if (!state_machine->field)                                  \
      caml_register_global_root(&state_machine->field);         \
  } while (0)

/* TODO: If it is acceptable for a global root to be a pointer to NULL, remove
   the caml_remove_global_root statement. */
#define UNREGISTER_SM_GLOBAL_ROOT(state_machine, field) \
  do {                                                  \
    if (state_machine->field) {                         \
      caml_remove_global_root(&state_machine->field);   \
      state_machine->field = 0;                         \
    }                                                   \
  } while(0)

static value Val_GSM_Config(const GSM_Config *config);

static void GSM_Config_val(GSM_Config *config, value vconfig);

#define VAL_GSM_CONNECTIONTYPE(ct) Val_int(ct - 1)

value caml_gammu_GSM_GetDebug(value s);

value caml_gammu_GSM_InitLocales(value vpath);

value caml_gammu_GSM_InitDefaultLocales();

value caml_gammu_GSM_AllocStateMachine(value vunit);

value caml_gammu_GSM_FindGammuRC_force(value vpath);

value caml_gammu_GSM_FindGammuRC(value vunit);

value caml_gammu_GSM_ReadConfig(value vcfg_info, value vnum);

value caml_gammu_GSM_GetConfig(value s, value vnum);

value caml_gammu_push_config(value s, value vcfg);

value caml_gammu_remove_config(value s);

value caml_gammu_GSM_GetConfigNum(value s);

value caml_gammu_GSM_InitConnection(value s, value vreply_num);

static void log_function_callback(const char *text, void *data);

value caml_gammu_GSM_InitConnection_Log(value s, value vreply_num,
                                       value vlog_func);

value caml_gammu_GSM_TerminateConnection(value s);

value caml_gammu_GSM_IsConnected(value s);

value caml_gammu_GSM_GetUsedConnection(value s);

value caml_gammu_GSM_ReadDevice(value s, value vwait_for_reply);


/************************************************************************/
/* Security related operations with phone */

#define GSM_SECURITYCODETYPE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_SECURITYCODETYPE(sct) Val_int(sct - 1)

value caml_gammu_GSM_EnterSecurityCode(value s, value vcode_type, value vcode);

value caml_gammu_GSM_GetSecurityStatus(value s);


/************************************************************************/
/* Informations on the phone */

#define CHARGESTATE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_CHARGESTATE(cs) Val_int(cs - 1)
#define GSM_BATTERYTYPE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_BATTERYTYPE(bt) Val_int(bt)
#define GSM_GPRS_STATE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_GPRS_STATE(gprss) Val_int(gprss - 1)
#define GSM_NETWORKINFO_STATE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_NETWORKINFO_STATE(nis) Val_int(nis - 1)

static value Val_GSM_BatteryCharge(GSM_BatteryCharge *battery_charge);

static value Val_GSM_PhoneModel(GSM_PhoneModel *phone_model);

static value Val_GSM_NetworkInfo(GSM_NetworkInfo *network);

static value Val_GSM_SignalQuality(GSM_SignalQuality *signal_quality);

#define CAML_GAMMU_GSM_STR_GET_PROTOTYPE(name, buf_length)      \
  CAMLexport                                                    \
  value caml_gammu_GSM_Get##name(value s)
#define CAML_GAMMU_GSM_STR_GET(name, buf_length)         \
  CAML_GAMMU_GSM_STR_GET_PROTOTYPE(name, buf_length)     \
  {                                                      \
    CAMLparam1(s);                                       \
    GSM_StateMachine *sm;                                \
    char val[buf_length] = "";                           \
    GSM_Error error;                                     \
    sm = GSM_STATEMACHINE_VAL(s);                        \
    caml_enter_blocking_section();                       \
    error = GSM_Get##name(sm, val);                      \
    caml_leave_blocking_section();                       \
    if (error != ERR_NOTSUPPORTED)                       \
      caml_gammu_raise_Error(error);                     \
    CAMLreturn(caml_copy_string(val));                   \
  }

#define CAML_GAMMU_GSM_TYPE_GET_PROTOTYPE(name) \
  value caml_gammu_GSM_Get##name(value s)

value caml_gammu_GSM_GetNetworkName(value vcode);

value caml_gammu_GSM_GetCountryName(value vcode);

CAML_GAMMU_GSM_TYPE_GET_PROTOTYPE(BatteryCharge);

value caml_gammu_GSM_GetFirmWare(value s);

CAML_GAMMU_GSM_STR_GET_PROTOTYPE(Hardware, BUFFER_LENGTH);

CAML_GAMMU_GSM_STR_GET_PROTOTYPE(IMEI, GSM_MAX_IMEI_LENGTH + 1);

CAML_GAMMU_GSM_STR_GET_PROTOTYPE(ManufactureMonth, BUFFER_LENGTH);

CAML_GAMMU_GSM_STR_GET_PROTOTYPE(
  Manufacturer, GSM_MAX_MANUFACTURER_LENGTH + 1);

CAML_GAMMU_GSM_STR_GET_PROTOTYPE(Model, GSM_MAX_MODEL_LENGTH + 1);

value caml_gammu_GSM_GetModelInfo(value s);

CAML_GAMMU_GSM_TYPE_GET_PROTOTYPE(NetworkInfo);

CAML_GAMMU_GSM_STR_GET_PROTOTYPE(ProductCode, BUFFER_LENGTH);

CAML_GAMMU_GSM_TYPE_GET_PROTOTYPE(SignalQuality);


/************************************************************************/
/* Date and time */

static GSM_DateTime *GSM_DateTime_val(
  GSM_DateTime *date_time, value vdate_time);

static value Val_GSM_DateTime(GSM_DateTime *date_time);

value caml_gammu_GSM_CheckDate(value vdate);

value caml_gammu_GSM_CheckTime(value vdate);

value caml_gammu_GSM_OSDate(value vdt);

value caml_gammu_GSM_OSDateTime(value vdt, value vtimezone);


/************************************************************************/
/* Memory */

#define GSM_MEMORYTYPE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_MEMORYTYPE(mt) Val_int(mt - 1)
#define VAL_GSM_ENTRYTYPE(et) Val_int(et - 1)

/* Currently unused
static value Val_GSM_SubMemoryEntry(GSM_SubMemoryEntry *sub_mem_entry);

static value Val_GSM_MemoryEntry(GSM_MemoryEntry *mem_entry);
*/


/************************************************************************/
/* Messages */

#define GSM_SMS_STATE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_SMS_STATE(sms_state) Val_int(sms_state - 1)
#define GSM_UDH_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_UDH(udh) Val_int(udh - 1)

static GSM_UDHHeader *GSM_UDHHeader_val(GSM_UDHHeader *udh_header,
                                        value vudh_header);

static value Val_GSM_UDHHeader(GSM_UDHHeader *udh_header);

static GSM_SMSValidity *GSM_SMSValidity_val(GSM_SMSValidity *validity,
                                            value vvalidity);

static value Val_GSM_SMSValidity(GSM_SMSValidity *validity);

#define GSM_SMSFORMAT_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_SMSFORMAT(v) Val_int(v - 1)

static GSM_SMSC *GSM_SMSC_val(GSM_SMSC *smsc, value vsmsc);

static value Val_GSM_SMSC(GSM_SMSC *smsc);

#define GSM_SMSMESSAGETYPE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_SMSMESSAGETYPE(mt) Val_int(mt - 1)
#define GSM_CODING_TYPE_VAL(v) (Int_val(v) + 1)
#define VAL_GSM_CODING_TYPE(ct) Val_int(ct - 1)

static GSM_SMSMessage *GSM_SMSMessage_val(GSM_SMSMessage *sms, value vsms);

static value Val_GSM_SMSMessage(GSM_SMSMessage *sms);

static GSM_MultiSMSMessage *GSM_MultiSMSMessage_val(
  value vmulti_sms, GSM_MultiSMSMessage *multi_sms);

static value Val_GSM_MultiSMSMessage(GSM_MultiSMSMessage *multi_sms);

value caml_gammu_GSM_GetSMS(value s, value vlocation, value vfolder);

value caml_gammu_GSM_GetNextSMS(value s, value vlocation, value vfolder,
                                value vstart);

value caml_gammu_GSM_SetSMS(value s, value vsms);

value caml_gammu_GSM_AddSMS(value s, value vsms);

value caml_gammu_GSM_SendSMS(value s, value vsms);

#define OUTBOX(outbox) (Val_int(outbox))

value caml_gammu_GSM_GetSMSFolders(value s);

static value Val_GSM_SMSMemoryStatus(GSM_SMSMemoryStatus *sms_mem);

value caml_gammu_GSM_GetSMSStatus(value s);

value caml_gammu_GSM_DeleteSMS(value s, value vlocation, value vfolder);

#define VAL_GSM_ENCODEMULTIPARTSMSID(v) Val_int(v - 1)

static value Val_GSM_MultiPartSMSEntry(GSM_MultiPartSMSEntry mult_part_sms);

static value Val_GSM_MultiPartSMSInfo(
  GSM_MultiPartSMSInfo *multipart_sms_info);

value caml_gammu_GSM_DecodeMultiPartSMS(value vdi, value vsms,
                                        value vems);


/************************************************************************/
/* Calls */

/* Currently unused
static GSM_CallStatus GSM_CallStatus_val(value vcall_status, int *param_int1);
*/
static value Val_GSM_CallStatus(GSM_CallStatus call_status, int *param_int1);
/* Currently unused
static GSM_Call *GSM_Call_val(GSM_Call *call, value vcall);
*/
static value Val_GSM_Call(GSM_Call *call);


/************************************************************************/
/* Events */

#if GAMMU_VERSION_NUM >= 12991
#define TYPE_MODIFIER1 *
#define TYPE_MODIFIER2
#else
#define TYPE_MODIFIER1
#define TYPE_MODIFIER2 &
#endif

#define CAML_GAMMU_GSM_SETINCOMING_PROTOTYPES(name, type)               \
  value caml_gammu_GSM_SetIncoming##name(value s, value venable);       \
  static void incoming_##name##_callback(GSM_StateMachine *sm,          \
                                         type TYPE_MODIFIER1 t,         \
                                         void *user_data);              \
  value caml_gammu_GSM_SetIncoming##name##Callback(value s, value vf)

CAML_GAMMU_GSM_SETINCOMING_PROTOTYPES(SMS, GSM_SMSMessage);

CAML_GAMMU_GSM_SETINCOMING_PROTOTYPES(Call, GSM_Call);


#endif /* __GAMMU_STUBS_H__ */
