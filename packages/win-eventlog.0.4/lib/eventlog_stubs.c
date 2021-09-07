/*
 * Copyright (C) 2016 Docker Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <stdio.h>
#include <string.h>

#ifdef WIN32
#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>
#else
/* For debugging on Unix */
#define HANDLE int*
#define BOOL int
#define TRUE 1
#define FALSE 0
#warning "This isn't a Windows platform: there is no real eventlog support here"
BOOL CloseHandle(HANDLE h){
  fprintf(stderr, "CloseHandle %lx\n", (unsigned long int)h);
  return TRUE;
}
#define LPCTSTR const char*
#define DWORD int
DWORD GetLastError(){
  return 0;
}
HANDLE RegisterEventSource(LPCTSTR lpUNCServerName, LPCTSTR lpSourceName){
  fprintf(stderr, "RegisterEventSource(%s, %s)\n", lpUNCServerName, lpSourceName);
  return (HANDLE)1;
}
void win32_maperr(DWORD err){
  return;
}
#define WORD int
#define PSID void*
#define LPVOID void*
BOOL ReportEvent(HANDLE hEventLog, WORD wType, WORD wCategory, DWORD dwEventID,
  PSID lpUserSid, WORD wNumStrings, DWORD dwDataSize, LPCTSTR *lpStrings, LPVOID lpRawData
){
  fprintf(stderr, "ReportEvent type=%d category=%d eventid=%d numstrings=%d\n",
    wType, wCategory, dwEventID, wNumStrings);
  int i = 0;
  for (i = 0; i < wNumStrings; i ++) {
    fprintf(stderr, "%d: %s\n", i, *(lpStrings + i));
  }
  return TRUE;
}
#endif

#define CAML_NAME_SPACE
#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/osdeps.h>

#if CAML_VERSION < 41200
#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
#define Tag_some 0
#define Is_none(v) ((v) == Val_none)
#define Is_some(v) Is_block(v)
#endif

#define Eventlog_val(v) (*((HANDLE *) Data_custom_val(v)))

static void eventlog_finalize(value v) {
  CloseHandle(Eventlog_val(v));
}

static struct custom_operations eventlog_ops = {
  .identifier = "mirage.eventlog",
  .finalize = eventlog_finalize,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default,
  .compare_ext = custom_compare_ext_default,
  .fixed_length = NULL,
};

static value alloc_eventlog(HANDLE h) {
  value v = caml_alloc_custom(&eventlog_ops, sizeof(HANDLE), 0, 1);
  Eventlog_val(v) = h;
  return v;
}

CAMLprim value stub_register_event_source(value server_opt, value source) {
  CAMLparam2(server_opt, source);
  char_os *lpUNCServerName = NULL;
  char_os *lpSourceName = NULL;
  DWORD error = 0;
  HANDLE h = NULL;
  if (Is_some(server_opt)) {
    lpUNCServerName = caml_stat_strdup_to_os(String_val(Some_val(server_opt)));
  }
  lpSourceName = caml_stat_strdup_to_os(String_val(source));

  caml_release_runtime_system();
  h = RegisterEventSource(lpUNCServerName, lpSourceName);
  if (h == NULL) {
    error = GetLastError();
  }
  caml_acquire_runtime_system();

  caml_stat_free(lpUNCServerName);
  caml_stat_free(lpSourceName);
  if (h == NULL) {
    win32_maperr(error);
    uerror("RegisterEventSource", Nothing);
  }
  CAMLreturn(alloc_eventlog(h));
}

CAMLprim value stub_report_event(value eventlog, value type, value category, value event, value strings) {
  CAMLparam5(eventlog, type, category, event, strings);
  HANDLE hEventLog = Eventlog_val(eventlog);
  WORD wType = Int_val(type);
  WORD wCategory = Int_val(category);
  DWORD dwEventID = Int_val(event);
  WORD wNumStrings = Wosize_val(strings);
  char_os **lpStrings = malloc(wNumStrings * sizeof(char *));
  int i = 0;
  for (i = 0; i < wNumStrings; i++){
    lpStrings[i] = caml_stat_strdup_to_os(String_val(Field(strings, i)));
  }

  caml_release_runtime_system();
  BOOL result = ReportEvent(hEventLog, wType, wCategory, dwEventID, NULL,
    wNumStrings, 0, (const char_os **)lpStrings, NULL);
  DWORD error = 0;
  if (!result){
    error = GetLastError();
  }
  caml_acquire_runtime_system();

  for (i = 0; i < wNumStrings; i++){
    caml_stat_free(lpStrings[i]);
  }
  free((void*)lpStrings);
  if (!result) {
    win32_maperr(error);
    uerror("ReportEvent", Nothing);
  }
  CAMLreturn(Val_unit);
}
