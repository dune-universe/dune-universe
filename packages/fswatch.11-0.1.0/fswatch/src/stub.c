/*
 * stub.c
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of ocaml-fswatch.
 */


#include <libfswatch.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/threads.h>
#include <stdint.h>
 
value of_fsw_cevent(fsw_cevent const * const cevent) {
    CAMLparam0();
    CAMLlocal2(flags, event);
    flags= caml_alloc(cevent->flags_num, 0);
    for (int i = 0; i < cevent->flags_num; ++i) {
        Store_field(flags, i, Val_int(*(cevent->flags + i)));
    }

    event= caml_alloc(3, 0);
    Store_field(event, 0, caml_copy_string(cevent->path));
    Store_field(event, 1, caml_copy_double(cevent->evt_time));
    Store_field(event, 2, flags);
    CAMLreturn(event);
}

void cevent_callback(fsw_cevent const * const cevents, const unsigned int cevent_num, void *cdata) {
    CAMLparam0();
    CAMLlocal2(session, events);
    caml_acquire_runtime_system();
    session= caml_copy_nativeint((intnat)cdata);
    events= caml_alloc(cevent_num, 0);
    for (int i = 0; i < cevent_num; ++i) {
        Store_field(events, i, of_fsw_cevent(cevents + i));
    }
    caml_callback2(*caml_named_value("callback"), session, events);
    caml_release_runtime_system();
    CAMLreturn0;
}

CAMLprim FSW_STATUS fsw_init_library_stub() {
    return Val_int(fsw_init_library());
}

CAMLprim value fsw_init_session_stub(value type) {
    CAMLparam1(type);
    FSW_HANDLE session= fsw_init_session(Int_val(type));
    fsw_set_callback(
        session,
        cevent_callback,
        (void*)session);
    CAMLreturn(caml_copy_nativeint((uintptr_t) session));
}

CAMLprim value fsw_add_path_stub(value handle, value path) {
    CAMLparam2(handle, path);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_add_path(session, String_val(path));
    CAMLreturn(Val_int(status));
}

CAMLprim value fsw_add_property_stub(value handle, value name, value val) {
    CAMLparam3(handle, name, val);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_add_property(session, String_val(name), String_val(val));
    CAMLreturn(Val_int(status));
}

CAMLprim value fsw_set_allow_overflow_stub(value handle, value allow_overflow) {
    CAMLparam2(handle, allow_overflow);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_set_allow_overflow(session, Bool_val(allow_overflow));
    CAMLreturn(Val_int(status));
}

/*
CAMLprim value fsw_set_callback_stub (value handle, value callback) {
    CAMLparam2(handle, callback);
    ref_callback= callback;
    caml_register_global_root(&ref_callback);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_set_callback(session, cevent_callback, (void*)callback);
    CAMLreturn(Val_int(status));
}
*/

CAMLprim value fsw_set_latency_stub (value handle, value latency) {
    CAMLparam2(handle, latency);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_set_latency(session, Double_val(latency));
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_set_recursive_stub (value handle, value recursive) {
    CAMLparam2(handle, recursive);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_set_recursive(session, Bool_val(recursive));
    CAMLreturn(Val_int(status));
}

CAMLprim value fsw_set_directory_only_stub (value handle, value directory_only) {
    CAMLparam2(handle, directory_only);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_set_directory_only(session, Bool_val(directory_only));
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_set_follow_symlinks_stub (value handle, value follow_symlinks) {
    CAMLparam2(handle, follow_symlinks);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_set_follow_symlinks(session, Bool_val(follow_symlinks));
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_add_event_type_filter_stub (value handle, value event_type) {
    CAMLparam2(handle, event_type);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    fsw_event_type_filter type_filter;
    type_filter.flag= Int_val(Field(event_type, 0));
    FSW_STATUS status= fsw_add_event_type_filter(session, type_filter);
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_add_filter_stub (value handle, value monitor_filter) {
    CAMLparam2(handle, monitor_filter);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    fsw_cmonitor_filter cmonitor_filter;
    cmonitor_filter.text= String_val(Field(monitor_filter, 0));
    cmonitor_filter.type= Int_val(Field(monitor_filter, 1));
    cmonitor_filter.case_sensitive= Int_val(Field(monitor_filter, 2));
    cmonitor_filter.extended= Int_val(Field(monitor_filter, 3));
    FSW_STATUS status= fsw_add_filter(session, cmonitor_filter);
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_start_monitor_stub (value handle) {
    CAMLparam1(handle);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    caml_release_runtime_system();
    FSW_STATUS status= fsw_start_monitor(session);
    caml_acquire_runtime_system();
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_stop_monitor_stub (value handle) {
    CAMLparam1(handle);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_stop_monitor(session);
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_is_running_stub (value handle) {
    CAMLparam1(handle);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    bool status= fsw_is_running(session);
    CAMLreturn(Val_bool(status));
}
 
CAMLprim value fsw_destroy_session_stub (value handle) {
    CAMLparam1(handle);
    FSW_HANDLE session= (void*)Nativeint_val(handle);
    FSW_STATUS status= fsw_destroy_session(session);
    CAMLreturn(Val_int(status));
}
 
CAMLprim value fsw_last_error_stub() {
    return Val_int(fsw_last_error_stub());
}
 
CAMLprim value fsw_is_verbose_stub () {
    return Val_bool(fsw_is_verbose());
}
 
CAMLprim void fsw_set_verbose_stub(value verbose) {
    CAMLparam1(verbose);
    fsw_set_verbose(Bool_val(verbose));
    CAMLreturn0;
}

