/*
        SVNService
        Written by Magnus Norddahl

        This code is in the public domain and is provided as-is without
        warranty of any kind, either expressed or implied, including but not
        limited to killing your cat or burning down your PC.
*/

/*
  Adapted for ocaml by ygrek, (c) 2009
*/

#define WIN32_LEAN_AND_MEAN // Exclude rarely-used stuff from Windows headers
#include <assert.h>
#include <stdio.h>
#include <tchar.h>
#include <windows.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>

static value cb_service_run = Val_unit;
static value cb_service_stop = Val_unit;
static char *s_service_name = NULL;

void call_service_run(void) {
  assert(Val_unit != cb_service_run);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback_exn(cb_service_run, Val_unit);
  caml_release_runtime_system();
  caml_c_thread_unregister();
}

void call_service_stop(void) {
  assert(Val_unit != cb_service_stop);

  caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback_exn(cb_service_stop, Val_unit);
  caml_release_runtime_system();
  caml_c_thread_unregister();
}

static SERVICE_STATUS service_status;	
static SERVICE_STATUS_HANDLE handle_service_status = 0;	
static int check_point = 1;

BOOL report_status(DWORD current_state, DWORD win32_exitcode, DWORD wait_hint) {
  if (current_state != SERVICE_START_PENDING)
    service_status.dwControlsAccepted = SERVICE_ACCEPT_STOP;
  service_status.dwCurrentState = current_state;
  service_status.dwWin32ExitCode = win32_exitcode;
  service_status.dwWaitHint = wait_hint;
  switch (current_state) {
  case SERVICE_RUNNING:
  case SERVICE_STOPPED:
    break;
  default:
    service_status.dwCheckPoint = check_point++;
  }
  return SetServiceStatus(handle_service_status, &service_status);
}

void stop_service() {
  report_status(SERVICE_STOP_PENDING, NO_ERROR, 1000);

  call_service_stop();
}

void WINAPI service_ctrl_handler(DWORD ctrl_code) {
  if (ctrl_code == SERVICE_CONTROL_STOP) {
    stop_service();
  } else {
    report_status(service_status.dwCurrentState, NO_ERROR, 0);
  }
}

void service_main(DWORD argc, TCHAR **argv) {
  memset(&service_status, 0, sizeof(SERVICE_STATUS));
  service_status.dwServiceType = SERVICE_WIN32_OWN_PROCESS;
  service_status.dwServiceSpecificExitCode = 0;
  handle_service_status =
      RegisterServiceCtrlHandler(s_service_name, service_ctrl_handler);

  report_status(SERVICE_START_PENDING, NO_ERROR, 2000);
  report_status(SERVICE_RUNNING, NO_ERROR, 0);

  call_service_run();

  report_status(SERVICE_STOPPED, NO_ERROR, 2000);
}

#define raise_error(str) caml_raise_with_string(*caml_named_value("caml_service_exn"), str)

CAMLprim value caml_winsvc_install(value v_name, value v_display, value v_text,
                                    value v_path) {
  CAMLparam4(v_name, v_display, v_text, v_path);

  SC_HANDLE handle_manager;
  SC_HANDLE handle_service;
  SERVICE_DESCRIPTION description;

  handle_manager = OpenSCManager(0, 0, SC_MANAGER_ALL_ACCESS);
  if (handle_manager == 0) {
    raise_error("Failed to open service control manager");
  }

  handle_service = CreateService(
      handle_manager, String_val(v_name), String_val(v_display),
      SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START,
      SERVICE_ERROR_NORMAL, String_val(v_path), 0, 0, 0, 0, 0);
  if (handle_service == 0) {
    CloseServiceHandle(handle_manager);
    raise_error("Failed to create service in service control manager");
  }

  description.lpDescription = String_val(v_text);
  ChangeServiceConfig2(handle_service, SERVICE_CONFIG_DESCRIPTION,
                       &description);

  CloseServiceHandle(handle_service);
  CloseServiceHandle(handle_manager);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_winsvc_remove(value v_name) {
  CAMLparam1(v_name);

  SC_HANDLE handle_manager;
  SC_HANDLE handle_service;
  SERVICE_STATUS status;
  BOOL result;

  handle_manager = OpenSCManager(0, 0, SC_MANAGER_ALL_ACCESS);
  if (handle_manager == 0) {
    raise_error("Failed to open service control manager");
  }

  handle_service =
      OpenService(handle_manager, String_val(v_name), SERVICE_ALL_ACCESS);
  if (handle_service == 0) {
    CloseServiceHandle(handle_manager);
    raise_error("Failed to open service in service control manager");
  }

  memset(&status, 0, sizeof(SERVICE_STATUS));
  if (ControlService(handle_service, SERVICE_CONTROL_STOP, &status)) {
    Sleep(1000);
    while (QueryServiceStatus(handle_service, &status)) {
      if (status.dwCurrentState != SERVICE_STOP_PENDING)
        break;
      Sleep(1000);
    }
  }

  result = DeleteService(handle_service);

  CloseServiceHandle(handle_service);
  CloseServiceHandle(handle_manager);

  if (0 == result) {
    raise_error("Failed to remove service");
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_winsvc_run(value v_name, value v_run, value v_stop) {
  CAMLparam3(v_name, v_run, v_stop);
  BOOL result;
  // not sure whether it is needed but better stay on the safe side
  char *s_name = strdup(String_val(v_name));
  SERVICE_TABLE_ENTRY dispatch_table[] = {
      {s_name, (LPSERVICE_MAIN_FUNCTION)service_main}, {0, 0}};

  if (Val_unit != cb_service_run) {
    free(s_name);
    raise_error("Already running");
  }

  s_service_name = s_name;

  cb_service_run = v_run;
  cb_service_stop = v_stop;
  caml_register_generational_global_root(&cb_service_run);
  caml_register_generational_global_root(&cb_service_stop);

  caml_release_runtime_system();
  result = StartServiceCtrlDispatcher(dispatch_table);
  caml_acquire_runtime_system();

  caml_remove_generational_global_root(&cb_service_run);
  caml_remove_generational_global_root(&cb_service_stop);
  cb_service_run = Val_unit;
  cb_service_stop = Val_unit;

  s_service_name = NULL;

  free(s_name);

  if (FALSE == result)
    raise_error("Failed to run service");

  CAMLreturn(Val_unit);
}
