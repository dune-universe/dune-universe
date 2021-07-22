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

#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN // Exclude rarely-used stuff from Windows headers
#include <assert.h>
#include <windows.h>
#define STRSAFE_NO_CCH_FUNCTIONS
#include <strsafe.h>

#define CAML_NAME_SPACE
#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include <caml/threads.h>

static value cb_service_run = Val_unit;
static value cb_service_stop = Val_unit;
static char_os *s_service_name = NULL;

static void call_service_run(void) {
  assert(Val_unit != cb_service_run);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback_exn(cb_service_run, Val_unit);
  caml_release_runtime_system();
  caml_c_thread_unregister();
}

static void call_service_stop(void) {
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

static BOOL report_status(DWORD current_state, DWORD win32_exitcode,
                          DWORD wait_hint) {
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

static void stop_service() {
  report_status(SERVICE_STOP_PENDING, NO_ERROR, 1000);

  call_service_stop();
}

static void WINAPI service_ctrl_handler(DWORD ctrl_code) {
  if (ctrl_code == SERVICE_CONTROL_STOP) {
    stop_service();
  } else {
    report_status(service_status.dwCurrentState, NO_ERROR, 0);
  }
}

static void service_main(DWORD argc, WCHAR **argv) {
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

static void raise_error(const char_os *prefix, DWORD rc) {
  LPVOID buffer = NULL;
  char_os msg[2048] = {0};

  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL, rc, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPTSTR)&buffer, 0, NULL);
  StringCbPrintf(msg, sizeof(msg) / sizeof(msg[0]), L"%s: %s", prefix, buffer);
  LocalFree(buffer);

  caml_raise_with_arg(*caml_named_value("ocaml_winsvc_exn"),
                      caml_copy_string_of_os(msg));
}

CAMLprim value ocaml_winsvc_install(value v_name, value v_display, value v_text,
                                    value v_path) {
  CAMLparam4(v_name, v_display, v_text, v_path);

  SC_HANDLE handle_manager;
  SC_HANDLE handle_service;
  SERVICE_DESCRIPTION description;
  char_os *name, *display, *text, *path;
  DWORD rc;

  handle_manager = OpenSCManager(0, 0, SC_MANAGER_ALL_ACCESS);
  if (handle_manager == NULL) {
    raise_error(L"OpenSCManager", GetLastError());
  }

  name = caml_stat_strdup_to_os(String_val(v_name));
  display = caml_stat_strdup_to_os(String_val(v_display));
  path = caml_stat_strdup_to_os(String_val(v_path));

  handle_service =
      CreateService(handle_manager, name, display, SERVICE_ALL_ACCESS,
                    SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START,
                    SERVICE_ERROR_NORMAL, path, 0, 0, 0, 0, 0);
  rc = GetLastError();

  caml_stat_free(name);
  caml_stat_free(display);
  caml_stat_free(path);

  if (handle_service == NULL) {
    CloseServiceHandle(handle_manager);
    raise_error(L"CreateService", rc);
  }

  text = caml_stat_strdup_to_os(String_val(v_text));
  description.lpDescription = text;
  ChangeServiceConfig2(handle_service, SERVICE_CONFIG_DESCRIPTION,
                       &description);
  caml_stat_free(text);

  CloseServiceHandle(handle_service);
  CloseServiceHandle(handle_manager);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_winsvc_remove(value v_name) {
  CAMLparam1(v_name);

  SC_HANDLE handle_manager;
  SC_HANDLE handle_service;
  SERVICE_STATUS status;
  BOOL result;
  char_os *name;
  DWORD rc;

  handle_manager = OpenSCManager(0, 0, SC_MANAGER_ALL_ACCESS);
  if (handle_manager == NULL) {
    raise_error(L"OpenSCManager", GetLastError());
  }

  name = caml_stat_strdup_to_os(String_val(v_name));
  handle_service = OpenService(handle_manager, name, SERVICE_ALL_ACCESS);
  caml_stat_free(name);
  if (handle_service == NULL) {
    CloseServiceHandle(handle_manager);
    raise_error(L"OpenService", GetLastError());
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
  rc = GetLastError();

  CloseServiceHandle(handle_service);
  CloseServiceHandle(handle_manager);

  if (!result) {
    raise_error(L"DeleteService", rc);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_winsvc_run(value v_name, value v_run, value v_stop) {
  CAMLparam3(v_name, v_run, v_stop);
  BOOL result;
  char_os *s_name = caml_stat_strdup_to_os(String_val(v_name));
  SERVICE_TABLE_ENTRY dispatch_table[] = {
      {s_name, (LPSERVICE_MAIN_FUNCTION)service_main}, {0, 0}};
  DWORD rc;

  if (Val_unit != cb_service_run) {
    caml_stat_free(s_name);
    caml_raise_with_string(*caml_named_value("ocaml_winsvc_exn"),
                           "Already running");
  }

  s_service_name = s_name;

  cb_service_run = v_run;
  cb_service_stop = v_stop;
  caml_register_generational_global_root(&cb_service_run);
  caml_register_generational_global_root(&cb_service_stop);

  caml_release_runtime_system();
  result = StartServiceCtrlDispatcher(dispatch_table);
  rc = GetLastError();
  caml_acquire_runtime_system();

  caml_remove_generational_global_root(&cb_service_run);
  caml_remove_generational_global_root(&cb_service_stop);
  cb_service_run = Val_unit;
  cb_service_stop = Val_unit;

  s_service_name = NULL;
  caml_stat_free(s_name);

  if (!result) {
    if (rc == ERROR_FAILED_SERVICE_CONTROLLER_CONNECT)
      caml_failwith("ERROR_FAILED_SERVICE_CONTROLLER_CONNECT");
    else
      raise_error(L"StartServiceCtrlDispatcher", rc);
  }

  CAMLreturn(Val_unit);
}
