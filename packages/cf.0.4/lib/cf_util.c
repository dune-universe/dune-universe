#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <CoreServices/CoreServices.h>

void caml_cf_run_loop_run() {
  caml_release_runtime_system();
  CFRunLoopRun();
  caml_acquire_runtime_system();
}

// On OS X 10.10 and earlier, the return type was SInt32
SInt32 caml_cf_run_loop_run_in_mode
(CFStringRef mode, CFTimeInterval seconds, Boolean returnAfterSourceHandled) {
  SInt32 r;
  caml_release_runtime_system();
  r = CFRunLoopRunInMode(mode, seconds, returnAfterSourceHandled);
  caml_acquire_runtime_system();
  return r;
}
