/*
 * Copyright (C) 2015 Unikernel Systems
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

#include <launch.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* From the OCaml Unix package */
extern void unix_error(int errcode, char *cmdname, value cmdarg);

CAMLprim value stub_launch_activate_socket(value name) {
  CAMLparam1(name);
  CAMLlocal1(result);
  const char *c_name = caml_strdup(String_val(name));
  int *listening_fds = NULL;
  size_t n_listening_fds = 0;
  int err;

  caml_release_runtime_system();
  err = launch_activate_socket(c_name, &listening_fds, &n_listening_fds);
  caml_acquire_runtime_system();

  caml_stat_free((void*)c_name);

  switch (err) {
    case 0:
      result = caml_alloc_tuple(n_listening_fds);
      for (int i = 0; i < n_listening_fds; i++) {
        Store_field(result, i, Val_int(*(listening_fds + i)));
      }
      break;
    default:
      unix_error(err, "launch_activate_socket", name);
      break;
  }
  CAMLreturn(result);
}
