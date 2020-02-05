/*
 * Copyright (c) 2019, Christopher Zimmermann
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */


#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/unixsupport.h>
#include <caml/threads.h>

#ifndef Handle_val
#include <unistd.h>
#endif

CAMLprim value
ocaml_bigstring_unix_read(value vfd, value vba, value voff, value vlen)
{
    void *iobuf = ((char *)Caml_ba_data_val(vba)) + Unsigned_long_val(voff);
#ifdef Handle_val
    unsigned len;
    int err;

    if (Descr_kind_val(vfd) == KIND_SOCKET) {
      SOCKET s = Socket_val(vfd);
      caml_release_runtime_system();
      if ((err = recv(s, iobuf, Unsigned_int_val(vlen), 0)) == SOCKET_ERROR)
        err = WSAGetLastError();
      else {
        len = err;
        err = 0;
      }
      caml_acquire_runtime_system();
    } else {
      HANDLE h = Handle_val(vfd);
      caml_release_runtime_system();
      if (ReadFile(h, iobuf, Unsigned_int_val(vlen), &len, NULL))
        err = 0;
      else {
check_error:
	switch (err = GetLastError()) {
	  case ERROR_BROKEN_PIPE:
	    /* This is no error, but just a closed pipe. */
	    err = len = 0;
	    break;
	  case ERROR_MORE_DATA:
#if 1
	    do {
	      char buf[1024];
	      unsigned dummy_len;
	      if (ReadFile(h, buf, sizeof(buf), &dummy_len, NULL))
		break;
	      else
		goto check_error;
	    } while (0);
#else
	    err = 0;
#endif
	  default:
	    break;
	}
      }
      caml_acquire_runtime_system();
    }
    /* GetLastError() and WSAGetLastError() error numbers _are_ compatible,
     * although not documented this behaviour will hopefully never change. */
    if (err) {
      win32_maperr(err);
      uerror("Bigstringaf.read", Nothing);
    }
    else
      return Val_int(len);

#else
    ssize_t ret;

    caml_release_runtime_system();
    ret = read(Int_val(vfd), iobuf, Unsigned_long_val(vlen));
    caml_acquire_runtime_system();
    if (ret < 0) uerror("Bigstringaf.read", Nothing);
    return Val_long(ret);
#endif
}

CAMLprim value
ocaml_bigstring_unix_write(value vfd, value vba, value voff, value vlen)
{
    char *iobuf = ((char *)Caml_ba_data_val(vba)) + Unsigned_long_val(voff);
#ifdef Handle_val
    unsigned len;
    int err;

    if (Descr_kind_val(vfd) == KIND_SOCKET) {
      SOCKET s = Socket_val(vfd);
      caml_release_runtime_system();
      if ((err = send(s, iobuf, Unsigned_int_val(vlen), 0)) == SOCKET_ERROR)
        err = WSAGetLastError();
      else {
        len = err;
        err = 0;
      }
      caml_acquire_runtime_system();
    } else {
      HANDLE h = Handle_val(vfd);
      caml_release_runtime_system();
      if (WriteFile(h, iobuf, Unsigned_int_val(vlen), &len, NULL))
        err = 0;
      else
        err = GetLastError();
      caml_acquire_runtime_system();
    }
    /* GetLastError() and WSAGetLastError() error numbers _are_ compatible,
     * although not documented this behaviour will hopefully never change. */
    if (err) {
      win32_maperr(err);
      uerror("Bigstringaf.write", Nothing);
    }
    return Val_long(len);

#else
    ssize_t ret;

    caml_release_runtime_system();
    ret = write(Int_val(vfd), iobuf, Unsigned_long_val(vlen));
    caml_acquire_runtime_system();
    if (ret < 0) uerror("Bigstringaf.write", Nothing);
    return Val_long(ret);
#endif
}
