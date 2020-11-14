/*
 * Copyright (C) 2018 Docker Inc
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
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/callback.h>

#ifdef __linux__
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/vm_sockets.h>
#endif

CAMLprim value stub_vsock_get_local_cid( value v_unit) {
  CAMLparam1(v_unit);
  unsigned int cid;
#ifdef AF_VSOCK
  int s = socket(AF_VSOCK, SOCK_STREAM, 0);
  if (s == -1) {
    uerror("socket", Nothing);
  }
  if (ioctl(s, IOCTL_VM_SOCKETS_GET_LOCAL_CID, &cid) == -1) {
    uerror("ioctl", Nothing);
  }
#else
  caml_failwith("AF_VSOCK not available");
#endif
  CAMLreturn(Val_int(cid));
}

CAMLprim value stub_vsock_socket(value v_unit){
  CAMLparam1(v_unit);
  int s;
#ifdef AF_VSOCK
  s = socket(AF_VSOCK, SOCK_STREAM, 0);
  if (s == -1){
    uerror("socket", Nothing);
  }
#else
  caml_failwith("AF_VSOCK not available");
#endif
  CAMLreturn(Val_int(s));
}

CAMLprim value stub_vsock_bind(value sock, value cid, value port) {
  CAMLparam3(sock, cid, port);
#ifdef AF_VSOCK
  struct sockaddr_vm sa;
  bzero(&sa, sizeof(sa));

  sa.svm_family = AF_VSOCK;
  sa.svm_port = Int_val(port);
  sa.svm_cid = Int_val(cid);

  int res = bind(Int_val(sock), (const struct sockaddr *)&sa, sizeof(sa));
  if (res == -1) {
    uerror("bind", Nothing);
  }
#else
  caml_failwith("AF_VSOCK not available");
#endif
  CAMLreturn(Val_unit);
}

CAMLprim value stub_vsock_accept(value sock){
  CAMLparam1(sock);
  CAMLlocal1(result);
  int lsock = Int_val(sock);
  int csock = -1;
#ifdef AF_VSOCK
  struct sockaddr_vm sac;
  socklen_t socklen = sizeof(sac);

  caml_release_runtime_system();
  csock = accept(lsock, (struct sockaddr *)&sac, &socklen);
  caml_acquire_runtime_system();
  if (csock == -1) {
    uerror("accept", Nothing);
  }

  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(csock));
  Store_field(result, 1, Val_int(sac.svm_cid));
  Store_field(result, 2, Val_int(sac.svm_port));
#else
  caml_failwith("AF_VSOCK not available");
#endif
  CAMLreturn(result);
}

CAMLprim value stub_vsock_connect(value sock, value cid, value port){
  CAMLparam3(sock, cid, port);
#ifdef AF_VSOCK
  struct sockaddr_vm sa;
  int fd = Int_val(sock);
  int res = -1;

  bzero(&sa, sizeof(sa));
  sa.svm_family = AF_VSOCK;
  sa.svm_cid = Int_val(cid);
  sa.svm_port = Int_val(port);
  
  caml_release_runtime_system();
  res = connect(fd, (const struct sockaddr *)&sa, sizeof(sa));
  caml_acquire_runtime_system();

  if (res == -1) {
    uerror("connect", Nothing);
  }
#else
  caml_failwith("AF_VSOCK not available");
#endif
  CAMLreturn(Val_unit);
}
