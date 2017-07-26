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
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compat.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <caml/callback.h>

#ifndef WIN32
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>
#include <sys/uio.h>
#else
#define ETIMEDOUT -WSAETIMEDOUT
#endif

/* Helper macros for parsing/printing GUIDs */
#define GUID_FMT "%08x-%04hx-%04hx-%02x%02x-%02x%02x%02x%02x%02x%02x"
#define GUID_ARGS(_g)                                               \
    (_g).Data1, (_g).Data2, (_g).Data3,                             \
    (_g).Data4[0], (_g).Data4[1], (_g).Data4[2], (_g).Data4[3],     \
    (_g).Data4[4], (_g).Data4[5], (_g).Data4[6], (_g).Data4[7]
#define GUID_SARGS(_g)                                              \
    &(_g).Data1, &(_g).Data2, &(_g).Data3,                          \
    &(_g).Data4[0], &(_g).Data4[1], &(_g).Data4[2], &(_g).Data4[3], \
    &(_g).Data4[4], &(_g).Data4[5], &(_g).Data4[6], &(_g).Data4[7]


static int parseguid(const char *s, GUID *g)
{
    int res;
    int p0, p1, p2, p3, p4, p5, p6, p7;

    res = sscanf(s, GUID_FMT,
                 &g->Data1, &g->Data2, &g->Data3,
                 &p0, &p1, &p2, &p3, &p4, &p5, &p6, &p7);
    if (res != 11)
        return 1;
    g->Data4[0] = p0;
    g->Data4[1] = p1;
    g->Data4[2] = p2;
    g->Data4[3] = p3;
    g->Data4[4] = p4;
    g->Data4[5] = p5;
    g->Data4[6] = p6;
    g->Data4[7] = p7;
    return 0;
}

static value Val_guid(GUID guid){
  char str[37];
  snprintf(str, 37, GUID_FMT, GUID_ARGS(guid));
  return caml_copy_string(str);
}

CAMLprim value stub_hvsock_wildcard(value v_unit){
  CAMLparam1(v_unit);
  CAMLreturn(Val_guid(HV_GUID_WILDCARD));
}

CAMLprim value stub_hvsock_children(value v_unit){
  CAMLparam1(v_unit);
  CAMLreturn(Val_guid(HV_GUID_CHILDREN));
}

CAMLprim value stub_hvsock_loopback(value v_unit){
  CAMLparam1(v_unit);
  CAMLreturn(Val_guid(HV_GUID_LOOPBACK));
}

CAMLprim value stub_hvsock_parent(value v_unit){
  CAMLparam1(v_unit);
  CAMLreturn(Val_guid(HV_GUID_PARENT));
}

CAMLprim value stub_hvsock_socket(value v_unit){
  SOCKET s = INVALID_SOCKET;
  s = socket(AF_HYPERV, SOCK_STREAM, HV_PROTOCOL_RAW);

  if (s == INVALID_SOCKET) {
    win32_maperr(WSAGetLastError());
    uerror("socket", Nothing);
  }
  return win_alloc_socket(s);
}

CAMLprim value stub_hvsock_bind(value sock, value vmid, value serviceid) {
  CAMLparam3(sock, vmid, serviceid);

  SOCKADDR_HV sa;
  sa.Family = AF_HYPERV;
  sa.Reserved = 0;
  if (parseguid(String_val(vmid), &sa.VmId) != 0) {
    caml_failwith("Failed to parse vmid");
  }
  if (parseguid(String_val(serviceid), &sa.ServiceId) != 0) {
    caml_failwith("Failed to parse serviceid");
  }

  int res;

  res = bind(Socket_val(sock), (const struct sockaddr *)&sa, sizeof(sa));
  if (res == SOCKET_ERROR) {
    win32_maperr(WSAGetLastError());
    uerror("bind", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value stub_hvsock_accept(value sock){
  CAMLparam1(sock);
  CAMLlocal1(result);
  SOCKET lsock = Socket_val(sock);
  SOCKET csock = INVALID_SOCKET;
  SOCKADDR_HV sac;
  socklen_t socklen = sizeof(sac);

  caml_release_runtime_system();
  csock = accept(lsock, (struct sockaddr *)&sac, &socklen);
  caml_acquire_runtime_system();
  if (csock == INVALID_SOCKET) {
    win32_maperr(WSAGetLastError());
    uerror("accept", Nothing);
  }

  result = caml_alloc_tuple(3);
  Store_field(result, 0, win_alloc_socket(csock));
  Store_field(result, 1, Val_guid(sac.VmId));
  Store_field(result, 2, Val_guid(sac.ServiceId));
  CAMLreturn(result);
}

CAMLprim value stub_hvsock_connect_blocking(value sock, value vmid, value serviceid){
  CAMLparam3(sock, vmid, serviceid);
  SOCKADDR_HV sa;
  SOCKET fd = Socket_val(sock);
  SOCKET res = INVALID_SOCKET;

  sa.Family = AF_HYPERV;
  sa.Reserved = 0;
  if (parseguid(String_val(vmid), &sa.VmId) != 0) {
    caml_failwith("Failed to parse vmid");
  }
  if (parseguid(String_val(serviceid), &sa.ServiceId) != 0) {
    caml_failwith("Failed to parse serviceid");
  }

  caml_release_runtime_system();
  res = connect(fd, (const struct sockaddr *)&sa, sizeof(sa));
  caml_acquire_runtime_system();

  if (res == SOCKET_ERROR) {
    win32_maperr(WSAGetLastError());
    uerror("connect", Nothing);
  }
  CAMLreturn(Val_unit);
}

#ifdef WIN32
CAMLprim value stub_hvsock_connect_nonblocking(value timeout_ms, value sock, value vmid, value serviceid){
  CAMLparam4(timeout_ms, sock, vmid, serviceid);
  SOCKADDR_HV sa;
  SOCKET fd = Socket_val(sock);
  SOCKET res = INVALID_SOCKET;
  fd_set fds;
  struct timeval timeout;
  unsigned long nonBlocking = 1, blocking = 0;
  DWORD err = 0;

  sa.Family = AF_HYPERV;
  sa.Reserved = 0;
  if (parseguid(String_val(vmid), &sa.VmId) != 0) {
    caml_failwith("Failed to parse vmid");
  }
  if (parseguid(String_val(serviceid), &sa.ServiceId) != 0) {
    caml_failwith("Failed to parse serviceid");
  }

  caml_release_runtime_system();
  ioctlsocket(fd, FIONBIO, &nonBlocking);
  res = connect(fd, (const struct sockaddr *)&sa, sizeof(sa));
  if (res == SOCKET_ERROR) err = WSAGetLastError();
  caml_acquire_runtime_system();

  if (res == SOCKET_ERROR) {
    if (err != WSAEWOULDBLOCK)
    {
      win32_maperr(err);
      uerror("connect", Nothing);
    } else {
      FD_ZERO(&fds);
		  FD_SET(fd, &fds);
      timeout.tv_sec = Int_val(timeout_ms) / 1000;
      timeout.tv_usec = (Int_val(timeout_ms) % 1000) * 1000;
      caml_release_runtime_system();
      res = select(1, NULL, &fds, NULL, &timeout);
      if (res == SOCKET_ERROR) err = WSAGetLastError();
      caml_acquire_runtime_system();
      if (res == SOCKET_ERROR) {
        win32_maperr(err);
        uerror("connect", Nothing);
      }
      if (res == 0) {
        /* Timeout */
        errno = ETIMEDOUT;
        uerror("connect", Nothing);
      }
    }
  }
  caml_release_runtime_system();
  ioctlsocket(fd, FIONBIO, &blocking);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}
#else
CAMLprim value stub_hvsock_connect_nonblocking(value timeout_ms, value sock, value vmid, value serviceid){
  CAMLparam4(timeout_ms, sock, vmid, serviceid);
  SOCKADDR_HV sa;
  SOCKET fd = Socket_val(sock);
  SOCKET res = INVALID_SOCKET;
  DWORD err;
  struct pollfd pollInfo = { 0 };
  int flags = fcntl(fd, F_GETFL, 0);

  pollInfo.fd = fd;
  pollInfo.events = POLLOUT;
  sa.Family = AF_HYPERV;
  sa.Reserved = 0;
  if (parseguid(String_val(vmid), &sa.VmId) != 0) {
    caml_failwith("Failed to parse vmid");
  }
  if (parseguid(String_val(serviceid), &sa.ServiceId) != 0) {
    caml_failwith("Failed to parse serviceid");
  }

  caml_release_runtime_system();
  fcntl(fd, F_SETFL, flags | O_NONBLOCK);
  res = connect(fd, (const struct sockaddr *)&sa, sizeof(sa));
  if (res == SOCKET_ERROR) err = errno;
  caml_acquire_runtime_system();

  if (res == SOCKET_ERROR) {
    if (err != EINPROGRESS)
    {
      uerror("connect", Nothing);
    }
    caml_release_runtime_system();
    res = poll(&pollInfo, 1, Int_val(timeout_ms));
    if (res == SOCKET_ERROR) err = WSAGetLastError();
    caml_acquire_runtime_system();

    if (res == -1)
    {
      win32_maperr(err);
      uerror("connect", Nothing);
    }
    if (res == 0)
    {
      /* Timeout */
      errno = ETIMEDOUT;
      uerror("connect", Nothing);
    }
  }
  caml_release_runtime_system();
  fcntl(fd, F_SETFL, flags);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}
#endif

CAMLprim value
stub_hvsock_ba_recv(value fd, value val_buf, value val_ofs, value val_len)
{
  CAMLparam4(fd, val_buf, val_ofs, val_len);
  int ret = 0;

  char *data = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  size_t c_len = Int_val(val_len);
  SOCKET s = Socket_val(fd);
  DWORD err = 0;

  caml_release_runtime_system();
  ret = recv(s, data, c_len, 0);
  if (ret == SOCKET_ERROR) err = WSAGetLastError();
  caml_acquire_runtime_system();

  if (ret == SOCKET_ERROR) {
    win32_maperr(err);
    uerror("read", Nothing);
  }

  CAMLreturn(Val_int(ret));
}

CAMLprim value
stub_hvsock_ba_send(value fd, value val_buf, value val_ofs, value val_len)
{
  CAMLparam4(fd, val_buf, val_ofs, val_len);
  int ret = 0;
  char *data = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  size_t c_len = Int_val(val_len);
  SOCKET s = Socket_val(fd);
  DWORD err = 0;

  caml_release_runtime_system();
  ret = send(s, data, c_len, 0);
  if (ret == SOCKET_ERROR) err = WSAGetLastError();
  caml_acquire_runtime_system();

  if (ret == SOCKET_ERROR) {
    win32_maperr(err);
    uerror("write", Nothing);
  }
  CAMLreturn(Val_int(ret));
}

CAMLprim value
stub_hvsock_ba_sendv(value fd, value val_list)
{
  CAMLparam2(fd, val_list);
  CAMLlocal5(next, head, val_buf, val_ofs, val_len);
  SOCKET s = Socket_val(fd);
  DWORD err = 0;
  DWORD sent = 0;
  int ret = 0;
  int length = 0;
  int i;
  next = val_list;
  for (next = val_list; next != Val_emptylist; next = Field(next, 1))
    length++;
#ifdef WIN32
  LPWSABUF iovec = (WSABUF*)malloc(sizeof(WSABUF) * length);
#else
  struct iovec *iovec = (struct iovec*)malloc(sizeof(struct iovec) * length);
#endif
  next = val_list;
  for (i = 0; i < length; i ++) {
    head = Field(next, 0);
    val_buf = Field(head, 0);
    val_ofs = Field(head, 1);
    val_len = Field(head, 2);
#ifdef WIN32
    iovec[i].buf = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
    iovec[i].len = Long_val(val_len);
#else
    iovec[i].iov_base = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
    iovec[i].iov_len = Long_val(val_len);
#endif
    next = Field(next, 1);
  }
  caml_release_runtime_system();
#ifdef WIN32
  ret = WSASend(s, iovec, length, &sent, 0, NULL, NULL);
#else
  ret = writev(s, iovec, length);
  if (ret != SOCKET_ERROR) sent = ret;
#endif
  if (ret == SOCKET_ERROR) err = WSAGetLastError();
  free(iovec);
  caml_acquire_runtime_system();

  if (ret == SOCKET_ERROR) {
    win32_maperr(err);
    uerror("write", Nothing);
  }
  CAMLreturn(Val_int(sent));
}
