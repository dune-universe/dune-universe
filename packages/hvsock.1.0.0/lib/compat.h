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

/*
 * Compatibility layer between Windows and Linux
 */
#ifdef WIN32
#undef UNICODE
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>
#include <ws2tcpip.h>
/* from hvsocket.h */
#define AF_HYPERV 34
#define HV_PROTOCOL_RAW 1

typedef struct _SOCKADDR_HV
{
    ADDRESS_FAMILY Family;
    USHORT Reserved;
    GUID VmId;
    GUID ServiceId;
} SOCKADDR_HV;

#pragma comment (lib, "Ws2_32.lib")
#pragma comment (lib, "Mswsock.lib")
#pragma comment (lib, "AdvApi32.lib")

#else
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/socket.h>
#endif /* !WIN32 */

#ifdef WIN32
typedef int socklen_t;
#endif


#ifndef WIN32
/* Compat layer for Linux/Unix */
typedef int SOCKET;
typedef unsigned int DWORD;

#ifndef SOCKET_ERROR
#define SOCKET_ERROR -1
#endif

#ifndef INVALID_SOCKET
#define INVALID_SOCKET -1

/* OCaml FFI windows helpers */
#define win32_maperr(s) /* s */
#define WSAGetLastError(s) 0
#define win_alloc_socket(s) Val_int(s)
#define Handle_val(x) Int_val(x)
#define Socket_val(x) Int_val(x)
#endif

#define closesocket(_fd) close(_fd)

/* Shutdown flags are different too */
#define SD_SEND    SHUT_WR
#define SD_RECEIVE SHUT_RD
#define SD_BOTH    SHUT_RDWR

#define __cdecl

/* GUID handling  */
typedef struct _GUID {
    uint32_t Data1;
    uint16_t Data2;
    uint16_t Data3;
    uint8_t  Data4[8];
} GUID;

/* HV Socket definitions */
#define AF_HYPERV 43
#define HV_PROTOCOL_RAW 1

typedef struct _SOCKADDR_HV
{
    unsigned short Family;
    unsigned short Reserved;
    GUID VmId;
    GUID ServiceId;
} SOCKADDR_HV;

#endif

#define MAKE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
    const GUID name = {l, w1, w2, {b1, b2,  b3,  b4,  b5,  b6,  b7,  b8}}

MAKE_GUID(HV_GUID_ZERO,
    0x00000000, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00);
MAKE_GUID(HV_GUID_BROADCAST,
    0xFFFFFFFF, 0xFFFF, 0xFFFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF);
MAKE_GUID(HV_GUID_WILDCARD,
    0x00000000, 0x0000, 0x0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00);

MAKE_GUID(HV_GUID_CHILDREN,
    0x90db8b89, 0x0d35, 0x4f79, 0x8c, 0xe9, 0x49, 0xea, 0x0a, 0xc8, 0xb7, 0xcd);
MAKE_GUID(HV_GUID_LOOPBACK,
    0xe0e16197, 0xdd56, 0x4a10, 0x91, 0x95, 0x5e, 0xe7, 0xa1, 0x55, 0xa8, 0x38);
MAKE_GUID(HV_GUID_PARENT,
    0xa42e7cda, 0xd03f, 0x480c, 0x9c, 0xc2, 0xa4, 0xde, 0x20, 0xab, 0xb8, 0x78);
