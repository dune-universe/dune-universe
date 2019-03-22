/* File: mindstorm_win.c

   Copyright (C) 2007

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */

#include <caml/config.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/unixsupport.h>

#include <windows.h>
#include <stdio.h>

#ifdef __CYGWIN__
extern void win32_maperr(DWORD errcode);
extern value win_alloc_handle(HANDLE);
#endif

CAMLexport
value ocaml_mindstorm_connect(value vdest)
{
  /* noalloc */
  HANDLE h;
  DCB serial_params = {0};
  COMMTIMEOUTS timeouts={0};

  h = CreateFile(String_val(vdest), GENERIC_READ | GENERIC_WRITE,
                 0, NULL, OPEN_EXISTING,
                 FILE_FLAG_WRITE_THROUGH |FILE_ATTRIBUTE_SYSTEM
                 | FILE_FLAG_NO_BUFFERING, 0);

  /* Error functions available in windows unix.cm[x]a */
  if (h == INVALID_HANDLE_VALUE) {
    win32_maperr(GetLastError());
    uerror("Mindstorm.*.connect_bluetooth", vdest);
  }
  /* Set port parameters  */
  serial_params.DCBlength= sizeof(serial_params);
  if (!GetCommState(h, &serial_params)) {
    uerror("Mindstorm.*.connect_bluetooth (get port params)", vdest);
  }
  serial_params.BaudRate = CBR_19200;
  serial_params.ByteSize = 8;
  serial_params.StopBits = ONESTOPBIT;
  serial_params.Parity   = NOPARITY;
  if(!SetCommState(h, &serial_params)){
    uerror("Mindstorm.*.connect_bluetooth (set port params)", vdest);
  }
  /* Set timeouts (in milliseconds) */
  timeouts.ReadIntervalTimeout        = 50;
  timeouts.ReadTotalTimeoutConstant   = 50;
  timeouts.ReadTotalTimeoutMultiplier = 10;
  timeouts.WriteTotalTimeoutConstant  = 50;
  timeouts.WriteTotalTimeoutMultiplier= 10;
  if(!SetCommTimeouts(h, &timeouts)){
    uerror("Mindstorm.*.connect_bluetooth (set timeouts)", vdest);    
  }

  return win_alloc_handle(h);
}
