/* This file is part of mlfenv.

  mlfenv is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  mlfenv is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with mlfenv. If not, see
 <http://www.gnu.org/licenses/>. */

#ifndef MLFENV_STUBS_H
#define MLFENV_STUBS_H

#include <caml/fail.h>
#include <caml/memory.h>
#include <fenv.h>

static int
rnd_val (value r)
{
  switch (Long_val (r))
    {
    case 0:
      return FE_TONEAREST;
    case 1:
      return FE_UPWARD;
    case 2:
      return FE_DOWNWARD;
    case 3:
      return FE_TOWARDZERO;
    default:
      caml_failwith (__FUNCTION__);
    }
}

static value
val_rnd (int r)
{
  CAMLparam0 ();
  CAMLlocal1 (rnd);

  switch (r)
    {
    case FE_TONEAREST:
      rnd = 0;
      break;
    case FE_UPWARD:
      rnd = 1;
      break;
    case FE_DOWNWARD:
      rnd = 2;
      break;
    case FE_TOWARDZERO:
      rnd = 3;
      break;
    default:
      caml_failwith (__FUNCTION__);
    }

  CAMLreturn (Val_int (rnd));
}

#endif
