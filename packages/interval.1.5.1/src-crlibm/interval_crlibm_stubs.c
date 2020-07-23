/*
    Copyright 2018- Christophe Troestler

    This file is part of the ocaml interval library.

    The ocaml interval library is free software:
    you can redistribute it and/or modify it under the terms of
    the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The ocaml interval library is distributed in the hope that it will be
    useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with the ocaml interval library.
    If not, see <http://www.gnu.org/licenses/>.
*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLexport value interval_is_odd(double x)
{
  /* noalloc */
  union { uint64_t i; double d; } u;
  int e;  /* exponent (biased) */
  u.d = x;
  e = ((u.i >> 52) & 0x7ff);
  /* Assume [x] is an integer: not NaN nor infinite and e >= 0 */
  if (e == 0 || e > 52 + 0x3ff) return Val_false;
  if (e == 0x3ff) return Val_true;
  return Val_bool(((u.i & 0xFFFFFFFFFFFFF) >> (52 + 0x3ff - e)) & 0x1);
}

CAMLexport double interval_is_odd_bc(value vx)
{
  return interval_is_odd(Double_val(vx));
}
