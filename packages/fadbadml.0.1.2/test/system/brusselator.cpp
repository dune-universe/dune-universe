/**************************************************************************/
/*                                                                        */
/*                                FADBADml                                */
/*                                                                        */
/*           OCaml port by François Bidet and Ismail Bennani              */
/*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      */
/*                                                                        */
/*                          Copyright 2019-2020                           */
/*                                                                        */
/*   This file is distributed under the terms of the CeCILL-C license.    */
/*                                                                        */
/**************************************************************************/

#include <iostream>
using namespace std;

template<typename T>
void brusselator(const T x, const T y, T &xp, T &yp)
{
  xp = 1 + x * x * y - 2.7 * x;
  yp = 1.7 * x - x * x * y;
}

template<typename T>
struct myvec {
  T x, y;
  myvec() : x(0), y(0) {};
  myvec(const myvec<T> &v) : x(v.x), y(v.y) {};
  myvec(T x, T y) : x(x), y(y) {};
};

template<typename T>
void euler(myvec<T> &v, double tstep) {
  T xp, yp;
  brusselator(v.x, v.y, xp, yp);
  v.x = v.x + tstep * xp;
  v.y = v.y + tstep * yp;
}
