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

#include "tadiff.h"
#include "common.cpp"
#include "brusselator.cpp"
#include <iostream>
#include <sys/time.h>
using namespace std;
using namespace fadbad;

struct tad_values {
  double t;
  double x;
  double y;
  vector<double> dxdt;
  vector<double> dydt;
};

void print_tad_values(const string name, const tad_values values) {
  cout << "\"" << name << "\": {" << endl;
  print_double("t", values.t);
  cout << "," << endl;
  print_double("x", values.x);
  cout << "," << endl;
  print_double("y", values.y);
  cout << "," << endl;
  print_double_vector("dx/dt", values.dxdt);
  cout << "," << endl;
  print_double_vector("dy/dt", values.dydt);
  cout << endl << "}";
}

void print_tad_res(result<tad_values> res) {
  print_res(*print_tad_values, res);
}

void main_tad(int nsteps, double dt, int ncoeff, result<tad_values> &res) {
  T<double> x, y;
  x = 1; y = 1;
  myvec< T<double> > v(x, y);

  double t = 0;
  timeval tv_start, tv_end;

  gettimeofday(&tv_start, NULL);
  for (int i = 0; i < nsteps; i++) {
    euler(v, dt);
    t += dt;
  }

  x[1] = 1; y[1] = 1;
  v.x.eval(ncoeff);
  v.y.eval(ncoeff);
  gettimeofday(&tv_end, NULL);

  double elapsed_time =
    (tv_end.tv_sec + tv_end.tv_usec / 1000000.0) -
    (tv_start.tv_sec + tv_start.tv_usec / 1000000.0);

  tad_values values;
  values.t = t;
  values.x = v.x[0];
  values.y = v.y[0];
  values.dxdt.resize(ncoeff+1);
  values.dydt.resize(ncoeff+1);
  values.dxdt[0] = v.x[0];
  values.dydt[0] = v.y[0];

  int factj = 1;
  for (int i = 1; i < ncoeff+1; i++) {
    factj *= i;
    values.dxdt[i] = v.x[i] * factj;
    values.dydt[i] = v.y[i] * factj;
  }

  res.exec_time = elapsed_time ;
  res.dt = dt;
  res.nsteps = nsteps;
  res.values = values;
}

int main(int argc, char** argv) {
  if(cmdOptionExists(argv, argv+argc, "-help") ||
     cmdOptionExists(argv, argv+argc, "--help"))
  {
      cout << "usage: ./tad_cpp [[-]-help] [-n N] [-dt DT]" << endl;
      cout << endl;
      cout << "  -n number of steps to compute (default: " <<
        default_nsteps << ")" << endl;
      cout << "  -dt size of one step (default: " << default_dt << ")" << endl;
      cout << "  -ncoeff number of taylor coefficients to compute (default: " <<
        default_ncoeff << ")" << endl;
      cout << "  -help Display this list of options" << endl;
      cout << "  --help Display this list of options" << endl;
      return 0;
  };

  int nsteps, ncoeff; double dt;

  char* nsteps_str = getCmdOption(argv, argv+argc, "-n");
  if (nsteps_str) nsteps = atoi(nsteps_str);
  else nsteps = default_nsteps;

  char* dt_str = getCmdOption(argv, argv+argc, "-dt");
  if (dt_str) dt = stod(dt_str);
  else dt = default_dt;

  char* ncoeff_str = getCmdOption(argv, argv+argc, "-ncoeff");
  if (ncoeff_str) ncoeff = atoi(ncoeff_str);
  else ncoeff = default_ncoeff;

  result<tad_values> res;
  main_tad(nsteps, dt, ncoeff, res);
  print_tad_res(res);
  cout << endl;
  return 0;
}
