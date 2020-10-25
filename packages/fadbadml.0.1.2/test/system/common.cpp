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
#include <vector>
#include <algorithm>
using namespace std;

int default_nsteps = 10;
double default_dt = 0.001;
int default_ncoeff = 5;

template<typename T>
struct result {
  double exec_time;
  double dt;
  int nsteps;
  T values;
};

void print_double(const string name, double f) {
  cout << fixed << "\"" << name << "\": " << f;
}
void print_int(const string name, int i) {
  cout << "\"" << name << "\": " << i;
}

void print_double_vector(const string name, vector<double> a) {
  if (a.size() == 0)
    cout << "\"" << name << "\":  []";
  else {
    cout << "\"" << name << "\": [" << a[0];
    for(int i = 1; i < a.size(); i++) {
      cout << fixed << ", " << a[i];
    }
    cout << "]";
  }
}

template<typename T>
void print_res(void(*print_values)(const string,T), result<T> res) {
  cout << "{" << endl;
  print_double("exec_time", res.exec_time);
  cout << "," << endl;
  print_double("dt", res.dt);
  cout << "," << endl;
  print_int("nsteps", res.nsteps);
  cout << "," << endl;
  print_values("values", res.values);
  cout << endl << "}";
}

// next two functions taken from
// https://stackoverflow.com/questions/865668/how-to-parse-command-line-arguments-in-c
char* getCmdOption(char ** begin, char ** end, const std::string & option)
{
    char ** itr = std::find(begin, end, option);
    if (itr != end && ++itr != end)
    {
        return *itr;
    }
    return 0;
}

bool cmdOptionExists(char** begin, char** end, const std::string& option)
{
    return std::find(begin, end, option) != end;
}
