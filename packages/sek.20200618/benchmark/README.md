# Benchmarking Sek

## Installation

By default, the benchmarks are compiled using OCaml `4.09.1+flambda`.
To install this version of OCaml, please use the following command:

```
  make setup
```

The tools `prun` and `pplot` are required. They can be installed as
follows:

```
  git clone git@github.com:deepsea-inria/pbench.git
  cd pbench
  make install
```

## Compilation and Cleanup

To compile the benchmarks, type

```
  make
```

To clean up, use one of the following commands:

```
  make clean
  make realclean
```

The latter command removes all of the `.out` and `.pdf` files that you might
have produced.

## Running

Each benchmark has a name; examples include `stack`, `iteration`, `reach`.
A list of all benchmarks is printed by `./make.sh list`.

To run a benchmark, use the `run` command:

```
  ./make.sh run stack
```

If you wish to select a different OCaml switch, use the following syntax:

```
  switch=4.11.0 ./make.sh run stack
```

If you wish to select a different `git` branch or commit,
use the following syntax:

```
  commit=58f2ddd  ./make.sh run stack
  commit=HEAD^    ./make.sh run stack
  commit=mybranch ./make.sh run stack
```

This causes a `git` worktree to be created in the directory `/tmp`. This
worktree is never automatically removed; you have to do so yourself once you
are done. (You can also use `./make.sh gc`, at your own risk.)

## Plotting

To plot the results of the latest run, type:

```
  ./make.sh plot
```

You can also be more precise and indicate exactly
which previous run you would like to plot, e.g.

```
  commit=58f2ddd ./make.sh plot stack
```

## Viewing

To view the most-recently-generated `.pdf` file, just type:

```
  make view
```

## Comparing several runs

To compare several runs, proceed as in this example:

```
  ./make.sh compare start
  switch=4.09.1         ./make.sh run stack
  switch=4.09.1+flambda ./make.sh run stack
  ./make.sh compare stop
  ./make.sh compare plot
  make view
```

In the above example, only two runs are compared, but you can compare
more than two, if you wish.

In this example, two different OCaml switches are compared. Of course,
you can also easily compare two commits.
