# Benchmarking Sek

## Installation

By default, the benchmarks are compiled using OCaml `4.09.1+flambda`.
To install this version of OCaml, please use the following command:

```
  make setup
```

If desired, another version of OCaml can be selected on the command
line by overriding the variable `SWITCH`, like this:

```
  make SWITCH=4.09.1 setup
```

## Running

Each benchmark has its own `Makefile` entry. To run a benchmark,
just use the appropriate entry, e.g.,

```
  make study_no_functor
```

If desired, another version of OCaml can be selected on the command
line by overriding the variable `SWITCH`, like this:

```
  make SWITCH=4.09.1 study_no_functor
```

## Viewing

To view the most-recently-generated `.pdf` file, just type `make view`.
