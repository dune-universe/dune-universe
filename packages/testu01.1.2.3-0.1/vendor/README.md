This directory includes a full version of TestU01 as provided by the authors, as
well as a way to compile it using Dune, producing in the end all the `*.h` files
and a `testu01.vendor` “OCaml” library containing the `*.a` and `*.so`. This is
only meant to be used to compile the bindings in the rest of the repository.

The original TestU01 library is copyrighted to Pierre L'Ecuyer and Université de
Montréal and is licensed under the Apache License Version 2.0 (see
https://www.apache.org/licenses/LICENSE-2.0). The `COPYING` file from TestU01's
archive is copied in this directory for transparency.

The zip file `TestU01.zip` **is not the same** as the original one. Here is a
complete list of changes:

- Renamed main directory from `TestU01-1.2.3` to `TestU01`

- Replaced `TestU01-1.2.3/config.guess` (timestamped `2006-07-02`) by the one
  timestamped `2021-01-01` taken from
  http://cvs.savannah.gnu.org/viewvc/*checkout*/config/config/config.guess

- Replaced `TestU01-1.2.3/config.sub` (timestamped `2006-09-20`) by the one
  timestamped `2021-01-01` taken from
  http://cvs.savannah.gnu.org/viewvc/*checkout*/config/config/config.sub
