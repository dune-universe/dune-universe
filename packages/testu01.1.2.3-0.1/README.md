OCaml Bindings for TestU01
==========================

From *TestU01 User’s guide, compact version*:

> It is also possible to test one’s own or an external generator (that is, a
> generator that is not predefined in TestU01) very easily [...] as long as this
> generator is programmed in C.

Wrong. Behold: OCaml bindings for TestU01.

Quick Links
-----------

About *TestU01*:
- [Webpage](http://simul.iro.umontreal.ca/testu01/tu01.html)
- [Installation Instructions](http://simul.iro.umontreal.ca/testu01/install.html)
- [User's Guide](http://simul.iro.umontreal.ca/testu01/guideshorttestu01.pdf)
- [GitHub Repository](https://github.com/umontreal-simul/TestU01-2009/)

About *OCaml Bindings for TestU01*:
- [Documentation](https://lesboloss-es.github.io/ocaml-testu01/index.html)
- API Documentation:
  [TestU01](https://lesboloss-es.github.io/ocaml-testu01/TestU01/index.html),
  [Probdist](https://lesboloss-es.github.io/ocaml-testu01/Probdist/index.html)
- [GitHub Repository](https://github.com/LesBoloss-es/ocaml-testu01/)

Installation
------------

The recommended way to install these bindings is via OPAM:

    opam install testu01

The package comes with its embedded version of the C library, so there is no
external dependency required. This means that it should also be trivial to build
and install these bindings the traditional way, with a simple:

    make
    make install

API
---

The OCaml API remains as close to that of TestU01 as possible. Some changes are
still introduced to provide a more “OCaml-like” interface. The differences are
the following:

- The names have changed:

  - The library is all under two main modules, `TestU01` and `Probdist`. The
    documentation of these modules can be found
    [here](https://lesboloss-es.github.io/ocaml-testu01/TestU01/index.html)
    and
    [here](https://lesboloss-es.github.io/ocaml-testu01/Probdist/index.html)
    respectively.

  - The functions, variables and types are not prefixed by the part of the
    library they belong to, but are in a corresponding module.

  - The names `are_with_underscore` and `NotInCamelcase`.

- The global variables are read and modified using getters and setters.

- The creation of external generators is done with three functions similar to
  that of the C library. In the module `TestU01`:

  - `Unif01.create_extern_gen_bits` takes an OCaml “bits” function, that is a
    function that returns an integer between `0` and `2^30`. This is the case of
    `Random.bits`, for instance.

  - `Unif01.create_extern_gen_int32` takes an OCaml function that returns an
    `int32`. Note that such a function is expected to provided all 32 bits. In
    particular, `fun () -> Random.int32 Int32.max_int` is not a good candidate.

  - `Unif01.create_extern_gen_01` takes an OCaml function that returns a float
    between `0` and `1`.

- The destruction of external generators is let to OCaml's garbage collector and
  cannot be done by hand.

About the name changes, here are a few examples. The binding for the function:

```c
void bbattery_RepeatBigCrush(unif01_Gen* gen, int* rep);
```

will be:

```ocaml
val Testu01.Bbattery.repeat_big_crush : Unif01.gen -> int array -> unit
```

and the binding for the variable:

```c
extern double gofw_Suspectp;
```

will be:

```ocaml
val Probdist.Gofw.get_suspectp : unit -> float
val Probdist.Gofw.set_suspectp : float -> unit
```

Example Use
-----------

Let us say we want to asses the quality of the PRNG in OCaml's standard library.
We can simply write the following file:

```ocaml
open TestU01
open Probdist
let () =
  let gen = Unif01.create_extern_gen_bits "stdlib" Random.bits in
  Gofw.set_suspectp 0.01;
  Bbattery.crush gen
```

This creates a TestU01 generator of type `Unif01.gen` out of a function `unit ->
bits`, sets the threshold for suspect p-values to `0.01` (that is optional, the
default is `0.001`) and applies the test battery "crush" to the generator. It
can then be compiled to an executable that takes about 40 minutes to run and
writes a report on the standard output, similar to the following:

```
       Test                          p-value
 ----------------------------------------------
 11  BirthdaySpacings, t = 2          eps
 31  Gap, r = 0                       eps
 33  Gap, r = 0                       eps
 47  SampleMean                      0.9940
 51  WeightDistrib, r = 0             eps
 52  WeightDistrib, r = 8             eps
 53  WeightDistrib, r = 16            eps
 83  HammingCorr, L = 300            8.6e-3
 86  HammingIndep, L = 30            0.9945
 ----------------------------------------------
```

Some tests (with p-value `eps`) do not pass. For some tests, the p-value is
suspicious (that is, inferior to `suspectp` or bigger than `1 - suspectp` but
not `eps` nor `1 - eps`), but we cannot really conclude anything from it.
Luckily, TestU01 provides `Repeat*` variants for its test batteries. They allow
to provide, *via* an array, the number of times each test must be run. Let us
repeat the 47th test, “SampleMean”, the 83rd test, “HammingCorr with L = 300”,
and the 86th test, “HammingIndep with L = 30”. We will repeat them 10 times
each, and the others 0 times, to decide whether the p-value is indeed suspicious
or if it was an unlucky artefact. For that, we can write the following file:

```ocaml
open TestU01
open Probdist
let () =
  let gen = Unif01.create_extern_gen_bits "stdlib" Random.bits in
  Gofw.set_suspectp 0.01;
  let rep = Array.make (1 + Bbattery.ntests_crush) 0 in
  rep.(47) <- 10;
  rep.(83) <- 10;
  rep.(86) <- 10;
  Bbattery.repeat_crush gen rep
```

Running it takes about 5 minutes and writes the following report:

```
 All tests were passed
```

This means that all these tests were indeed unlucky artefacts. (The other tests,
with p-value `eps`, definitely do not.)

For more information, we encourage you to refer to TestU01's user's guide.

Examples From TestU01's Documentation
-------------------------------------

TestU01's User Guide includes several examples. Here are some with their OCaml
version. These examples as well as their C version and the expected result can
be found in the `examples` directory.

### Figure 4.1

In the User Guide:

```c
#include "ulcg.h"
#include "unif01.h"
#include "bbattery.h"

int main (void)
{
   unif01_Gen *gen;
   gen = ulcg_CreateLCG (2147483647, 16807, 0, 12345);
   bbattery_SmallCrush (gen);
   ulcg_DeleteGen (gen);
   return 0;
}
```

With these bindings:

```ocaml
open TestU01

let () =
  let gen = Ulcg.create_lcg 2147483647 16807 0 12345 in
  Bbattery.small_crush gen
```

### Figure 4.3

In the User Guide:

```c
#include "gdef.h"
#include "swrite.h"
#include "bbattery.h"

int main (void)
{
   swrite_Basic = FALSE;
   bbattery_RabbitFile ("vax.bin", 1048576);
   return 0;
}
```

With these bindings:

```ocaml
open TestU01

let () =
  Swrite.set_basic false;
  Bbattery.rabbit_file "vax.bin" 1048576.
```

You can refer to the `examples` directory which contains examples from TestU01
as well as their OCaml version.

License
-------

Copyright (C) 2020-2021 Martin Pépin, Nicolas “Niols” Jeannerod

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.

FAQ
---

### Missing Bindings

We add bindings on-demand and our use is mainly oriented towards test batteries.
So if some bindings do not appear, it is simply that we did not have any use for
them. Add them (PR are welcome), or ask that we do!
