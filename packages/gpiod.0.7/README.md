# libGpiod Ocaml Bindings

A direct (1-1) wrapper to libgpiod from ocaml. This unlocks access to the
new GPIO interface on Linux, instead of the error-prone and sluggish sysfs
approach. Bindings are systematically generated from the libgpiod header
file on the system. Memory management and reference counting are not taken
care of by the bindings, so the corresponding reference decrement or free
methods will still need to be called.

The bindings in this package are primitive, but unlock access to GPIO
ports on all kernels with gpiod support. I welcome improvements or full replacement
with a pure Ocaml version (Implementation of this would require communication
of precisely packed structures to the kernel through ioctl calls).

#### Usage

An example of usage on a Raspberry Pi can be seen in [examples](/examples/).

#### Dependencies

Requires libgpiod to be installed \([link](https://git.kernel.org/pub/scm/libs/libgpiod/libgpiod.git/about/)\).

#### TODO

- Synthesize bindings for methods that take a timespec like the gpiod_wait methods.
- Generate useful bindings for the methods that take mutable integer arrays.
