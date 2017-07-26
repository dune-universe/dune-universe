shared-memory-ring
==================

This repository contains a set of libraries for creating shared memory
producer/consumer rings. The rings follow the Xen ABI and may be used
to create or implement Xen virtual devices.

Example use:

One program wishes to create data records and push them efficiently
to a second process on the same physical machine for
sampling/analysis/archiving.

Example use:

A Xen virtual machine wishes to send and receive network packets to
and from a backend driver domain.
