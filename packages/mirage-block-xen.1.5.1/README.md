mirage-xen-block
======================

This library allows a Mirage OCaml application to

  1. read and write blocks from any Xen "backend" (server)
  2. service block requests from any Xen "frontend" (client)

This library can be used in both kernelspace (on Xen)
or in userspace (using libraries that come with Xen).

This library depends on the
[shared-memory-ring](https://github.com/mirage/shared-memory-ring)
library which enables high-throughput, low-latency data
transfers over shared memory on both x86 and ARM architectures,
using the standard Xen RPC and event channel semantics.

Example: in Mirage unikernels on xen
------------------------------------

The [block_perf](https://github.com/mirage/mirage-skeleton/tree/master/block_perf)
example shows how a Mirage application can use this library
to access a virtual disk with very little overhead.

For performance results and discussion, see
[Unikernels: Library Operating Systems for the Cloud](http://anil.recoil.org/papers/2013-asplos-mirage.pdf)

Example: prototyping new storage for Virtual Machines
-----------------------------------------------------

The [xen-disk](https://github.com/mirage/xen-disk) application runs
in userspace and attaches a virtual disk to an existing xen VM.
The disk read and write requests are served in userspace through
one of a set of 'backends' which are selectable on the commandline.

