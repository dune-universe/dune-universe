ocaml-evtchn
============

Xen event channel interface for Mirage.

Event channels are the Xen equivalent of interrupts, used to signal
when data (or space) is available for processing. There are 2 distinct
implementations:

  1. a Xen shared-memory + hypercall protocol for kernelspace
  2. a binding to a set of libxc functions which access /dev/xen/evtchn
     for userspace

Implementation notes
--------------------

The mirage-platform/xen tree contains a custom set of C stubs which
implements the simple parts of the protocol (eg mask, unmask, notify). These stubs
are referenced from the Eventchn module. The choice of stubs (userspace
or kernelspace) is performed at binary link-time by the 'mirage' command-line
tool (or manually), avoiding the need for another functor for these cases.

The more 'Activations' signature which has blocking functions is implemented
in OCaml in both the mirage-platform/xen tree (Activations) and here
(Unix_activations). Libraries must be functorised and the functor must be
instantiated in the final program.
