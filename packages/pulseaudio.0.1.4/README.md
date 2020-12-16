ocaml-pulseaudio


This package contains an OCaml interface for 
PulseAudio client library, 
otherwise known as pulseaudio.

Please read the COPYING file before using this software.

Prerequisites:
==============

- ocaml
- pulseaudio
- dune >= 2.0

Compilation:
============

```
$ dune build
```

This should build both the native and the byte-code version of the
extension library.

Installation:
=============

Via `opam`:

```
$ opam install samplerate
```

Via `dune` (for developers):
```
$ dune install
```

This should install the library file (using ocamlfind) in the
appropriate place.

Author:
=======

This author of this software may be contacted by electronic mail
at the following address: savonet-users@lists.sourceforge.net.
