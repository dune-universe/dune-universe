
Introduction
============

:code:`opam-bin` is a framework to build and use binary packages with
:code:`opam`.

With :code:`opam-bin`, it is possible:

* To create a binary package for every source package built by :code:`opam`;
* To re-use previously built binary packages instead of rebuilding the
  corresponding source packages;
* To share these binary packages with other users, by exporting these
  packages as :code:`opam` repositories;

Packages shared with :code:`opam-bin` have to be relocatable, i.e. can
be installed in any directory. A specific :code:`git` repository of
patches is provided, these patches are applied automatically by
:code:`opam-bin` on source packages at build time to make these
packages relocatable.

The following resources are associated with :code:`opam-bin`:

* :code:`git` repository with patches to make packages relocatable:
  `https://github.com/OCamlPro/relocation-patches <https://github.com/OCamlPro/relocation-patches>`__

* Software in the Github Project:
  `https://github.com/OCamlPro/opam-bin/ <https://github.com/OCamlPro/opam-bin/>`__
