
Installation
============

Installation as a source package
--------------------------------

You will need :code:`opam` and a switch containing a recent version of
OCaml.

Then, you can use the following instructions::

  opam remote add reloc git@github.com:OCamlPro/opam-bin-repository
  opam install opam-bin -y

Once installation is successful, you can install it in your :code:`opam`
configuration with::

  eval $(opam env)
  opam-bin install

Installation as a binary package
--------------------------------

Select an :code:`opam` binary repository for your os/arch containing
the :code:`opam-bin` package and install it.

For exemple::

  opam switch create opam-bin --packages opam-bin -y --repos binaries=https://www.origin-labs.com/opam-bin/debian10.4-amd64/repo

Once installation is successful, you can install it in your :code:`opam`
configuration with::

  eval $(opam env)
  opam-bin install

Installation from sources
-------------------------

To install from sources, you need :code:`opam` and a switch containing
a recent version of OCaml.

You can use the following instructions::

  git clone https://github.com/OCamlPro/opam-bin
  cd opam-bin
  opam install --deps-only .
  make

Once compilation is successful, you can install it in your :code:`opam`
configuration with::

  ./opam-bin install
