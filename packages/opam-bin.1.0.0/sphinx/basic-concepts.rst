
Basic Concepts
==============

:code:`opam-bin` is a simple framework to use :code:`opam` with binary packages.
The framework is composed of:

* A set of repositories containing binary packages. These repositories
  will only work for people using the same distribution on the same
  architecture. You will need to select carefully the right one if
  you want to use them. These repositories are provided by external
  contributors. `Check this list <repositories.html>`__

* A :code:`git` repository containing patches to make :code:`opam`
  packages relocatable. Relocatable packages are needed because binary
  packages will be installed in different directories by different
  users. The repository is available here:
  `https://github.com/ocamlpro/relocation-patches
  <https://github.com/ocamlpro/relocation-patches>`__

* A tool called :code:`opam-bin` to create and use binary packages,
  available here: `https://github.com/ocamlpro/opam-bin
  <https://github.com/ocamlpro/opam-bin>`__

If you only want to use a repository of binary packages and not create
them, you will only need to access one of the binary repositories in
the first item, without the need for :code:`opam-bin`.
If you want to develop with a cache of binary packages, or to create
repositories of binary packages, then you need to install :code:`opam-bin`.

Binary packages
---------------

Binary packages created by :code:`opam-bin` follow the following convention:

* The binary package created from package :code:`$NAME.$VERSION` is called
  :code:`$NAME.$VERSION+bin+$HASH`, where $HASH is a unique hash. This hash
  is used because dependencies between binary packages are strict and
  cannot be changed.
* An alias package called :code:`$NAME+bin.$VERSION` is also generated, pointing
  to :code:`$NAME.$VERSION+bin+$HASH`. You can use it to install
  the corresponding binary package. For example::

    $ opam install ocamlfind+bin

When :code:`opam-bin` is installed and you ask to install a package
:code:`NAME.VERSION`, OPAM may decide to install the source package instead
of the binary package. OPAM will always select the source package if
you have a :code:`"NAME" { = VERSION }` dependencies asking for the package.

HOWEVER, :code:`opam-bin` will detect if there is a corresponding binary
package, and if it is the case, it will install the binary package
instead of compiling the package (:code:`opam` will still show you the build
steps, but these build steps will actually not be executed).

Relocatable packages
--------------------

Binary packages have to be relocatable to be installed in many
different locations. Most OCaml packages are relocatable, but some of
them are not. For example, :code:`ocaml-base-compiler`,
:code:`ocaml-variants`, :code:`ocamlfind`, :code:`ocamlbuild`, etc.
If you want to create binary packages, you should only use relocatable
packages.

We provide a specific :code:`git` repository containing patches to
make packages relocatable. This repository is available in the
project:

`https://github.com/OCamlPro/relocation-patches <https://github.com/OCamlPro/relocation-patches/>`__

It can be used automatically by :code:`opam-bin` with any :code:`opam`
repository.

If you want to use or contribute to this repository, you may want to
use a local version. You will then have to set the :code:`patches_url`
option, for example using the :code:`opam-bin config` command::

  opam bin config --patches-url file:///home/user/GIT/relocation-patches

Currently, it contains patches for the following packages:

* apron
* menhir
* mlgmpidl
* ocaml-base-compiler
* ocaml-variants
* ocamlbuild
* ocaml-config
* ocamlfind
* ocamlfind
* ocaml-variants

:code:`opam-bin` will try to find the best patch for a given source
package. For that, it will use the patch with the highest version less
or equal to the current package version. If :code:`opam-bin` cannot
find such a patch, it will disable itself automatically and let
:code:`opam` build the package without using/creating binary packages.

File Structure
--------------

:code:`opam-bin` creates the following file structure in the
:code:`$HOME/.opam/` directory (or :code:`OPAMROOT`):

* :code:`~/.opam/`

  * :code:`plugins/opam-bin/`

    * :code:`opam-bin.exe` This file is the executable of
      :code:`opam-bin` used in :code:`opam` wrappers.
    * :code:`opam-bin.log` This file is an internal log of
      :code:`opam-bin` used for debugging its behavior.
    * :code:`cache/` This directory contains a cache of the archives
      of the binary packages created locally. It is necessary as the
      URLs in the :code:`opam` files are not correct before the
      archives have been uploaded to their final location.
    * :code:`store/` This directory contains the files that have to be
      published to share binary packages between users/computers.

      * :code:`archives/` This directory contains all the archives of
        the binary packages that have been created locally.
      * :code:`repo/` This directory contains an :code:`opam`
        repository that exposes the binary packages locally
        created. It is used locally by :code:`opam`, and can be shared
        with other users/computers.

  * :code:`SWITCH/`

    * :code:`etc/opam-bin/packages/` This directory contains, for every
      binary package that has been installed or built in the switch, its
      binary version.
    * :code:`.opam-switch/opam-bin` This directory contains information
      on the packages viewed by :code:`opam-bin`. In particular, it contains
      the :code:`opam` file used to generate the binary package, the
      package patch if one was applied.

Patches Repository Structure
----------------------------
The :code:`git` repository of patches follows the following structure:

* :code:`patches/` directory:

  * :code:`NAME/` where :code:`NAME` is the name of a package

    * :code:`VERSION.patch` where :code:`VERSION` is the minimal version
      to which this patch can be applied.
    * :code:`NAME.alias` where :code:`NAME` is another package name,
      whose patches should be used for this package.
    * :code:`VERSION=.patch` where :code:`VERSION` is a version that should
      exactly match the version of the package on which it should be applied
