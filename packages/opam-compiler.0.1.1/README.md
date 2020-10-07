opam-compiler
=============

A WIP opam plugin to manage compiler installations.

It can be used to create switches from various sources such as the main
repository, ocaml-multicore, or a local directories. It can use tag names,
branch names, or PR numbers to specify what to install.

Once installed, these are normal opam switches, and one can install packages in
them. To iterate on a compiler feature and try opam packages at the same time,
it supports to ways to reinstall the compiler: either a safe and slow technique
that will reinstall all packages, or a quick way that will just overwrite the
compiler in place.

Installing
----------

This is an opam plugin. Once installed, it will be available globally using
`opam compiler ARGS`. To install it, pin it to get a development version:

    opam pin add opam-compiler 'git+https://github.com/emillon/opam-compiler.git'

Creating a switch
-----------------

`opam compiler create` is a wrapper around `opam switch create` that will use a
custom. The documentation can be found [here](doc/create.txt), but as an
example, the following is recognized:

    # Use this pull request number
    opam compiler create '#1234'

    # Use this branch
    opam compiler create 'myself/ocaml:mybranch'

It will try determine a switch name and description from the source name, but it
is also possible to pass an explicit switch name:

    # Use an explicit switch name
    opam compiler create '#1234' --switch optimize-list-map

The resulting switch can be used like a normal switch: one can install packages,
update them, etc.

By default, the compiler will be built using a plain `./configure` command,
which will create a vanilla compiler. It is possible to override this:

    # Just build the bytecode compiler from a pull request
    opam compiler create '#1234' --configure-command "./configure --disable-native-compiler"
