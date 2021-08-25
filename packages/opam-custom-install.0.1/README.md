# opam-custom-install - Experimental plugin for custom package installations in opam

This is an attempt to help with workflows where you work on a package source,
and currently have the choice to:

1. either properly run `opam install .` or other proper pin/install commands
2. or go brute-force with _e.g._ `make install` or `dune install` to install to
   the opam prefix

The first option keeps a clean and synchronised opam state, but it can be
annoying because it will sync your source and recompile everything from scratch
everytime, something you want to avoid on large projects like the compiler.
Besides, it's not great for error reporting, so you probably build the package
by hand already, and with git pinning you need to remember to commit beforehand.
There are workarounds like `--inplace-build` or `--assume-built`, but they are
not convenient and don't work very well in practice.

The second option just skips opam entirely, and desynchronises the actual switch
prefix state from what opam knows about it. It can be OK for simple quick tests,
but can lead to all sort of problems downstream, with stale package files left
lying around, confused reinstallation not corresponding to the actual state of
packages, etc. It also won't allow you to use opam to easily test packages
depending on the one you are editing.


## Installing `opam-custom-install`

The easiest way is to pin it directly, since it's unreleased yet:
```
opam pin https://gitlab.ocamlpro.com/louis/opam-custom-install.git
```

Since it's registered as a plugin, it should make the `opam custom-install`
command available whatever switch you are in.

If you prefer to compile it manually;
```
dune build @install
ln -sf _build/default/opam-custom-install.exe opam-custom-install
```

## Usage

```
opam custom-install [--no-recompilations] PACKAGE[.VERSION] -- COMMAND [ARG]...
```

Typically, when working on package `foo`:
```bash
[.../foo]$ ./configure --prefix=$(opam var prefix)
...
[.../foo]$ make all
...
[.../foo]$ opam custom-install foo -- make install
```

Opam will run the supplied command (here `make install`), but with a few extra
precautions:
- the configured opam sandbox is used, making sure you install in the correct
  opam prefix
- the previously installed version of `foo`, if any, is first removed from the
  prefix
- unless you specified `-n`/`--no-recompilations`, any installed package
  depending on `foo` that was installed is first removed, and reinstalled
  afterwards
- opam will track all files installed by the command, and register them as
  belonging to this package, ensuring correct uninstallation later on
- the usual locks are taken on the switch to prevent concurrent opam operations

## Current status

This is a proof of concept at this stage, but should be fairly usable. Feel free
to report any issues or discuss it on this project tracker, ocamllabs.slack.com,
or by email.

Usage with the `-n`/`--no-recompilations` option may be more stable than usage
without at this point.

## Next steps

If this seems useful, and once it's proven stable enough, this will be released
on opam-repository as an optional opam plugin.

If we decide this really provides workflow improvements, it'll be merged as a
built-in command in opam (it's written as a commandliner subcommand already, so
that should be just a copy-paste).

Going even beyond that, we might want to deprecate opam flags such as
`--assume-built`.

## Copyright and license (same as opam)

Copyright 2020 OCamlPro

All rights reserved. this is distributed under the terms of the GNU Lesser
General Public License version 2.1, with the special exception on linking
described in the file LICENSE.

opam-custom-install is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
more details.
