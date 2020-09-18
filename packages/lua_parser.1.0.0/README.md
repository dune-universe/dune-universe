Luajit/Lua 5.2 Parser and Pretty Printer
========================================

This repository contains a
[luajit](https://luajit.org)/[lua](https://www.lua.org) 5.2 parser and pretty
printer implemented in [OCaml](https://ocaml.org).  It has been reasonably well
tested and any code parsed and pretty printed should execute correctly.  If not
please submit an issue.

Installation
------------

As long as you have a current version of [opam](https://opam.ocaml.org),
installation is as simple as:
```bash
$ opam install lua_parser
```

You can then use it to parse and print lua code:

```bash
$ l2l -lua file1.lua
```

License
-------

[MIT](https://opensource.org/licenses/MIT)
