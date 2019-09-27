# Lowlevel bindigs to the [GR](http://gr-framework.org) plotting framework

Based on version [0.37.0](https://github.com/sciapp/gr/tree/v0.37.0/lib/gks), the one installed by hombrew on osx (`brew install libgr`).
Very incomplete: I don't know how to thead with the meta thingy in ctypes, and I have yet to bind the GKS library (which includes all the constants for the plot styles).

The documentation is published here: [online documentation](http://www.mseri.me/ocaml-gr/gr/index.html).

Currently `libGr` is only looked through the default library paths of your system. you can customise this by specifying the complete absolute path of `libGR.so` with the environment variable `LIBGRPATH`
