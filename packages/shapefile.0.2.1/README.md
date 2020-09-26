# OCaml shapefile library

A small library to read shapefiles. The implementation follows the
technical description provided by ESRI
(https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf).

## Install

You will need the [Bitstring](https://github.com/xguerin/bitstring)
library (version >= 3) to build, which you can install via opam:
```
opam install bitstring
```

Then the usual:
```
make
make install
```
(a public opam package is planned when I have time).

## License

This library is licensed under the Apache License, Version 2.0.
