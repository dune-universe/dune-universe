## GMP - Packaged with Dune

More info on GMP: https://gmplib.org/

To use GMP in a _cross-compilation_-friendly way, simply add this library in 
your library dependencies and make sure it is locally fetched using the 
`opam-monorepo` plugin (https://github.com/ocamllabs/duniverse).

Example Dune configuration:
```
(library my_library)
 (foreign_stubs (language c) (include_dirs (lib gmp)))
 (libraries gmp)
 ...
)
```
