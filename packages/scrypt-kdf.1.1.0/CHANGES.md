# 1.1.0 (2020-03-31)

* Port to mirage-crypto (thanks to @hannesm)

# 1.0.0 (2019-04-12)

* Move to dune
* Upgrade to opam 2.0

# 0.4.0 (2017-03-09)

* Removed Makefile, unneeded with topkg
* Made pkg.ml executable
* Added salsa20-core as a dependency and remove related code

# 0.3.0 (2017-02-21)

* Replaced underscores by dashes in library names
* Exported Salsa20_core module

# 0.2.0 (2016-10-31)

* Added topkg dependency
* Optimized inner loop in salsa_core to improve performance
* Replaced custom clone function by Nocrypto's implementation

# 0.1.0 (2016-03-18)

* Initial release
