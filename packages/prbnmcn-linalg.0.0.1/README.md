prbnmcn-linalg
--------------

This package provides a functional overlay over vectors and matrices.
This implementation is heavily influced by the notion of "push vectors"
exposed in the book

"Reconciling Abstraction with High Performance: A MetaOCaml approach"

This approach in allows in particular to define algorithms independent of
the underlying data representation (dense, sparse, etc) _and_ independent
of the staging level (ie we can directly run _or_ generate code).

This library is still in the experimental stage: APIs are not stabilized yet.

[Documentation][doc-link]

[doc-link]: https://igarnier.github.io/prbnmcn-linalg/
