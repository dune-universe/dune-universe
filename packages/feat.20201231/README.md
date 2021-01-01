## Feat

Feat is a library that offers support for counting, enumerating, and sampling
objects of a certain kind, such as (say) the inhabitants of an algebraic data
type.

Feat was inspired by the paper
[Feat: Functional Enumeration of Algebraic Types](https://kar.kent.ac.uk/47486/)
by Jonas Duregård, Patrik Jansson and Meng Wang (2012).
It can perhaps be compared with the Haskell library
[testing-feat](https://hackage.haskell.org/package/testing-feat),
although a detailed comparison has not been carried out.

The library is laid out as follows:

* The module [Feat.IFSeq](src/IFSeq.mli) implements
  the signature [Feat.IFSEQ](src/IFSeqSig.ml)
  of (implicit, finite) **sequences**.

  These sequences are implicit, which means that they are not explicitly
  epresented in memory as an actual sequence of elements; instead, they are
  *described* by a data structure which contains enough information to
  produce an arbitrary element upon request. This design decision imposes
  some constraints on the operations that can be efficiently supported; for
  instance, `filter` is not supported.

* The module [Feat.Enum](src/Enum.mli) implements
  **enumerations**.
  An enumeration of type `'a enum` is a function of a size `s`
  to a sequence of elements of size `s`.

* The module [Feat.Bigint](src/bigint.mli)
  offers a function `random` of type `Z.t -> Z.t`,
  thus repairing an unfortunate omission in the library `zarith`.

* The external library
  [Fix](https://gitlab.inria.fr/fpottier/fix/)
  comes in handy when building enumerations
  of recursive algebraic data types:
  see the [demo](demo/Test.ml).
