Coq Proofs that the Hash Algorithm Does Not Collide

## What is Proven

We prove Collision Resistance of the function that computes the hash of a node.
In order to prove this, we develop the proof that the function is also injective, assuming that the underlying hash function is injective.

### The Reference Implementation
https://gitlab.com/dailambda/plebeia/-/blob/347b10ccd5f160dc493fdcab4982044e68e33482/src/node_hash.ml#L43-86

### What are the axioms

TODO:

## How to Verify

```sh
./configure.sh
make
```

### My environment

- Coq version 8.9.1
