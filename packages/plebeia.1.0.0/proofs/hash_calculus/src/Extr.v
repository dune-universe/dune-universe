Require Import ExtrOcamlBasic.
Require NodeHash.

Extraction Language OCaml.

Extraction "coq_hash_node.ml" NodeHash.compute.
