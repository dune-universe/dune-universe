
(* options' default values, can be changed by the CLI *)

type fingerprint = MACCS | ECFP4 | MOP2D

let curr_fingerprint = ref ECFP4 (* default *)

type score = Tanimoto | Tversky of float

let curr_score = ref Tanimoto (* default *)

(* index queries prior to oppo policy Tanimoto scoring?
   probably many queries (> 40?) are needed to observe some acceleration *)
let fast_oppo_score = ref false

let force_scaffolds_diversity = ref false

type scheme = Logarithmic | Linear

let potency_scaling = ref Logarithmic

(* Power Metric threshold *)
let pm_percent = ref 0.1
