open Types

(** If you want to write an instance of meta_conv implementation, 
    you define the following primitive encoders and decoders at least.
*)
module Make(A : Min) : S with type target = A.target
