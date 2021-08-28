module A = Asttypes
module P = Ppxlib

let arg_label (label : A.arg_label) : P.arg_label =
  match label with
  | A.Nolabel -> P.Nolabel
  | A.Labelled label -> P.Labelled label
  | A.Optional label -> P.Optional label


let mutable_flag (flag : A.mutable_flag) : P.mutable_flag =
  match flag with
  | A.Immutable -> P.Immutable
  | A.Mutable -> P.Mutable
