type 'a t = 'a lazy_t

module OCSL = OCamlStandard.Lazy

let value = OCSL.force

let is_value = OCSL.is_val

let map x ~f =
  lazy (f (value x))
