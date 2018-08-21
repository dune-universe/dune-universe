module Spec = struct
  type t = string
  type nt = string
  type prod = unit

  type symbol = NT of nt | T of t

  let compare_t = compare
  let compare_nt = compare
  let compare_prod = compare
end

module Bnf = Cfg_impl.Make (Spec)
