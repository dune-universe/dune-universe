open Cfg_intf

module Spec :
  SPEC
    with type t = string
    with type nt = string
    with type prod = unit

module Bnf : CFG with module Spec = Spec
