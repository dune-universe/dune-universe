open Included_const

module FX =  F(struct end)

module B = struct
  include FX
  let x = T (* ? T *) (* CR jfuruse: BUG here *)
end
