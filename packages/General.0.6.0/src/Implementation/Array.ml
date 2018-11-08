type 'a t = 'a array

module OCSA = OCamlStandard.Array

let get = OCSA.get

let set = OCSA.set

let size = OCSA.length
