module OCSB = OCamlStandard.Bytes

type t = bytes

let of_string = OCSB.of_string
let to_string = OCSB.to_string

let get = OCSB.get
let set = OCSB.set

let size = OCSB.length

let make ~len =
  OCSB.create len
