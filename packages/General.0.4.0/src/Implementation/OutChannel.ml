module OCSP = OCamlStandard.Pervasives
module OCSPr = OCamlStandard.Printf

type t = OCSP.out_channel

let print channel format =
  OCSPr.fprintf channel format

let output = OCSP.output_bytes

let flush = OCSP.flush
