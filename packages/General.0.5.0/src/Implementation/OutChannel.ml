module OCSP = OCamlStandard.Pervasives
module OCSPr = OCamlStandard.Printf

type t = OCSP.out_channel

let flush = OCSP.flush

let print ?flush:(do_flush=false) channel format =
  OCSPr.kfprintf
    (fun channel -> if do_flush then flush channel)
    channel
    format

let output = OCSP.output_bytes
