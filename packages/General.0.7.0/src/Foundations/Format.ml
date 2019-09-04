module OCSPr = OCamlStandard.Printf
module OCSSc = OCamlStandard.Scanf

type ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, 'b, 'c, 'd, 'e, 'f) OCSP.format6

let with_result format ~f =
  OCSPr.ksprintf f format

let apply format =
  OCSPr.sprintf format

let of_string = OCSP.format_of_string
let to_string = OCSP.string_of_format
let concat = OCSP.(^^)

let with_scan_result format ~f s =
  OCSSc.sscanf s format f
