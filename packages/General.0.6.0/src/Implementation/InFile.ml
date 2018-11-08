module OCSP = OCamlStandard.Pervasives

type t = OCSP.in_channel

let seek x ~pos =
  OCSP.LargeFile.seek_in x pos

let pos = OCSP.LargeFile.pos_in

let size = OCSP.LargeFile.in_channel_length

let channel = identity

let with_file s ~f =
  let file = OCSP.open_in s in
  try
    let r = f file in
    OCSP.close_in file;
    r
  with
    | ex -> OCSP.close_in file; Exception.raise ex

let with_channel s ~f =
  with_file s ~f:(f % channel)
