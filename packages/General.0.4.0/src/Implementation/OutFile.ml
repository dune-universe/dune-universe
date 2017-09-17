module OCSP = OCamlStandard.Pervasives

type t = OCSP.out_channel

let seek x ~pos =
  OCSP.LargeFile.seek_out x pos

let pos = OCSP.LargeFile.pos_out

let size = OCSP.LargeFile.out_channel_length

let channel = identity

let with_file s ~f =
  let file = OCSP.open_out s in
  try
    let r = f file in
    OCSP.close_out file;
    r
  with
    | ex -> OCSP.close_out file; Exception.raise ex

let with_channel s ~f =
  with_file s ~f:(f % channel)
