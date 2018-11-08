module OCSP = OCamlStandard.Pervasives

include StandardOutChannel.Make(struct
  let channel = OCSP.stderr
  let flush = true
end)
