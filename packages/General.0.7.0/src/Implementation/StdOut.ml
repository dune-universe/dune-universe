module OCSP = OCamlStandard.Pervasives

include StandardOutChannel.Make(struct
  let channel = OCSP.stdout
  let flush = false
end)
