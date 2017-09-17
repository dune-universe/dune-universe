module OCSP = OCamlStandard.Pervasives

include StandardOutChannel.Make(struct
  let channel = OCSP.stdout
end)
