let gen_bytes ~(len:int) : bytes =
  let gen_bytes_helper () : bytes =
    Cstruct.to_string (
      Nocrypto.Rng.generate ~g:!Nocrypto.Rng.generator len
    ) in
  try
    gen_bytes_helper ()
  with
  | Nocrypto.Rng.Unseeded_generator ->
    begin
      Nocrypto_entropy_unix.initialize ();
      gen_bytes_helper ()
    end
;;
