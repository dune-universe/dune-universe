let gen_string ~(len:int) : string =
  let gen_string_helper () : string =
    Cstruct.to_string (
      Nocrypto.Rng.generate ~g:!Nocrypto.Rng.generator len
    ) in
  try
    gen_string_helper ()
  with
  | Nocrypto.Rng.Unseeded_generator ->
    begin
      Nocrypto_entropy_unix.initialize ();
      gen_string_helper ()
    end
;;
