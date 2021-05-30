open TestU01

let () =
  Swrite.set_basic false;

  let gen = Ulcg.create_lcg 2147483647 397204094 0 12345 in
  let res = Sres.create_poisson () in

  Smarsa.birthday_spacings gen (Some res) 1 1000 0 10000 2 1;
  (* .... Examine or postprocess res *)

  Smarsa.birthday_spacings gen (Some res) 1 10000 0 1000000 2 1
  (* .... Examine or postprocess res *)
