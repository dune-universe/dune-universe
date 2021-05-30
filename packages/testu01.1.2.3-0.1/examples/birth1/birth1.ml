open TestU01

let () =
  let gen = Ulcg.create_lcg 2147483647 397204094 0 12345 in
  Smarsa.birthday_spacings gen None 1 1000 0 10000 2 1;
  Smarsa.birthday_spacings gen None 1 10000 0 1000000 2 1
