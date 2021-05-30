open TestU01

let () =
  let gen = Ulcg.create_lcg 2147483647 16807 0 12345 in
  Bbattery.small_crush gen
