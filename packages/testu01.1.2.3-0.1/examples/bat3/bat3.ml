open TestU01

let () =
  let n = float_of_int (1024*1024) in

  (* Test the first n bits of binary file vax.bin *)
  Bbattery.alphabit_file "vax.bin" n;

  (* Test the Java random number generator *)
  let gen = Usoft.create_java48 1234567 1 in
  Bbattery.alphabit gen n 0 32
