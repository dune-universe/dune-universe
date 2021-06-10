open TestU01

let () =
  Swrite.set_basic false;
  Bbattery.rabbit_file "vax.bin" 1048576.
