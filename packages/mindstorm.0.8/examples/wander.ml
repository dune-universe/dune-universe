(* Example based on
   http://www.mactech.com/articles/mactech/Vol.23/23.04/LegoNXTontheMac/index.html
*)
open Printf
module Motor = Mindstorm.NXT.Motor
module U = Mindstorm.NXT.Sensor.Ultrasonic

let usleep s = ignore(Unix.select [] [] [] s)

let run conn =
  let speed b c =
    Motor.set conn Motor.b (Motor.speed b);
    Motor.set conn Motor.c (Motor.speed c) in

  let ultra = U.make conn `S4 in
  U.set ultra `Meas_cont;
  let i = ref 0 in
  while true do
    incr i;
    let dist = U.get ultra `Byte0 in
    if dist > 63 then (
      printf "%4i: Forward;     dist = %3i\r%!" !i dist;
      speed 75 75
    )
    else (
      printf "%4i: Turn;        dist = %3i\r%!" !i dist;
      speed 25 (-25)
    );
    usleep 0.2;
  done


let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);  exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  let stop _ =
    Motor.set conn Motor.b (Motor.speed 0);
    Motor.set conn Motor.c (Motor.speed 0);
    Mindstorm.NXT.close conn;
    printf "\n";
    exit 0 in
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
  printf "Press Ctrl-c to quit.\n%!";
  run conn
