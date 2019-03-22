open Printf
module Sensor = Mindstorm.NXT.Sensor
module Motor = Mindstorm.NXT.Motor

(* `S1 : touch sensor
   `S4 : ultrasonic sensor
   Motors connected to B and C (like the basic robot of the LEGO® set). *)

let test_motor id_motor conn speed =
  printf "speed %i\n%!" speed;
  Motor.set conn id_motor (Motor.speed speed);
  Unix.sleep 5

let test_rot id_motor conn speed rot =
  printf "speed %i rot %i\n%!" speed rot;
  Motor.set conn id_motor (Motor.speed speed ~tach_limit:rot);
  Unix.sleep 10

let run conn =
  printf "begin";
  List.iter (test_rot Motor.a conn (-10))
    [1; 30; 45; 70; 100; 200; 500];
  Motor.set conn Motor.a (Motor.speed 0);
  (*List.iter (test_motor Motor.b conn) [-50;-70;0];
  Motor.set conn Motor.b (Motor.speed 0);*)
  Mindstorm.NXT.close conn;;

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  printf "Test motor.\n%!";
  run conn
