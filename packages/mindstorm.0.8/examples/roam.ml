open Printf
module Sensor = Mindstorm.NXT.Sensor
module Motor = Mindstorm.NXT.Motor

(* `S1 : touch sensor
   `S4 : ultrasonic sensor
   Motors connected to B (left motor) and C (right motor). *)

let usleep s = ignore(Unix.select [] [] [] s)

let run conn =
  Sensor.set conn `S1 `Switch `Bool;
  let ultra = Sensor.Ultrasonic.make conn `S4 in

  let set_speed b c  =
    Motor.set conn Motor.b (Motor.speed b);
    Motor.set conn Motor.c (Motor.speed c) in
  (* Stop if Ctrl-c is pressed. *)
  let stop _ = set_speed 0 0; Mindstorm.NXT.close conn; exit 0 in
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop);

  let get_ultra () =
    Sensor.Ultrasonic.set ultra `Meas;
    Sensor.Ultrasonic.get ultra `Byte0 in
  let cnt = ref 0 in
  while true do
    incr cnt;
    let switch = Sensor.get conn `S1 in
    let dist = get_ultra() in
    if switch.Sensor.scaled = 1 then begin
      (* Switch pressed, move back fearfully *)
      set_speed (-70) (-70);
      usleep 0.6;
    end;
    if dist > 50 then set_speed dist dist
    else begin
      (* obstable *)
      set_speed 30 0;
      usleep 0.6;
      let dist' = get_ultra() in
      printf "dist' = %i  disy = %i\n%!" dist' dist;
      if dist' < dist -3 then (set_speed 0 30; usleep 1.);

      set_speed 30 30;

      usleep 0.2;
    end
  done

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  printf "Press the button on the robot to stop.\n%!";
  run conn
