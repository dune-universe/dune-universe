open Printf
module Sensor = Mindstorm.NXT.Sensor
module Motor = Mindstorm.NXT.Motor

let switch = `S1
let m = Motor.a

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)


let speed s =
  { Motor.speed = s;  motor_on = s <> 0;  brake = s <> 0;
    regulation = `Idle;
    turn_ratio = 0; run_state = `Running; tach_limit = 0  }

let () =
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  printf "Connected!  Push the touch sensor connected to \"1\".\n%!";
  Sensor.set conn switch `Switch `Bool;
  let rec run_until_pushed () =
    let sw = Sensor.get conn switch in
    if sw.Sensor.scaled <> 0 then
      Motor.set conn m (speed 0) (* Stop motor *)
    else run_until_pushed()  in
  Motor.set conn m (speed 1);
  run_until_pushed ();
  Mindstorm.NXT.close conn
