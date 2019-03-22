open Printf
module Sensor = Mindstorm.NXT.Sensor
module Motor = Mindstorm.NXT.Motor

let c_touch = Condition.create ()
let m = Mutex.create ()

let test_touch conn =
 Sensor.set conn `S1 `Switch `Bool;
 while true do
   let sensor = Sensor.get conn `S1 in
   printf "sensor = %i\n%!" sensor.Sensor.scaled;
   if sensor.Sensor.scaled <> 0 then begin
     Condition.signal c_touch
   end;
   Thread.delay 0.3
 done;;

let react_touch _conn =
 while true do
   Condition.wait c_touch m;
   printf "touche\n%!";
   Thread.delay 0.3;
 done;;

let () =
 let bt =
   if Array.length Sys.argv < 2 then (
     printf "%s <bluetooth addr>\n" Sys.argv.(0);
     exit 1;
   )
   else Sys.argv.(1) in
 let conn = Mindstorm.NXT.connect_bluetooth bt in
 printf "Connected\n%!";
 let _t1 = Thread.create react_touch conn in
 let _t2 = Thread.create test_touch conn in
 Thread.delay 10.;
 printf "FIN\n%!";;
