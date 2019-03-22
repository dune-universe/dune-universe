(* This tests show that the last orders keep being executed even if we
   disconnect from the brick.  It also shows how to register a handler
   to deal with interactive interrupts. *)
open Printf
module Motor = Mindstorm.NXT.Motor

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)


let speed s = { Motor.speed = s;  motor_on = s <> 0;
                brake = false; run_state = `Running;
                regulation = `Idle; turn_ratio = 0;
                tach_limit = 0 (* forever *) }

let () =
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle(fun _ ->
                         Motor.set conn Motor.a (speed 0);
                         printf "Handler executed!  Motor off.\n";
                         exit 1;
                      ));
  Motor.set conn Motor.a (speed 10);
  printf "The motor connected to the port A should be running\n";
  printf "The program is going to terminate in 5 sec; try also Ctrl-C\n%!";
  Unix.sleep 5;
  Mindstorm.NXT.close conn;
  printf "We are disconnected.  Motor should still be running...\n";
  printf "Rexecute and press Ctrl-C\n"
