open Printf

module Motor = Mindstorm.NXT.Motor

let bt = ref ""
let speed = ref(30)
let rot_deg = ref(3 * 360)

let () =
  let args = Arg.align [
    "-speed", Arg.Set_int speed, "s Set the speed (in -100 .. 100)";
    "-s", Arg.Set_int speed, "s Set the speed (in -100 .. 100)";
    "-rotate", Arg.Set_int rot_deg, "r Rotation in degrees";
    "-r", Arg.Set_int rot_deg, "r Rotation in degrees";
  ]
  and usage_msg = sprintf "%s <options> bluetooth_addr" Sys.argv.(0) in
  Arg.parse args (fun addr -> bt := addr) usage_msg;
  if !bt = "" then begin Arg.usage args usage_msg; exit 1 end


let () =
  let conn = Mindstorm.NXT.connect_bluetooth !bt in
  printf "Rotate motor connected to port A by at speed %i for %i degrees... %!"
    !speed !rot_deg;
  let st = { Motor.speed = !speed;
             motor_on = true;
             brake = true; (* without this, the motor does not rotate
                              at low speeds *)
             regulation = `Idle;
             turn_ratio = 0;
             run_state = `Running;
             tach_limit = !rot_deg } in
  Motor.set conn Motor.a st ~check_status:true;
  printf "done\n%!";
  Unix.sleep 5; (* we cannot exit immediately, otherwise no motor
                   rotation is performed. *)
  Motor.set conn Motor.a (Motor.speed 0);
  Mindstorm.NXT.close conn
