open Printf
module Sensor = Mindstorm.NXT.Sensor

(* Win32 command shell is poor *)
let repeat_till_ENTER_win32 msg f =
  printf "%s.\n%!" msg;
  let i = ref 0 in
  while !i < 200 do f !i; incr i done

(* Unix & Mac OSX have proper terminals *)
let repeat_till_ENTER_unix msg f =
  let params = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
    { params with Unix.c_icanon = false; c_echo = false;
        c_vmin = 0; c_vtime = 0; };
  (* FIXME: We should also catch signals *)
  let no_key_pressed() = Unix.read Unix.stdin (Bytes.create 1) 0 1 = 0 in
  printf "%s; when finished press ENTER.\n%!" msg;
  try
    let i = ref 0 in
    while no_key_pressed() do f !i; incr i done;
    (* restore params *)
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH params
  with e ->
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH params;
    raise e

let repeat_till_ENTER =
  if Sys.os_type = "Win32" then repeat_till_ENTER_win32
  else repeat_till_ENTER_unix


let color_sensor = ref true

let sensors conn =
  printf "It is assumed that sensors are connected as follows:\n";
  if !color_sensor then printf "- port 1: NXT 2.0 color sensor\n"
  else printf "- port 1: NXT 1.0 light sensor\n";
  printf "- port 2: touch sensor\n";
  printf "- port 3: sound sensor\n";
  printf "- port 4: ultrasonic sensor\n%!";

  let default_scaled data = sprintf "%-5i" data.Sensor.scaled in
  let test_sensor ?(scaled=default_scaled) name port =
    repeat_till_ENTER (sprintf "- %s sensor" name) begin fun i ->
      let data = Sensor.get conn port in
      printf "%4i:\t raw = %4i   normalized = %4i   scaled = %s\r%!"
        i data.Sensor.raw data.Sensor.normalized (scaled data);
    end;
    printf "\n" in

  if !color_sensor then (
    let color_of_data data = match Sensor.color_of_data data with
      | `Black  -> "black " | `Blue -> "blue  " | `Green -> "green "
      | `Yellow -> "yellow" | `Red  -> "red   " | `White -> "white " in
    Sensor.set conn `S1 `Color_full `Pct_full_scale;
    test_sensor "Color" `S1 ~scaled:color_of_data;
  )
  else (
    (* Sensor.set conn `S1 `Light_active `Pct_full_scale; *)
    Sensor.set conn `S1 `Color_full `Pct_full_scale;
    (*   Sensor.set conn `S1 `Switch `Light_inactive; *)
    test_sensor "Light" `S1;
  );
  Sensor.set conn `S1 `No_sensor `Raw; (* => turn off light *)

  Sensor.set conn `S2 `Switch `Bool;
  test_sensor "Touch (bool)" `S2;
  Sensor.set conn `S2 `Switch `Transition_cnt;
  test_sensor "Touch (transition)" `S2;
  Sensor.reset_scaled conn `S2;
  Sensor.set conn `S2 `Switch `Period_counter;
  test_sensor "Touch (period)" `S2;

  Sensor.set conn `S3 `Sound_db `Pct_full_scale;
  (*   Sensor.set conn `S3 `Sound_dba `Pct_full_scale; *)
  test_sensor "Sound" `S3;

  let us = Sensor.Ultrasonic.make conn `S4 in
  Sensor.Ultrasonic.set us `Meas_cont ~check_status:true;
  repeat_till_ENTER "- Ultrasonic sensor" begin fun i ->
    try
      let dist = Sensor.Ultrasonic.get us `Byte0 in
      printf "%4i:\t                    dist = %i\r%!" i dist
    with e ->
      printf "%4i:\t %s\r%!" i (Printexc.to_string e);
      Unix.sleep 1
  end;
  printf "\n"

let () =
  Connect.and_do {
    Connect.args = [ "--light", Arg.Clear color_sensor,
                     " says that the sensor on port 1 is the old light sensor"];
    f = sensors }
