open Printf

let new_brick_name = ref None

let print_info conn =
  printf "Connected!\n%!";
  begin match !new_brick_name with
  | None -> ()
  | Some name ->
      Mindstorm.NXT.set_brick_name conn name ~check_status:true;
      printf "Brick name set to %S.\n" name
  end;
  printf "Device info: \n%!";
  let i = Mindstorm.NXT.get_device_info conn in
  printf "- brick name = %S\n" i.Mindstorm.NXT.brick_name;
  printf "- bluetooth address = %S\n" i.Mindstorm.NXT.bluetooth_addr;
  printf "- signal strength = %i\n" i.Mindstorm.NXT.signal_strength;
  printf "- free user FLASH = %i bytes\n%!" i.Mindstorm.NXT.free_user_flash;
  let (p1, p0, f1, f0) = Mindstorm.NXT.firmware_version conn in
  printf "- protocol = %i.%i, firmware = %i.%02i\n" p1 p0 f1 f0;
  printf "Battery level: %!";
  let bat = Mindstorm.NXT.battery_level conn in
  printf "%i millivolts\n%!" bat

let () =
  Connect.and_do {
    Connect.args = [ "--name", Arg.String(fun n -> new_brick_name := Some n),
                     "n Set a new name for the brick";
                   ];
    f = print_info;
  }
