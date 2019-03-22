open Lwt
open Lwt_io
module NXT = Mindstorm_lwt.NXT

let new_brick_name = ref None

let print_info conn =
  printf "Connected!\n%!" >>= fun () ->
  begin match !new_brick_name with
  | None -> return ()
  | Some name ->
     NXT.set_brick_name conn name ~check_status:true >>= fun () ->
     printf "Brick name set to %S.\n" name
  end >>= fun () ->
  printf "Device info: \n%!" >>= fun () ->
  NXT.get_device_info conn >>= fun i ->
  printf "- brick name = %S\n" i.NXT.brick_name >>= fun () ->
  printf "- bluetooth address = %S\n" i.NXT.bluetooth_addr >>= fun () ->
  printf "- signal strength = %i\n" i.NXT.signal_strength >>= fun () ->
  printf "- free user FLASH = %i bytes\n%!" i.NXT.free_user_flash >>= fun () ->
  NXT.firmware_version conn >>= fun (p1, p0, f1, f0) ->
  printf "- protocol = %i.%i, firmware = %i.%02i\n" p1 p0 f1 f0 >>= fun () ->
  printf "Battery level: %!" >>= fun () ->
  NXT.battery_level conn >>= fun bat ->
  printf "%i millivolts\n%!" bat

let () =
  Connect_lwt.and_do {
    Connect_lwt.args = [ "--name", Arg.String(fun n -> new_brick_name := Some n),
                         "n Set a new name for the brick";
                       ];
    f = print_info;
  }
