open Printf

type connection = Bluetooth of string | USB
let dev = ref None

type t = {
    args : (Arg.key * Arg.spec * Arg.doc) list;
    f : 'a. 'a Mindstorm.EV3.conn -> unit;
  }

let default_args = [
  "--bt", Arg.String(fun n -> dev := Some(Bluetooth n)),
  "addr Connects to this bluetooth address";
  "--usb", Arg.Unit(fun () -> dev := Some USB), " Connects to a USB EV3 brick";
]

let and_do d =
  let args = Arg.align(default_args @ d.args) in
  let usage_msg = sprintf "%s (--by addr|--usb)" Sys.argv.(0) in
  Arg.parse args (fun _ -> raise(Arg.Bad "no anonymous argument")) usage_msg;
  match !dev with
  | Some(Bluetooth addr) ->
     let conn = Mindstorm.EV3.connect_bluetooth addr in
     d.f conn;
     Mindstorm.EV3.close conn
  | Some USB ->
     printf "Mindstorm.EV3.USB.connect not yet implemented.\n";
     exit 2
     (* (match Mindstorm.EV3.USB.bricks() with *)
     (*  | dev :: _ -> *)
     (*     let conn = Mindstorm.EV3.USB.connect dev in *)
     (*     d.f conn; *)
     (*     Mindstorm.EV3.close conn *)
     (*  | [] -> print_endline "No EV3 brick connected to a USB port.") *)
  | None ->
     Arg.usage args usage_msg;
     exit 1
