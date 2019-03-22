open Printf

type connection = Bluetooth of string | USB
let dev = ref None

type t = {
  args : (Arg.key * Arg.spec * Arg.doc) list;
  f : 'a. 'a Mindstorm.NXT.conn -> unit;
}

let default_args = [
  "--bt", Arg.String(fun n -> dev := Some(Bluetooth n)),
  "addr Connects to this bluetooth address";
  "--usb", Arg.Unit(fun () -> dev := Some USB), " Connects to a USB NXT brick";
]

let and_do d =
  let args = Arg.align(default_args @ d.args) in
  let usage_msg = sprintf "%s (--bt addr|--usb)" Sys.argv.(0) in
  Arg.parse args (fun _ -> raise(Arg.Bad "no anonymous argument")) usage_msg;
  match !dev with
  | Some(Bluetooth addr) ->
      let conn = Mindstorm.NXT.connect_bluetooth addr in
      d.f conn;
      Mindstorm.NXT.close conn
  | Some USB ->
      (match Mindstorm.NXT.USB.bricks() with
       | dev :: _ ->
           let conn = Mindstorm.NXT.USB.connect dev in
           d.f conn;
           Mindstorm.NXT.close conn
       | [] -> print_endline "No NXT brick connected to a USB port.")
  | None -> Arg.usage args usage_msg; exit 1
