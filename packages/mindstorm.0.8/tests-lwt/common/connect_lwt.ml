open Lwt
open Lwt_io
module NXT = Mindstorm_lwt.NXT

type connection = Bluetooth of string | USB
let dev = ref None

type t = {
  args : (Arg.key * Arg.spec * Arg.doc) list;
  f : 'a. 'a NXT.conn -> unit Lwt.t;
}

let default_args = [
  "--bt", Arg.String(fun n -> dev := Some(Bluetooth n)),
  "addr Connects to this bluetooth address";
  "--usb", Arg.Unit(fun () -> dev := Some USB), " Connects to a USB NXT brick";
]

let and_do d =
  let args = Arg.align(default_args @ d.args) in
  let usage_msg = Printf.sprintf "%s (--bt addr|--usb)" Sys.argv.(0) in
  Arg.parse args (fun _ -> raise(Arg.Bad "no anonymous argument")) usage_msg;
  match !dev with
  | Some(Bluetooth addr) ->
     Lwt_main.run (NXT.connect_bluetooth addr >>= fun conn ->
                   d.f conn >>= fun () ->
                   NXT.close conn)
  | Some USB ->
     Lwt_main.run (NXT.USB.bricks() >>= fun bricks ->
                   (match bricks with
                    | dev :: _ ->
                       NXT.USB.connect dev >>= fun conn ->
                       d.f conn >>= fun () ->
                       NXT.close conn
                    | [] -> printf "No NXT brick connected to a USB port."))
  | None -> Arg.usage args usage_msg; exit 1
