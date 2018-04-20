open Ipv6_multicast
module L = Ipv6_multicast_lwt

module Error_monad = struct
  include Lwt.Infix

  let (>>=?) a f =
    a >>= function
    | Ok v -> f v
    | Error e -> Lwt.return_error e

  let (>>|?) a f =
    a >>= function
    | Ok v -> Lwt.return (f v)
    | Error e -> Lwt.return_error e

  let (>>>=?) a b =
    Lwt.return a >>=? b

  let (>>>|?) a b =
    Lwt.return a >>|? b
end
open Error_monad

let all_nodes = Ipaddr.V6.of_string_exn "ff02::1"
let iface = Iface.(of_string_exn "eth0" |> to_int32)
let saddr = Sockaddr.of_ipv6_port ~scope_id:iface all_nodes 4444

let t, u = Lwt.wait ()

let receiver_opt msg =
  let socket = L.Socket.create Socket.inet6 Socket.dgram in
  L.bind socket saddr >>>=? fun () ->
  L.Sockopt.set socket Sockopt.v6_join_group saddr >>>=? fun () ->
  Lwt.wakeup u () ;
  L.recvfrom socket msg

let receiver () =
  let msg = Cstruct.create 2 in
  receiver_opt msg >>= function
  | Ok (nb_read, Sockaddr.V6 { addr; port; flowinfo; scope_id } ) ->
      assert (scope_id = iface) ;
      if nb_read <> 2  then Lwt.fail_with "error reading"
      else if Cstruct.LE.get_uint16 msg 0 <> 42 then Lwt.fail_with "error reading"
      else Lwt.return_unit
  | Error msg -> Lwt.fail_with msg

let sender () =
  let msg = Cstruct.create 2 in
  Cstruct.LE.set_uint16 msg 0 42 ;
  let socket = L.Socket.create Socket.inet6 Socket.dgram in
  t >>= fun () ->
  L.send ~saddr socket msg >>= function
  | Ok _nb_sent -> Lwt.return_unit
  | Error msg -> Lwt.fail_with msg

let runtest () =
  Lwt_main.run (receiver () <&> sender ())

let basic = [
  "basic", `Quick, runtest ;
]

let () =
  Alcotest.run "ipv6-multicast" [
    "basic", basic ;
  ]
