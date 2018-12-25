let make_proxy bus_name =
  let%lwt bus = OBus_bus.session () in
  Lwt.return (OBus_proxy.make
    ~peer:(OBus_peer.make ~connection:bus ~name:bus_name)
    ~path:["org"; "mpris"; "MediaPlayer2"])
