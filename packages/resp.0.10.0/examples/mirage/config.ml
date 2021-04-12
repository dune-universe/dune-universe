open Mirage

let main =
  foreign
    ~packages:
      [ package "duration"; package "resp-mirage"; package "conduit-mirage" ] "Unikernel.Main"
    (pclock @-> conduit @-> job)

let server = generic_stackv4 default_network |> conduit_direct

let () = register "resp-server" [ main $ default_posix_clock $ server ]
