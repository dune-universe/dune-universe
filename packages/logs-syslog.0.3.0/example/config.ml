open Mirage

let packages = [
  package "duration";
  package ~sublibs:["mirage"] "logs-syslog";
  package ~sublibs:["lwt"] "logs"
]

let handler =
  foreign ~packages "Unikernel.Main"
    (console @-> pclock @-> time @-> stackv4v6 @-> job)

let stack = generic_stackv4v6 default_network

let () =
  register "syslog" [handler $ default_console $ default_posix_clock $ default_time $ stack]
