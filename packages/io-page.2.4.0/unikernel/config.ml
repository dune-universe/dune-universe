open Mirage

let io_page =
  foreign "Unikernel.Make"
    (console @-> job)

let () =
  register "io-page" ~packages:[ package "io-page" ]
    [ io_page $ default_console ]
