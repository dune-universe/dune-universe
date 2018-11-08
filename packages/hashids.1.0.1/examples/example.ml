(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

let config = Hashids.make ()

let () =
  [4; 8; 15; 16; 23; 42]
  |> Hashids.encode config
  |> print_endline
