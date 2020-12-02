open Ezcmd.V1
open Ezcmd.TYPES

let toto = ref false

let () =
  Ezcmd.parse ~name:"test3"
    [ ("-toto", Arg.Set toto, "Documentation on toto") ]
    (fun s ->
       Printf.printf "toto: %b\narg: %s\n%!" !toto s
    )
    "test3 [OPTIONS] [ARGUMENTS]"
