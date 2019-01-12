open Mirage

let main = foreign "Unikernel.Main" (console @-> job)

let platform =
    match get_mode () with
        | `Xen -> "xen"
        | _ -> "unix"

let () =
    add_to_ocamlfind_libraries [ 
        "mirage-vnetif" ; 
        "mirage-net-" ^ platform ; 
        "mirage-" ^ platform; 
        "mirage-clock-" ^ platform;
        "tcpip.stack-direct" ; 
        "mirage-types" ];
  register "unikernel" [
    main $ default_console
  ]
