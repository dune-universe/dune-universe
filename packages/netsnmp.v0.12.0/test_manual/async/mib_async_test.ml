open! Core
open Async
open Netsnmp_async

let mibdirs paths = 
  Deferred.List.iter paths ~f:(fun p -> 
    let%map num = Raw.Mib.add_mibdir p in printf "mibs(%s) = %d\n%!" p num)

let test paths save_descr = 
  if save_descr then Raw.Mib.snmp_set_save_descriptions true else Deferred.unit
  >>= fun () -> Raw.Mib.netsnmp_init_mib ()
  >>= fun () -> mibdirs paths

let () =
  Command.async_spec
    ~summary:"test mib access"
    Command.Spec.(
      empty
      +> flag "-mib-path" (listed string)
        ~doc:"PATH location of additional mib files"
      +> flag "-save-descr" no_arg
        ~doc:" include MIB descriptions"
    )
    (fun path save_descr () -> test path save_descr)
  |> Command.run
