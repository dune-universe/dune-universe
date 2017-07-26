open Core.Std
open Lwt

let subscribe_lwt host port =
  let open Redis_lwt.Client in

  let print_value = function
    | `Bulk Some str -> Lwt_io.printf "%s " str
    | `Error str -> Lwt_io.printf "error: %s " str
    | `Status str -> Lwt_io.printf "status: %s " str
    | `Int i -> Lwt_io.printf "int: %d " i
    | _ -> return () in

  let print_stream_value v =
    Lwt_list.iter_s print_value v;
    Lwt_io.printf "%s" "\n";
    Lwt_io.flush Lwt_io.stdout in

  let t = (connect {host=host; port=port})
          >>= (fun conn -> subscribe conn ["example"]; return conn)
          >>= (fun conn -> Lwt_stream.iter_s print_stream_value (stream conn)) in
  Lwt_unix.run t;
  ()
