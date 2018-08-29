open Lwt

let main () =
  let t =
    Camltc.Hotc.create "/tmp/bla.db" [] >>= fun db ->
    Camltc.Hotc.close db
  in
  Lwt_main.run t;;
