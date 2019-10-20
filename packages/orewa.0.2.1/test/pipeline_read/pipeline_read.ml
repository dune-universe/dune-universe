open Core
open Async

let host = "localhost"

let key = "testkey1"

let rec req conn =
  match%bind Orewa.get conn key with
  | Ok _ -> req conn
  | Error _ -> eprintf "Done\n%!"; return ()

let main () =
  let%bind conn = Orewa.connect ?port:None ~host in
  let%bind _res = Orewa.set conn ~key "test" in
  let l = List.init 20 ~f:(fun _ -> req conn) in
  Deferred.List.all_unit l

let () =
  Command.async
    ~summary:"Run continous read"
    (Command.Let_syntax.return (fun () -> main ()))
  |> Command.run
