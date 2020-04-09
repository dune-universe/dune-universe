open Base
open Async
open Influxdb_async

let test_ping host () = Client.ping host >>= fun _ -> return ()

let test_write host () =
  let field = Influxdb.Field.int 123 in
  let points =
    [
      Influxdb.Point.create ~field "thing"; Influxdb.Point.create ~field "thing";
    ]
  in
  Client.write ~database:"thing" ~host points >>= fun _ -> return ()

let test_write_with_ts host () =
  let timestamp =
    Unix.gettimeofday () |> Influxdb.TimestampNS.of_float_seconds
  in
  let field = Influxdb.Field.int 123 in
  let points =
    [
      Influxdb.Point.create ~timestamp ~field "thing";
      Influxdb.Point.create ~field "thing";
    ]
  in
  Client.write ~database:"thing" ~host points >>= fun _ -> return ()

let test_host = "localhost"

(** 
   These tests expect a running instance of influxdb with a database called "thing".
   The easiest way to get one up and running is via docker:

   docker run -e "INFLUXDB_DB=thing" influxdb

   Edit "test_host" above with the ip of the container
*)
let () =
  let _ =
    Alcotest_async.run "influxdb-async"
      [
        ( "all",
          [
            Alcotest_async.test_case "ping" `Quick (test_ping test_host);
            Alcotest_async.test_case "test_write" `Quick (test_write test_host);
            Alcotest_async.test_case "test_write_with_ts" `Quick
              (test_write_with_ts test_host);
          ] );
      ]
  in
  Async_unix.Scheduler.go () |> ignore
