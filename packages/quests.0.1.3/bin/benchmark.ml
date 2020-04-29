let urls = List.init 50 (fun _ -> "https://postman-echo.com/get")

let without_session () =
  let t0 = Unix.gettimeofday () in
  let%lwt _ = Lwt_list.map_s (fun url -> Quests.get url) urls in
  Format.printf "Without a persistent connection: %f\n"
    (Unix.gettimeofday () -. t0)
  |> Lwt.return

let with_session () =
  let open Quests in
  let s = Session.create ~max_pool_size:1 () in
  let%lwt _ = Session.get s "https://postman-echo.com/get" in
  let t0 = Unix.gettimeofday () in
  let%lwt _ = Lwt_list.map_s (fun url -> Session.get s url) urls in
  Format.printf "With a persistent connection: %f\n" (Unix.gettimeofday () -. t0)
  |> Lwt.return

let with_session_medium_pool () =
  let open Quests in
  let s = Session.create ~max_pool_size:5 () in
  let%lwt _ = Session.get s "https://postman-echo.com/get" in
  let t0 = Unix.gettimeofday () in
  let%lwt _ = Lwt_list.map_s (fun url -> Session.get s url) urls in
  Format.printf "With a pool of persistent connections: %f\n"
    (Unix.gettimeofday () -. t0)
  |> Lwt.return

let with_session_big_pool () =
  let open Quests in
  let s = Session.create ~max_pool_size:50 () in
  let%lwt _ = Session.get s "https://postman-echo.com/get" in
  let t0 = Unix.gettimeofday () in
  let%lwt _ = Lwt_list.map_s (fun url -> Session.get s url) urls in
  Format.printf "With a huge pool of persistent connections: %f\n"
    (Unix.gettimeofday () -. t0)
  |> Lwt.return

let () =
  Lwt_main.run
    (Lwt_list.iter_s
       (fun f -> f ())
       [
         without_session;
         with_session;
         with_session_medium_pool;
         with_session_big_pool;
       ])
