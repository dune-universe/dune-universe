module Client = Redis_lwt.Client

module IntStr_params = struct
  type key = int
  type data = string

  let cache_key = string_of_int
  let cache_expiration = Some 200

  let data_of_string (str:string) = (str:data)
  let string_of_data (data:data) = (data:string)
end

module IntStrCache
  : (Redis.S.Cache with
        module Params = IntStr_params and
        module IO = Redis_lwt.IO and
        module Client = Redis_lwt.Client)
  = Redis.Cache.Make(Redis_lwt.IO)(Redis_lwt.Client)(IntStr_params)

let redis_test_host () =
  try Sys.getenv "OCAML_REDIS_TEST_IP"
  with Not_found -> "127.0.0.1"

let redis_test_port () = 63791

let redis_spec : Client.connection_spec =
  Client.({host=redis_test_host ();
           port=redis_test_port () })

let () =
  let open Lwt.Infix in
  let r =
    Client.with_connection redis_spec (fun conn ->
      Lwt_io.printl "connected" >>= fun () ->
      let value = String.make 300000 'c' in
      Lwt_result.bind_lwt (Lwt_result.return conn)
        (fun conn -> IntStrCache.set conn 1 value))
    |> Lwt_main.run
  in
  match r with
  | Ok () -> ()
  | Error e -> print_endline e; exit 1

