open Core
open Async

module Json_async = struct
  module Json_of_async = Jsonxt.Basic_monad.Make(struct
      type 'a t = 'a Deferred.t

      let return = Deferred.return
      let (>>=) = Deferred.Monad_infix.(>>=)
    end)


  let reader inc buf size =
    Reader.read inc ~len:size buf
    >>= function
    | `Eof -> return 0
    | `Ok len -> return len

  let read inc =
    let reader = reader inc in
    Json_of_async.read_json ~reader ()

  let write outc =
    let writer buf = Writer.write outc buf |> return in
    Json_of_async.write_json ~writer

end

let run () =
  Reader.open_file "./asyncdata.json"
  >>= fun inc -> Json_async.read inc
  >>= function
      | Error err -> raise (Failure err)
      | Ok json -> begin
          Json_async.write (force Writer.stdout) json
          >>= fun () -> printf "\n"; shutdown 0 |> return
        end

let () =
  ignore (run ());
  never_returns (Scheduler.go ())
