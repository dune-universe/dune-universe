open! Core
open Async
module Find = Async_extended.Std.Find

let main path =
  let options = Find.Options.default in
  Find.iter (Find.create ~options path)
    ~f:(fun (file, _) -> printf "%s\n" file; return ())
;;

let () =
  Command.async ~summary:"simple find like tool"
    (let open Command.Let_syntax in
     let%map_open
       path = anon ("PATH" %: file)
     in
     fun () -> main path)
  |> Command.run
;;
