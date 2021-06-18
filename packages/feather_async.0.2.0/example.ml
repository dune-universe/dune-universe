open! Base
open! Async
open! Stdio
open! Feather
open! Feather_async
open! Feather.Infix

let main () : unit Deferred.t =
  let%bind () = echo "hello world\nwow\ncool" |. sort |> run in
  match%bind ls "." < devnull |> fzf with
  | Some s -> echo [%string "hi there: %{s}"] |> run
  | None -> echo "got nothing" |> run

let () =
  Command.run
  @@ Command.async ~summary:"example feather async command"
       (Command.Param.return (fun () -> main ()))
