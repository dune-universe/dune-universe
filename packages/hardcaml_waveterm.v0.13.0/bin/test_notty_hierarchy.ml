(* Test the notty based waveterm interactive viewer. *)
open! Import
open! Core
open! Async

let () =
  Command.async
    ~summary:"Test fullscreen interactive viewer"
    (let open Command.Let_syntax in
     let%map_open () = return ()
     and signals_width =
       flag
         "-signals-width"
         (optional_with_default 20 int)
         ~doc:"<int> signals window width"
     and values_width =
       flag
         "-values-width"
         (optional_with_default 20 int)
         ~doc:"<int> values window width"
     in
     let prefix =
       let a = [| ""; "foo$"; "foo$bar$"; "foo$baz$"; "hello$"; "world$" |] in
       fun i -> a.(Int.rem i (Array.length a))
     in
     fun () ->
       Widget.run_waves
         ~signals_width
         ~values_width
         Test_data.(create ~prefix ~length:1000 ~num_signals:100))
  |> Command.run
;;
