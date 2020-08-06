open Core
open Async
module Jsoo = Js_of_ocaml_webidl.Bindings

let main ~input ~output_module ~errors_file =
  let jsoo = Webidl.Parse.data_from_file input |> Jsoo.create in
  let mli = Jsoo.mli jsoo |> String.concat ~sep:"\n" in
  let ml = Jsoo.ml jsoo |> String.concat ~sep:"\n" in
  let errors = [%message (Jsoo.errors jsoo : Error.t list)] in
  let ml_file = sprintf "%s.ml" output_module in
  let mli_file = sprintf "%s.mli" output_module in
  let%bind () = Writer.save mli_file ~contents:mli in
  let%bind () = Writer.save ml_file ~contents:ml in
  let%bind () = Writer.save_sexp errors_file errors in
  return ()
;;

let command =
  let open Command.Let_syntax in
  Command.async
    ~readme:(fun () -> "")
    ~summary:""
    [%map_open
      let input = anon ("anon" %: Filename.arg_type)
      and output_module =
        flag "output-module" (required Filename.arg_type) ~doc:""
      and errors_file =
        flag "errors-file" (required Filename.arg_type) ~doc:""
      in
      fun () -> main ~input ~output_module ~errors_file]
;;

let () = Command.run command
