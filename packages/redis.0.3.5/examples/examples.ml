open Core.Std
open Redis
open Subscribe_sync
open Subscribe_lwt

let run_example example host port () =
  match example with
  | "subscribe_sync" -> subscribe_sync host port
  | "subscribe_lwt" -> subscribe_lwt host port
  | _ -> failwith "no such example"

let command =
  Command.basic
    ~summary:"Example of usage ocaml-redis"
    Command.Spec.(empty
                  +> anon ("example_name" %: string)
                  +> anon ("host" %: string)
                  +> anon ("port" %: int))
    run_example

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
