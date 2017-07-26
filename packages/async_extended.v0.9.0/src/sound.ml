open Core
open Async

type t =
| Default
| File of string

let of_file s = File s

let usual_place name = Filename.concat "/j/office/app/sounds" (name ^ ".wav")

let to_file = function
  | Default -> usual_place "info"
  | File s -> s

let default = Default


let play =
  let seq = lazy (Throttle.Sequencer.create ()) in
  fun ?(quiet=true) t ->
    Throttle.enqueue (Lazy.force seq) (fun () ->
      let redirect = if quiet then " 2> /dev/null" else "" in
      let command = "esdplay " ^ to_file t ^ redirect in
      Unix.system command
      >>| (function status ->
            if not (Result.is_ok status) then
              failwithf "command failed: %s" command ()))
;;
