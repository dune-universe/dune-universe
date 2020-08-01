open Js_of_ocaml

(* CR-someday:
   https://github.com/ocsigen/js_of_ocaml/pull/944

   There is also
   https://github.com/Voronar/jsoo-async-await/blob/master/promise.mli
*)

val to_lwt : 'a Promise.t Js.t -> 'a Lwt.t
val of_lwt : (unit -> 'a Lwt.t) -> 'a Promise.t Js.t
