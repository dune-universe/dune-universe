open Js_of_ocaml
open Js


class type js_iface =
  object
    method question: js_string t -> (js_string t -> unit) callback -> unit meth
    method close: unit meth
    method on:  js_string t -> (unit -> unit) callback -> unit meth
    method removeListener: js_string t -> (unit -> unit) callback -> unit meth
  end

type iface = js_iface t


let create_interface (): iface =
  Unsafe.eval_string
    "require('readline').createInterface(\
     {input: process.stdin, output: process.stdout})"


let question
      (rl:iface)
      (prompt:string)
      (cmd:string -> unit)
      (stop:unit -> unit)
    : unit
  =
  let cb = wrap_callback stop
  and str_close = string "close"
  in
  rl##on str_close cb;
  rl##question
    (string prompt)
    (wrap_callback @@
       fun jstr ->
       rl##removeListener str_close cb;
       cmd (to_string jstr))



let close (rl:iface): unit =
  rl##close
