(* MIT License
 * 
 * Copyright (c) 2017 Xavier Van de Woestyne
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
*)

open Js_of_ocaml
exception Not_supported

class type ['message] messageEvent = 
  ['message] EventSource.messageEvent

type 'a message = 'a messageEvent Js.t

class type ['message] broadcaster = 
  object ('self)
    inherit Dom_html.eventTarget
    method name  : (Js.js_string Js.t) Js.readonly_prop
    method close : unit -> unit Js.meth
    method postMessage :  'message -> unit Js.meth
    method onmessage: 
      ('self Js.t, 'message message) Dom_html.event_listener Js.writeonly_prop
  end

type 'a t = 'a broadcaster Js.t

let constr = Js.Unsafe.global##._BroadcastChannel
let is_supported () = Js.Optdef.test constr

let create name = 
  if is_supported () 
  then new%js constr (Js.string name)
  else raise Not_supported

let close bus = 
  ignore (bus ## close())

let name bus = 
  Js.to_string (bus##.name)

let post bus message = 
  ignore (bus##postMessage(message))

let on bus f = 
  bus##.onmessage := (Dom.handler f)

let addEventListener = 
  Dom.addEventListener

let message _ = 
  Dom.Event.make "message"

let lwt_js_message
    ?(use_capture = false)
    ?(passive = false)
    target
  = 
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () = Js.Opt.iter !el Dom.removeEventListener in
  Lwt.on_cancel t cancel;
  el := Js.some
      (Dom.addEventListenerWithOptions
         ~capture:(Js.bool use_capture)
         ~passive:(Js.bool passive)
         target (message target)
         (Dom.handler (fun ev -> cancel (); Lwt.wakeup w ev; Js.bool true))
      );
  t

let create_with name _ =
  let bus = create name in 
  (bus, message bus)
