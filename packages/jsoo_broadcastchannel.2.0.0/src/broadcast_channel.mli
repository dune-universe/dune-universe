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

(** [jsoo_broadcastchannel] provides a wrapper around the Broadcast_channel API 
    in JavaScript. 
    The Broadcast_channel interface represents a named channel that any browsing context 
    of a given origin can subscribe to. It allows communication between different documents 
    (in different windows, tabs, frames or iframes) of the same origin. Messages are 
    broadcasted via a message event fired at all Broadcast_channel objects listening 
    to the channel.

    Example of use : 

    Creating a channel an post message (on a first file)  : 

    {[
      let channel = Broadcast_channel.create "my_first_channel"
      let _ = Broadcast_channel.post channel (Js.string "Hello World")
    ]}

    Receiving message from the channel [my_first_channel] on an another file 
    with [onmessage] function :

    {[
      (* Retreive the channel *)
      let channel : Js.string Js.t Broadcast_channel.t = 
        Broadcast_channel.create "my_first_channel"
      (* You have to fix the type of the channel, you can also use [Broadcast_channel.create_with] *)

      let _ = 
        Broadcast_channel.on
          channel 
          (fun ev -> 
            (* Use the ev object *)
            Js._true
          )
    ]}

    Receiving message from the channel [my_first_channel] on an another file 
    with [addEventListener] function :

    {[
      (* Retreive the channel *)
      let channel : Js.string Js.t Broadcast_channel.t = 
          Broadcast_channel.create "my_first_channel"
      (* You have to fix the type of the channel, you can also use [Broadcast_channel.create_with] *)

      let _ = 
        Broadcast_channel.addEventListener
          channel
          (Broadcast_channel.message channel)
          (Dom.handler (fun ev -> ... Js._true))
          Js._true
    ]}

    Or you can use [Broadcast_channel.create_with] (for a more conveinent usage)

    {[
      (* Retreive the channel *)
      let (channel, message_event) = 
        Broadcast_channel.create_with 
          "my_first_channel"
          (Js.string "a sample")

      let _ = 
        Broadcast_channel.addEventListener
          channel
          message_event
          (Dom.handler (fun ev -> ... Js._true))
          Js._true
    ]}

    Receiving message from the channel [my_first_channel] on an another file 
    with [Lwt_js_events] :

    {[
      (* Retreive the channel *)
      let channel : Js.string Js.t Broadcast_channel.t = 
        Broadcast_channel.create "my_first_channel"

      let _ = 
        Lwt_js_events.async_loop 
          Broadcast_channel.lwt_js_message
          channel
          (fun ev _ -> 
            ... 
            Lwt.return_unit
          )
    ]}


    @see <https://developer.mozilla.org/en-US/docs/Web/API/BroadcastChannel>
    The BroadcastChannel API
 *)


(** {1 Exceptions and types} *)

open Js_of_ocaml

(** Exception if Broadcast_channel is not supported *)
exception Not_supported

(** Class type to define a messageEvent 
    @see <http://ocsigen.org/js_of_ocaml/2.8.4/api/EventSource.messageEvent-c>
    The EventSource of Js_of_OCaml API
*)
class type ['message] messageEvent = 
  ['message] EventSource.messageEvent

(** Shortcut for a messageEvent *)
type 'a message = 'a messageEvent Js.t

(** Interface of a Broadcast_channel *)
class type ['message] broadcaster = 
object ('self)
  inherit Dom_html.eventTarget
  method name  : (Js.js_string Js.t) Js.readonly_prop
  method close : unit -> unit Js.meth
  method postMessage :  'message -> unit Js.meth
  method onmessage: 
    ('self Js.t, 'message message) Dom_html.event_listener Js.writeonly_prop
end

(** Shortcut for a broadcaster *)
type 'a t = 'a broadcaster Js.t

(** {1 Common functions} *)

(** Returns [true] if Broadcast_channel is supported by the 
    client's browser, false otherwise.
  *)
val is_supported : unit -> bool

(** Creates a Broadcast_channel with a name. Raise [Not_supported "Broadcast_channel"] 
    if Broadcast_channel is not supported by the client's browser.
*)
val create: string -> 'message t

(** Creates a Broadcast_channel with a name. Raise [Not_supported "Broadcast_channel"] 
    if Broadcast_channel is not supported by the client's browser.
    The functions takes a "sample of a message" to fix the types of the broadcaster. 
    The functions returns a couple of the Broadcast_channel and the [Event] (to be used)
    in [addEventListener].
*)
val create_with: string -> 'a -> ('a t * 'a message Dom.Event.typ)


(** Closes the channel object, indicating it won't get any new messages, 
    and allowing it to be, eventually, garbage collected. 
*)
val close:  'message t -> unit

(** Returns a [string], the name of the channel. *)
val name:   'message t -> string

(** Sends the message, of the broadcaster type to each Broadcast_channel 
    object listening to the same channel. 
*)
val post:   'message t -> 'message -> unit

(** Is an [EventHandler] property that specifies the function to execute
    when a message event is fired on this object.
*)
val on:     'message t -> ('message message -> bool Js.t) -> unit


(** {1 Event support} *)

(** Add an event listener. This function matches the [addEventListener] 
    DOM method, except that it returns an id for removing the listener. 
*)
val addEventListener : 
  'a t
  -> 'a message Dom.Event.typ
  -> ('a t, 'a message) Dom.event_listener
  -> bool Js.t 
  -> Dom.event_listener_id

(** An event to be used with [addEventListener] *)
val message : 'a t -> 'a message Dom.Event.typ

(** An event to be used with [Lwt_js_events] *)
val lwt_js_message: 
  ?use_capture:bool
  -> ?passive:bool
  -> 'a t 
  -> ('a messageEvent) Js.t Lwt.t
