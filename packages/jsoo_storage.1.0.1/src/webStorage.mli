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

(** WebStorage is a wrapper around the DOMStorage API.
    The binding provides an OCaml API for using DOMStorage. The 
    library is fragmented in two submodules : Session and Local.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API>
    The reference on Mozilla Developer Network
*)


open Js_of_ocaml

(** Shortcut for [Dom_html.storage Js.t] *)
type t = Dom_html.storage Js.t


(** {2 Exceptions} *)

(** Raised if the Storage is not supported by the browser *)
exception Not_supported

(** This exception will be raised in particular case of conversion *)
exception Not_found


(** {2 Events} *)

(** Patch of StorageEvent (because Key could be null !) *)
class type storageEvent = 
object 
  inherit Dom_html.event
  method key : Js.js_string Js.t Js.opt Js.readonly_prop
  method oldValue : Js.js_string Js.t Js.opt Js.readonly_prop
  method newValue : Js.js_string Js.t Js.opt Js.readonly_prop
  method url : Js.js_string Js.t Js.readonly_prop
  method storageArea : Dom_html.storage Js.t Js.opt Js.readonly_prop
end

type event = storageEvent Js.t
val event : event Dom.Event.typ

(** {2 Interfaces} *)

(** The basic interface of a storage handler. 
    A Storage is basically a Key/Value store, using [string] for 
    the keys and the values.  This implémentation is a low-level 
    binding for the API. 

    The library uses [Hashtbl.t] as an output format for filtering.
*)
module type STORAGE = 
sig 

  type key = string 
  type value = string
  type old_value = string
  type url = string

  (** This type represents a changeset on the storage *)
  type change_state = 
    | Clear
    | Insert of key * value 
    | Remove of key * old_value 
    | Update of key * old_value * value


  (** Dump a changestate (mainly for debugging) *)
  val dump_change_state : change_state -> string


  (** [is_supported ()] returns [true] if the current storage is 
      supported by the browser, false otherwise.
   *)
  val is_supported: unit -> bool

  (** Returns the JavaScript's representation of the storage object *)
  val handler: t

  (** The length read-only property of the Storage interface returns an 
      integer representing the number of data items stored in the Storage object.
  *)
  val length: unit -> int

  (** When passed a key name, will return that key's value.  (Wrapped into an option) *)
  val get: key -> value option

  (**  When passed a key name and value, will add that key to the storage, 
       or update that key's value if it already exists.)
  *)
  val set: key -> value -> unit

  (** When passed a key name, will remove that key from the storage. *)
  val remove: key -> unit 

  (**  When invoked, clears all stored keys. *)
  val clear: unit -> unit

  (** When passed a number n, returns the name of the nth key in the storage. 
      The order of keys is [user-agent] defined, so you should not rely on it. *)
  val key: int -> key option

  (** Returns the couple Key/Value at a specific position  *)
  val at: int -> (key * value) option

  (** Produce an [Hashtbl.t] from a Storage *)
  val to_hashtbl: unit -> (key, value) Hashtbl.t

  (** applies function f on each key/value of a storage *)
  val iter: (key -> value -> unit) -> unit

  (** [find p] returns the first element of the storage that satisfies the predicate [p]. 
      The result is wrapped into an option.
  *)
  val find: (key -> value -> bool) -> (key * value) option


  (** [select p] returns all the elements of the storage that satisfy the predicate [p]. 
      The results is an [Hashtbl.t] of the results.
  *)
  val select: (key -> value -> bool) -> (key, value) Hashtbl.t

  (** [on_change f] trigger [f] at each changement of the storage. (You can use a [prefix]
      to trigger the events only if it concerns a set of keys (with the gived prefix)) 
  *)
  val on_change : 
    ?prefix:string 
    -> (change_state -> url -> unit) 
    -> Dom.event_listener_id


  (** [on_clear f] trigger [f] at each clear of the storage.  *)
  val on_clear: (url -> unit) -> Dom.event_listener_id


  (** [on_insert f] trigger [f] at each insertion in the storage. (You can use a [prefix]
      to trigger the events only if it concerns a set of keys (with the gived prefix)) 
  *)
  val on_insert:
    ?prefix:string  
    -> (key -> value -> url -> unit)
    -> Dom.event_listener_id

  (** [on_remove f] trigger [f] at each remove in the storage. (You can use a [prefix]
      to trigger the events only if it concerns a set of keys (with the gived prefix)) 
  *)
  val on_remove:
    ?prefix:string  
    -> (key -> old_value -> url -> unit)
    -> Dom.event_listener_id

  (** [on_update f] trigger [f] at each key update in the storage. (You can use a [prefix]
      to trigger the events only if it concerns a set of keys (with the gived prefix)) 
  *)
  val on_update:
    ?prefix:string  
    -> (key -> old_value -> value -> url -> unit)
    -> Dom.event_listener_id

end


(** {2 Concrete implementation} *)

(** Support for [LocalStorage] *)
module Local : STORAGE

(** Support for [SessionStorage] *) 
module Session : STORAGE
