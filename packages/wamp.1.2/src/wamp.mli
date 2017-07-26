(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** wamp-proto.org implementation in OCaml

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Wamp} *)

type msgtyp =
  | HELLO
  | WELCOME
  | ABORT
  | GOODBYE
  | ERROR
  | PUBLISH
  | PUBLISHED
  | SUBSCRIBE
  | SUBSCRIBED
  | UNSUBSCRIBE
  | UNSUBSCRIBED
  | EVENT

val msgtyp_of_enum : int -> msgtyp option
val msgtyp_to_enum : msgtyp -> int

type 'a dict = (string * 'a) list

type 'a msg =
  | Hello of { realm: Uri.t; details: 'a dict }
  | Welcome of { id: int; details: 'a dict }
  | Abort of { details: 'a dict; reason: Uri.t }
  | Goodbye of { details: 'a dict; reason: Uri.t }
  | Error of { reqtype: int; reqid: int; details: 'a dict; error: Uri.t; args: 'a list; kwArgs: 'a dict }
  | Publish of { reqid: int; options: 'a dict; topic: Uri.t; args: 'a list; kwArgs: 'a dict }
  | Published of { reqid: int; id: int }
  | Subscribe of { reqid: int; options: 'a dict; topic: Uri.t }
  | Subscribed of { reqid: int; id: int }
  | Unsubscribe of { reqid: int; id: int }
  | Unsubscribed of int
  | Event of { subid: int; pubid: int; details: 'a dict; args: 'a list; kwArgs: 'a dict }

val hello : realm:Uri.t -> details:'a dict -> 'a msg
val welcome : id:int -> details:'a dict -> 'a msg
val abort : details:'a dict -> reason:Uri.t -> 'a msg
val goodbye : details:'a dict -> reason:Uri.t -> 'a msg
val error :
  reqtype:int -> reqid:int -> details:'a dict ->
  error:Uri.t -> args:'a list -> kwArgs:'a dict -> 'a msg
val publish :
  reqid:int -> options:'a dict -> topic:Uri.t ->
  args:'a list -> kwArgs:'a dict -> 'a msg
val published : reqid:int -> id:int -> 'a msg
val subscribe : reqid:int -> options:'a dict -> topic:Uri.t -> 'a msg
val subscribed : reqid:int -> id:int -> 'a msg
val unsubscribe : reqid:int -> id:int -> 'a msg
val unsubscribed : reqid:int -> 'a msg
val event :
  subid:int -> pubid:int -> details:'a dict ->
  args:'a list -> kwArgs:'a dict -> 'a msg

type role = Subscriber | Publisher
val string_of_role : role -> string

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
