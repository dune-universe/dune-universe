(*
 * Copyright (C) 2006-2014 Citrix Inc.
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type handle

external init': unit -> handle = "stub_evtchn_init"

let singleton_eventchn = ref None
let printed_error_already = ref false

let init () = match !singleton_eventchn with
  | Some e -> e
  | None ->
    try
      let e = init' () in
      singleton_eventchn := Some e;
      e
    with e ->
      if not(!printed_error_already) then begin
        printed_error_already := true;
        Printf.fprintf stderr "Failed to open event channel interface: ENOENT\n";
        Printf.fprintf stderr "Does this system have Xen userspace eventchannel support?\n";
        Printf.fprintf stderr "On linux try:\n";
        Printf.fprintf stderr "  sudo modprobe xen-evtchn\n%!";
      end;
      raise e

(* We'd rather leak connections than suffer use-after-free *)
let close _ = 0

type t = int Generation.t

external stub_bind_unbound_port: handle -> int -> int = "stub_evtchn_alloc_unbound"
external stub_bind_interdomain: handle -> int -> int -> int = "stub_evtchn_bind_interdomain"
external stub_unmask: handle -> int -> unit = "stub_evtchn_unmask"
external stub_notify: handle -> int -> unit = "stub_evtchn_notify" [@@noalloc]
external stub_unbind: handle -> int -> unit = "stub_evtchn_unbind"
external stub_virq_dom_exc: unit -> int = "stub_evtchn_virq_dom_exc"
external stub_bind_virq: handle -> int -> int = "stub_evtchn_bind_virq"

let construct f x = Generation.wrap (f x)
let bind_unbound_port h = construct (stub_bind_unbound_port h)
let bind_interdomain h remote_domid = construct (stub_bind_interdomain h remote_domid)

let maybe t f d = Generation.maybe t f d
let unmask h t = maybe t (stub_unmask h) ()
let notify h t = maybe t (stub_notify h) ()
let unbind h t = maybe t (stub_unbind h) ()
let is_valid t = maybe t (fun _ -> true) false

let of_int n = Generation.wrap n
let to_int t = Generation.extract t

let bind_dom_exc_virq h =
  let port = stub_bind_virq h (stub_virq_dom_exc ()) in
  construct (fun () -> port) ()
