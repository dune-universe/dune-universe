(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
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
open Lwt

module Make = struct
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    type id = int
    type macaddr = Macaddr.t

    type c = {
        mutable callback_counter : int;
        cond : int Lwt_condition.t;
        mutex : Lwt_mutex.t;
    }

    type t = {
        mutable last_id : int;
        mutable call_counter : int;
        use_async_readers : bool;
        yield : (unit -> unit io);
        listener_callback : ((buffer -> unit io) -> c -> buffer -> unit io);
        listener_callbacks_in_progress : (int, c) Hashtbl.t;
        listeners : (int, buffer -> unit io) Hashtbl.t;
        macs : (int, macaddr) Hashtbl.t;
    }

    let make_mac id =
        let i = Int32.of_int id in
        let byte v shr = Int32.to_int (Int32.logand (Int32.shift_right_logical v shr) 0xFFl) in
        let base_mac = [| 0 ; 0x50 ; (byte i 24) ; (byte i 16) ; (byte i 8) ; (byte i 0) |] in (* TODO Use different prefix? *)
        Macaddr.make_local (Array.get base_mac)

    let dec_callback_counter c =
        Lwt_mutex.with_lock c.mutex (
            fun () -> (c.callback_counter <- c.callback_counter - 1);
            Lwt.return_unit
        ) >>= fun () ->
        Lwt.return (Lwt_condition.signal c.cond 0)

    let inc_callback_counter c =
        Lwt_mutex.with_lock c.mutex (
            fun () -> (c.callback_counter <- c.callback_counter + 1);
                      Lwt.return_unit
        ) >>= fun () ->
        Lwt.return (Lwt_condition.signal c.cond 0)

    let create ?(yield=(fun () -> Lwt.pause ())) ?(use_async_readers=false) () =
        let last_id = 0 in
        let call_counter = 0 in
        let listeners = Hashtbl.create 7 in
        let macs = Hashtbl.create 7 in
        let listener_callbacks_in_progress = Hashtbl.create 7 in
        if use_async_readers then
            let listener_callback f c buffer =
                inc_callback_counter c >>= fun () ->
                Lwt.async (fun () ->
                    f buffer >>= fun () ->
                    dec_callback_counter c);
                Lwt.return_unit
            in
            {last_id;call_counter;listeners;macs;listener_callbacks_in_progress;yield;use_async_readers;listener_callback}
        else
            let listener_callback f c buffer =
                inc_callback_counter c >>= fun () ->
                f buffer >>= fun () ->
                dec_callback_counter c
            in
            {last_id;call_counter;listeners;macs;listener_callbacks_in_progress;yield;use_async_readers;listener_callback}

    let register t =
        t.last_id <- t.last_id + 1;
        Hashtbl.add t.macs t.last_id (make_mac t.last_id);
        Hashtbl.add t.listener_callbacks_in_progress t.last_id {
            callback_counter = 0;
            cond = Lwt_condition.create();
            mutex = Lwt_mutex.create() };
        Ok t.last_id

    let unregister t id =
        Hashtbl.remove t.macs id;
        Hashtbl.remove t.listeners id;
        Hashtbl.remove t.listener_callbacks_in_progress id;
        Lwt.return_unit

    let wait_for_callbacks c =
        Lwt_mutex.with_lock c.mutex (fun () ->
            let rec loop = function
                | 0 -> Lwt.return_unit
                | _ -> (Lwt_condition.wait ~mutex:c.mutex c.cond >>= fun _ ->
                       (loop c.callback_counter))
            in
            loop c.callback_counter
        )

    let unregister_and_flush t id =
        let c = Hashtbl.find t.listener_callbacks_in_progress id in
        unregister t id >>= fun () ->
        wait_for_callbacks c

    let mac t id =
        Hashtbl.find t.macs id

    let set_listen_fn t id fn =
        Hashtbl.replace t.listeners id fn

    let buffer_copy src =
        let len = Cstruct.len src in
        let dst = Cstruct.create len in
        Cstruct.blit src 0 dst 0 len;
        dst

    let write t id ~size fill =
        let keys = Hashtbl.fold (fun k _v lst -> k::lst) t.listeners [] in
        let send t buf src dst =
          if src != dst then
            begin
              t.call_counter <- t.call_counter + 1;
              let fn = (Hashtbl.find t.listeners dst) in
              let c = (Hashtbl.find t.listener_callbacks_in_progress dst) in
              t.listener_callback fn c (buffer_copy buf)
            end else
            Lwt.return_unit
        in
        let buf = Cstruct.create size in
        let len = fill buf in
        assert (len <= size) ;
        let buf = Cstruct.sub buf 0 len in
        Lwt_list.iter_s (send t buf id) keys >>= fun () ->
        t.yield () >|= fun () -> Ok ()

end
