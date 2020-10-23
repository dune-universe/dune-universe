(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2015 Citrix Inc
 * Copyright (c) 2015 Thomas Leonard <talex5@gmail.com>
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

open Lwt.Infix
open Mirage_net

module Gntref = OS.Xen.Gntref
module Import = OS.Xen.Import

let src = Logs.Src.create "net-xen backend" ~doc:"Mirage's Xen netback"
module Log = (val Logs.src_log src : Logs.LOG)

let return = Lwt.return

module Cleanup : sig
  type t
  (** A stack of (cleanup) actions to perform.
      This is a bit like [Lwt_switch], but ensures things happen in order. *)

  val create : unit -> t

  val push : t -> (unit -> unit Lwt.t) -> unit
  (** [push t fn] adds [fn] to the stack of clean-up operations to perform. *)

  val perform : t -> unit Lwt.t
  (** [perform t] pops and performs actions from the stack until it is empty. *)
end = struct
  type t = (unit -> unit Lwt.t) Stack.t

  let create = Stack.create

  let push t fn = Stack.push fn t

  let rec perform t =
    if Stack.is_empty t then Lwt.return_unit
    else (
      let fn = Stack.pop t in
      fn () >>= fun () ->
      perform t
    )
end

module Make(C: S.CONFIGURATION) = struct
  exception Netback_shutdown

  type error = Mirage_net.Net.error
  let pp_error = Mirage_net.Net.pp_error

  type t = {
    channel: OS.Eventchn.t;
    frontend_id: int;
    mac: Macaddr.t;
    frontend_mac: Macaddr.t;
    mtu: int;
    backend_configuration: S.backend_configuration;
    mutable to_netfront: (RX.Response.t,int) Ring.Rpc.Back.t option;
    rx_reqs: RX.Request.t Lwt_dllist.t;         (* Grants we can write into *)
    mutable from_netfront: (TX.Response.t,int) Ring.Rpc.Back.t option;
    stats: stats;
    write_mutex: Lwt_mutex.t;
    get_free_mutex: Lwt_mutex.t;
  }

  let h = OS.Eventchn.init ()

  let create ~switch ~domid ~device_id =
    let id = `Server (domid, device_id) in
    let cleanup = Cleanup.create () in
    Lwt_switch.add_hook (Some switch) (fun () -> Cleanup.perform cleanup);
    Cleanup.push cleanup (fun () -> C.disconnect_backend id);
    C.read_backend_mac id >>= fun mac ->
    C.read_frontend_mac id >>= fun frontend_mac ->
    C.init_backend id Features.supported >>= fun backend_configuration ->
    let frontend_id = backend_configuration.S.frontend_id in
    C.read_frontend_configuration id >>= fun f ->
    let channel = OS.Eventchn.bind_interdomain h frontend_id (int_of_string f.S.event_channel) in
    Cleanup.push cleanup (fun () -> OS.Eventchn.unbind h channel; return ());
    (* Note: TX and RX are from netfront's point of view (e.g. we receive on TX). *)
    let from_netfront =
      let tx_gnt = {Import.domid = frontend_id; ref = Gntref.of_int32 f.S.tx_ring_ref} in
      let mapping = Import.map_exn tx_gnt ~writable:true in
      Cleanup.push cleanup (fun () -> Import.Local_mapping.unmap_exn mapping; return ());
      let buf = Import.Local_mapping.to_buf mapping |> Io_page.to_cstruct in
      let sring = Ring.Rpc.of_buf_no_init ~buf ~idx_size:TX.total_size
        ~name:("Netif.Backend.TX." ^ backend_configuration.S.backend) in
      Ring.Rpc.Back.init ~sring in
    let to_netfront =
      let rx_gnt = {Import.domid = frontend_id; ref = Gntref.of_int32 f.S.rx_ring_ref} in
      let mapping = Import.map_exn rx_gnt ~writable:true in
      Cleanup.push cleanup (fun () -> Import.Local_mapping.unmap_exn mapping; return ());
      let buf = Import.Local_mapping.to_buf mapping |> Io_page.to_cstruct in
      let sring = Ring.Rpc.of_buf_no_init ~buf ~idx_size:RX.total_size
        ~name:("Netif.Backend.RX." ^ backend_configuration.S.backend) in
      Ring.Rpc.Back.init ~sring in
    let stats = Stats.create () in
    let rx_reqs = Lwt_dllist.create () in
    OS.Eventchn.unmask h channel;
    C.connect id >>= fun () ->
    let write_mutex = Lwt_mutex.create () in
    let get_free_mutex = Lwt_mutex.create () in
    C.read_mtu id >>= fun mtu ->
    let t = {
      channel; frontend_id; backend_configuration;
      to_netfront = Some to_netfront; from_netfront = Some from_netfront; rx_reqs;
      get_free_mutex; write_mutex;
      stats; mac; frontend_mac; mtu; } in
    Cleanup.push cleanup (fun () ->
      t.to_netfront <- None;
      t.from_netfront <- None;
      return ()
    );
    Lwt.async (fun () ->
        C.wait_for_frontend_closing id >>= fun () ->
        Log.info (fun f -> f "Frontend asked to close network device dom:%d/vif:%d" domid device_id);
        Lwt_switch.turn_off switch
      );
    return t

  let make ~domid ~device_id =
    let switch = Lwt_switch.create () in
    Lwt.catch
      (fun () -> create ~switch ~domid ~device_id)
      (fun ex -> Lwt_switch.turn_off switch >>= fun () -> Lwt.fail ex)

  (* Loop checking for incoming requests on the from_netfront ring.
     Frames received will go to [fn]. *)
  let listen (t: t) ~header_size:_ fn : (unit, error) result Lwt.t =
    let from_netfront () =
      match t.from_netfront with
      | None -> raise Netback_shutdown
      | Some x -> x in
    let module Recv = Assemble.Make(TX.Request) in
    let rec loop after =
      let q = ref [] in
      Ring.Rpc.Back.ack_requests (from_netfront ())
        (fun slot ->
          match TX.Request.read slot with
          | Error msg -> Log.warn (fun f -> f "read_read TX has unparseable request: %s" msg)
          | Ok req ->
            q := req :: !q
        );
      (* -- at this point the ring slots may be overwritten, but the grants are still valid *)
      List.rev !q
      |> Recv.group_frames
      |> Lwt_list.iter_s (function
        | Error (e, _) -> e.TX.Request.impossible
        | Ok frame ->
            let data = Cstruct.create frame.Recv.total_size in
            let next = ref 0 in
            frame.Recv.fragments |> Lwt_list.iter_s (fun {Recv.size; msg} ->
              let { TX.Request.flags = _; size = _; offset; gref; id } = msg in
              let gnt = { Import.
                domid = t.frontend_id;
                ref = Gntref.of_int32 gref
              } in
              Import.with_mapping gnt ~writable:false (fun mapping ->
                  let buf = Import.Local_mapping.to_buf mapping |> Io_page.to_cstruct in
                  Cstruct.blit buf offset data !next size;
                  next := !next + size;
                  let slot =
                    let ring = from_netfront () in
                    Ring.Rpc.Back.(slot ring (next_res_id ring)) in
                  let resp = { TX.Response.id; status = TX.Response.OKAY } in
                  TX.Response.write resp slot;
                  return ()
                )
              >|= function
              | Error (`Msg m) -> failwith m   (* Couldn't map client's grant; give up *)
              | Ok () -> ()
            ) >|= fun () ->
            assert (!next = Cstruct.len data);
            Stats.rx t.stats (Int64.of_int (Cstruct.len data));
            Lwt.async (fun () ->
              Lwt.catch (fun () -> fn data)
                (function
                  | Out_of_memory -> Lwt.fail Out_of_memory
                  | ex ->
                    Log.err (fun f -> f "uncaught exception from listen callback while handling frame:@\n@[<v2>  %a@]@\nException: @[%s@]"
                                Cstruct.hexdump_pp data (Printexc.to_string ex));
                    Lwt.return ()
                )
              )
      )
      >>= fun () ->
      let notify = Ring.Rpc.Back.push_responses_and_check_notify (from_netfront ()) in
      if notify then OS.Eventchn.notify h t.channel;
      OS.Activations.after t.channel after
      >>= loop in
    Lwt.catch
      (fun () -> loop OS.Activations.program_start >|= fun `Never_returns -> assert false)
      (function
        | Netback_shutdown -> Lwt.return (Ok ())
        | ex -> Lwt.fail ex
      )

  let to_netfront t =
    match t.to_netfront with
    | None -> raise Netback_shutdown
    | Some x -> x

  (* We need [n] pages to send a packet to the frontend. The Ring.Back API
     gives us all the requests that are available at once. Since we may need
     fewer of this, stash them in the t.rx_reqs sequence.
     Raises [Netback_shutdown] if the interface has been shut down. *)
  let get_n_grefs t n =
    let rec take seq = function
    | 0 -> []
    | n -> Lwt_dllist.take_l seq :: (take seq (n - 1)) in
    let rec loop after =
      let n' = Lwt_dllist.length t.rx_reqs in
      if n' >= n then return (take t.rx_reqs n)
      else begin
        Ring.Rpc.Back.ack_requests (to_netfront t)
          (fun slot ->
            let req = RX.Request.read slot in
            ignore(Lwt_dllist.add_r req t.rx_reqs)
          );
        if Lwt_dllist.length t.rx_reqs <> n'
        then loop after
        else OS.Activations.after t.channel after >>= loop
      end in
    (* We lock here so that we handle one frame at a time.
       Otherwise, we might divide the free pages among lots of
       waiters and deadlock. *)
    Lwt_mutex.with_lock t.get_free_mutex (fun () ->
      loop OS.Activations.program_start
    )

  let write t ~size fillf =
    Lwt.catch
      (fun () ->
         let pages_needed = max 1 @@ Io_page.round_to_page_size size / Io_page.page_size in
         (* Collect enough free pages from the client. *)
         get_n_grefs t pages_needed >>= fun reqs ->
         Lwt_mutex.with_lock t.write_mutex (fun () ->
             match reqs with
             | [ r ] ->
               let gnt = {Import.domid = t.frontend_id; ref = Gntref.of_int32 r.RX.Request.gref} in
               let mapping = Import.map_exn gnt ~writable:true in
               let dst = Import.Local_mapping.to_buf mapping |> Io_page.to_cstruct in
               Cstruct.memset dst 0;
               let len = fillf (Cstruct.sub dst 0 size) in
               Import.Local_mapping.unmap_exn mapping;
               if len > size then failwith "length exceeds total size" ;
               let slot =
                 let ring = to_netfront t in
                 Ring.Rpc.Back.(slot ring (next_res_id ring)) in
               let size = Ok len in
               let flags = Flags.empty in
               let resp = { RX.Response.id = r.RX.Request.id; offset = 0; flags; size } in
               RX.Response.write resp slot;
               Stats.tx t.stats (Int64.of_int len);
               return ()
             | reqs ->
               let rec fill_reqs ~src ~is_first = function
                 | r :: rs ->
                   let gnt = {Import.domid = t.frontend_id; ref = Gntref.of_int32 r.RX.Request.gref} in
                   let mapping = Import.map_exn gnt ~writable:true in
                   let dst = Import.Local_mapping.to_buf mapping |> Io_page.to_cstruct in
                   let len, src = Cstruct.fillv ~src ~dst in
                   Import.Local_mapping.unmap_exn mapping;
                   let slot =
                     let ring = to_netfront t in
                     Ring.Rpc.Back.(slot ring (next_res_id ring)) in
                   let size = Ok (if is_first then size else len) in
                   let flags = if rs = [] then Flags.empty else Flags.more_data in
                   let resp = { RX.Response.id = r.RX.Request.id; offset = 0; flags; size } in
                   RX.Response.write resp slot;
                   fill_reqs ~src ~is_first:false rs
                 | [] when Cstruct.lenv src = 0 -> ()
                 | [] -> failwith "BUG: not enough pages for data!" in
               (* TODO: find a smarter way to not need to copy around *)
               let data = Cstruct.create size in
               let len = fillf data in
               if len > size then failwith "length exceeds total size" ;
               let src = Cstruct.sub data 0 len in
               fill_reqs ~src:[src] ~is_first:true reqs;
               Stats.tx t.stats (Int64.of_int len);
               return ()
           ) >|= fun () -> Ok (
           if Ring.Rpc.Back.push_responses_and_check_notify (to_netfront t)
           then OS.Eventchn.notify h t.channel)
      )
      (function
        | Netback_shutdown -> Lwt.return (Error `Disconnected)
        | ex -> Lwt.fail ex
      )

  let get_stats_counters t = t.stats
  let reset_stats_counters t = Stats.reset t.stats

  let frontend_mac t = t.frontend_mac
  let mac t = t.mac
  let mtu t = t.mtu

  let disconnect _t = failwith "TODO: disconnect"
end
