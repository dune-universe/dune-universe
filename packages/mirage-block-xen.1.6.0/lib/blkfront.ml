(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Citrix Systems Inc
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
open Printf
open Mirage_block
open Blkproto
open Gnt
open OS

let src =
  let src = Logs.Src.create "blkfront" ~doc:"Mirage Xen blkfront" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

type 'a io = 'a Lwt.t

type page_aligned_buffer = Cstruct.t

type transport = {
  backend_id: int;
  backend: string;
  ring: (Res.t,int64) Ring.Rpc.Front.t;
  client: (Res.t,int64) Lwt_ring.Front.t;
  gnts: Gnt.gntref list;
  evtchn: Eventchn.t;
  max_indirect_segments: int;
  info: info;
}

type t = {
  vdev: int;
  mutable t: transport
}

type id = string
exception IO_error of string

(** slot number (e.g. "51712") to device threads *)
let devices : (id, t Lwt.t) Hashtbl.t = Hashtbl.create 16

let h = Eventchn.init ()

(* Allocate a ring, given the vdev and backend domid *)
let alloc ~order (num,domid) =
  let name = sprintf "Blkif.%d" num in
  let idx_size = Req.Proto_64.total_size in (* bigger than res *)
  let buf = Io_page.get_order order in

  let pages = Io_page.to_pages buf in
  let open Lwt.Infix in
  Gntshr.get_n (List.length pages)
  >>= fun gnts ->
  List.iter (fun (gnt, page) ->
      Gntshr.grant_access ~domid ~writable:true gnt page)
    (List.combine gnts pages);

  let sring = Ring.Rpc.of_buf ~buf:(Io_page.to_cstruct buf) ~idx_size ~name in
  Log.info (fun f -> f "Blkfront.alloc %s" (Ring.Rpc.to_summary_string sring));
  let fring = Ring.Rpc.Front.init ~sring in
  let client = Lwt_ring.Front.init Int64.to_string fring in
  return (gnts, fring, client)

(* Thread to poll for responses and activate wakeners *)
let poll t =
  let rec loop from =
    let open Lwt.Infix in
    Activations.after t.evtchn from
    >>= fun next ->
    let () = Lwt_ring.Front.poll t.client (Res.read_response) in
    loop next in
  loop Activations.program_start

(* Given a VBD ID and a backend domid, construct a blkfront record.
   Note the logic in Block.connect below to only call this once per device *)
let plug (id:id) =
  let open Lwt.Infix in
  ( try return (int_of_string id)
    with _ -> fail (Failure "invalid vdev") )
  >>= fun vdev ->
  Log.info (fun f -> f "Blkfront.plug id=%s" id);
  let node = sprintf "device/vbd/%d/%s" vdev in

  Xs.make ()
  >>= fun xs ->
  Xs.(immediate xs (fun h -> read h (node "backend-id")))
  >>= fun backend_id ->
  ( Lwt.catch
    (fun () -> return (int_of_string backend_id))
    (fun _ -> fail (Failure "invalid backend_id")) )
  >>= fun backend_id ->
  Xs.(immediate xs (fun h -> read h (node "backend")))
  >>= fun backend ->

  let backend_read fn default k =
    let backend = sprintf "%s/%s" backend in
    Lwt.catch
      (fun () ->
        Xs.(immediate xs (fun h -> read h (backend k)))
        >>= fun s ->
        return (fn s)
      ) (fun exn -> return default) in

  (* The backend can advertise a multi-page ring: *)
  backend_read int_of_string 0 "max-ring-page-order"
  >>= fun backend_max_ring_page_order ->
  if backend_max_ring_page_order = 0
  then Log.info (fun f -> f "Blkback can only use a single-page ring")
  else Log.info (fun f -> f "Blkback advertises multi-page ring (size 2 ** %d pages)" backend_max_ring_page_order);

  let our_max_ring_page_order = 2 in (* 4 pages *)
  let ring_page_order = min our_max_ring_page_order backend_max_ring_page_order in
  Log.info (fun f -> f "Negotiated a %s"
    (if ring_page_order = 0 then
       "single-page ring" else
       sprintf "multi-page ring (size 2 ** %d pages)" ring_page_order));

  alloc ~order:ring_page_order (vdev,backend_id)
  >>= fun (gnts, ring, client) ->
  let evtchn = Eventchn.bind_unbound_port h backend_id in
  let port = Eventchn.to_int evtchn in
  let ring_info =
    (* The new protocol writes (ring-refN = G) where N=0,1,2 *)
    let rfs = snd(List.fold_left (fun (i, acc) g ->
        i + 1, ((sprintf "ring-ref%d" i, string_of_int g) :: acc)
      ) (0, []) gnts) in
    if ring_page_order = 0
    then [ "ring-ref", string_of_int (List.hd gnts) ] (* backwards compat *)
    else [ "ring-page-order", string_of_int ring_page_order ] @ rfs in
  let info = [
    "event-channel", string_of_int port;
    "protocol", "x86_64-abi";
    "state", Device_state.(to_string Connected)
  ] @ ring_info in
  Xs.(transaction xs (fun h ->
      Lwt_list.iter_s (fun (k, v) -> write h (node k) v) info
    ))
  >>= fun () ->
  Xs.(wait xs (fun h ->
      read h (sprintf "%s/state" backend)
      >>= fun state ->
      if Device_state.(of_string state = Connected) then return () else fail Xs_protocol.Eagain
    ))
  >>= fun () ->
  (* Read backend info *)
  backend_read int_of_string 0 FeatureIndirect._max_indirect_segments
  >>= fun max_indirect_segments ->
  (* Limit to one page for now *)
  let max_indirect_segments = min max_indirect_segments Req.Proto_64.segments_per_indirect_page in
  ( backend_read (Device_state.of_string) Device_state.Unknown "state"
    >>= fun state ->
    Log.debug (fun f -> f "state=%s" (Device_state.prettyprint state));
    backend_read Int64.of_string (-1L) "sectors"
    >>= fun size_sectors ->
    backend_read int_of_string 0 "sector-size"
    >>= fun sector_size ->
    backend_read (fun x -> x = "w") false "mode"
    >>= fun read_write ->
    return { sector_size; size_sectors; read_write } )
  >>= fun info ->

  Log.info (fun f -> f "Blkfront info: sector_size=%u sectors=%Lu max_indirect_segments=%d"
    info.sector_size info.size_sectors max_indirect_segments);
  Eventchn.unmask h evtchn;
  let t = { backend_id; backend; ring; client; gnts; evtchn; max_indirect_segments; info } in
  (* Start the background poll thread *)
  let _ = poll t in
  return t

(* Unplug shouldn't block, although the Xen one might need to due
   to Xenstore? XXX *)
let unplug id =
  Log.err (fun f -> f "Blkfront.unplug %s: not implemented yet" id)

(** Return a list of valid VBDs *)
let enumerate () =
  let open Lwt.Infix in
  Xs.make ()
  >>= fun xs ->
  Lwt.catch
    (fun () ->
      Xs.(immediate xs (fun h -> directory h "device/vbd"))
    ) (function
    | Xs_protocol.Enoent _ ->
      return []
    | e ->
      Log.err (fun f -> f "Blkfront.enumerate caught exception: %s" (Printexc.to_string e));
      return []
    )

let deprecated_prefixes = [ "tapdisk"; "tap2"; "aio"; "ioemu"; "file"; "phy" ]

let strip_prefixes x =
  Stringext.split x ~on:':'
  |> List.fold_left (fun acc x -> match acc with
      | [] ->
        if List.mem x deprecated_prefixes
        then []
        else [ x ]
      | xs -> x :: xs
    ) []
  |> List.rev
  |> String.concat ":"

(** Return a list of pairs [backend-params-key, frontend-id].
    This is only intended to be a heuristic for 'connect' below. *)
let params_to_frontend_ids ids =
  let open Lwt.Infix in
  Xs.make ()
  >>= fun xs ->
  Lwt_list.fold_left_s (fun list id ->
    Lwt.catch
      (fun () ->
        Xs.(immediate xs (fun h -> read h (Printf.sprintf "device/vbd/%s/backend" id)))
        >>= fun backend ->
        Xs.(immediate xs (fun h -> read h (Printf.sprintf "%s/params" backend)))
        >>= fun params ->
        (* According to http://xenbits.xen.org/docs/4.6-testing/misc/xl-disk-configuration.txt
           the params keys can have deprecated prefixes which should be stripped and ignored. *)
        return ((strip_prefixes params, id) :: list)
      ) (function
        | Xs_protocol.Enoent path ->
          Log.warn (fun f -> f "Blkfront.params_to_frontend_ids: missing %s" path);
          return list
        | e ->
          Log.warn (fun f -> f "Blkfront.params_to_frontend_ids caught exception: %s" (Printexc.to_string e));
          return list
        )
    ) [] ids

(** Create a Direct request if we have 11 or fewer requests, else an Indirect request. *)
let with_segs t ~start_offset ~end_offset rs fn =
  let len = Array.length rs in
  let segs = Array.mapi (fun i rf ->
      let first_sector = match i with
        | 0 -> start_offset
        | _ -> 0 in
      let last_sector = match i with
        | n when n == len-1 -> end_offset
        | _ -> 7 in
      let gref = Int32.of_int rf in
      { Req.gref; first_sector; last_sector }
    ) rs in
  if len <= 11 then (
    fn (Req.Direct segs)
  ) else (
    (* The protocol allows up to 8 pages, but at 512 entries each it's unlikely
     * we'll want more than one. The Linux blkback limits us to 256 by default
     * anyway. *)
    let indirect_page = Io_page.get 1 in
    Req.Proto_64.write_segments segs (Io_page.to_cstruct indirect_page);
    Gntshr.with_ref (fun indirect_ref ->
      Gntshr.with_grant ~domid:t.t.backend_id ~writable:false indirect_ref indirect_page (fun () ->
        fn (Req.Indirect [| Int32.of_int indirect_ref |])
      )
    )
  )

(** [single_request_into op t start_sector start_offset end_offset pages]
    issues a single request [op], starting at [start_sector] and using
    the memory [pages] as either the target of data (if [op] is Read) or the
    source of data (if [op] is Write). If the sector size is less than a page
    then [start_offset] and [end_offset] can be used to start and finish the
    data on sub-page sector boundaries in the first and last pages. *)
let single_request_into op t start_sector ?(start_offset=0) ?(end_offset=7) pages =
  let len = List.length pages in
  let rec retry () =
    Lwt.catch
      (fun () ->
      Gntshr.with_refs len
        (fun rs ->
           Gntshr.with_grants ~domid:t.t.backend_id ~writable:(op = Req.Read) rs pages
             (fun () ->
                let rs = Array.of_list rs in
                let nr_segs = Array.length rs in
                with_segs t ~start_offset ~end_offset rs (fun segs ->
                  let id = Int64.of_int rs.(0) in
                  let sector = Int64.(add start_sector (of_int start_offset)) in
                  let req = Req.({ op=Some op; handle=t.vdev; id; sector; nr_segs; segs }) in
                  let open Lwt.Infix in
                  Lwt_ring.Front.push_request_and_wait t.t.client
                      (fun () -> Eventchn.notify h t.t.evtchn)
                      (Req.Proto_64.write_request req)
                  >>= fun res ->
                  let open Res in
                  match res.st with
                  | Some Error -> fail (IO_error "read")
                  | Some Not_supported -> fail (IO_error "unsupported")
                  | None -> fail (IO_error "unknown error")
                  | Some OK -> return ()
                )
             )
        )
    ) (function
      | Lwt_ring.Shutdown -> retry ()
      | exn -> fail exn) in
  retry ()

(* THIS FUNCTION IS DEPRECATED. Use 'write' instead.

   Write a single page to disk.
   Offset is in bytes, which must be sector-aligned
   Page must be an Io_page *)
let rec write_page t offset page =
  let sector = Int64.(div offset (of_int t.t.info.sector_size)) in
  if not t.t.info.read_write
  then fail (IO_error "read-only")
  else single_request_into Req.Write t sector [ page ]


(* THIS FUNCTION IS DEPRECATED. Use 'read' instead.

   Reads [num_sectors] starting at [sector], returning a stream of Io_page.ts *)
let read_512 t sector num_sectors =
  let module Single_request = struct
    (** A large request must be broken down into a series of smaller page-aligned requests: *)
    type t = {
      start_sector: int64; (* page-aligned sector to start reading from *)
      start_offset: int;   (* sector offset into the page of our data *)
      end_sector: int64;   (* last page-aligned sector to read *)
      end_offset: int;     (* sector offset into the page of our data *)
    }

    (** Number of pages required to issue this request *)
    let npages_of t = Int64.(to_int (div (sub t.end_sector t.start_sector) 8L))

    let to_string t =
      sprintf "(%Lu, %u) -> (%Lu, %u)" t.start_sector t.start_offset t.end_sector t.end_offset

    (* Transforms a large read of [num_sectors] starting at [sector] into a Lwt_stream
       of single_requests, where each request will fit on the ring. *)
    let stream_of sector num_sectors =
      let from (sector, num_sectors) =
        assert (sector >= 0L);
        assert (num_sectors > 0L);
        (* Round down the starting sector in order to get a page aligned sector *)
        let start_sector = Int64.(mul 8L (div sector 8L)) in
        let start_offset = Int64.(to_int (sub sector start_sector)) in
        (* Round up the ending sector to the page boundary *)
        let end_sector = Int64.(mul 8L (div (add (add sector num_sectors) 7L) 8L)) in
        (* Calculate number of sectors needed *)
        let total_sectors_needed = Int64.(sub end_sector start_sector) in
        (* Maximum of 11 segments per request; 1 page (8 sectors) per segment so: *)
        let total_sectors_possible = min 88L total_sectors_needed in
        let possible_end_sector = Int64.add start_sector total_sectors_possible in
        let end_offset = min 7 (Int64.(to_int (sub 7L (sub possible_end_sector (add sector num_sectors))))) in

        let first = { start_sector; start_offset; end_sector = possible_end_sector; end_offset } in
        if total_sectors_possible < total_sectors_needed
        then
          let num_sectors = Int64.(sub num_sectors (sub total_sectors_possible (of_int start_offset))) in
          first, Some ((Int64.add start_sector total_sectors_possible), num_sectors)
        else
          first, None in
      let state = ref (Some (sector, num_sectors)) in
      Lwt_stream.from
        (fun () ->
           match !state with
           | None -> return None
           | Some x ->
             let item, state' = from x in
             state := state';
             return (Some item)
        )

    let list_of sector num_sectors =
      Lwt_stream.to_list (stream_of sector num_sectors)
  end in
  let requests = Single_request.stream_of sector num_sectors in
  let read_single_request t r =
    let open Single_request in
    let len = npages_of r in
    let pages = Io_page.(to_pages (get len)) in
    let open Lwt.Infix in
    single_request_into Req.Read t r.start_sector
        ~start_offset:r.start_offset ~end_offset:r.end_offset pages
    >>= fun () ->
    return (Lwt_stream.of_list
              (List.rev
                 (snd
                    (List.fold_left
                       (fun (i, acc) page ->
                          let start_offset = match i with
                            |0 -> r.start_offset * 512
                            |_ -> 0 in
                          let end_offset = match i with
                            |n when n = len-1 -> (r.end_offset + 1) * 512
                            |_ -> 4096 in
                          let bytes = end_offset - start_offset in
                          let subpage = Cstruct.sub (Io_page.to_cstruct page) start_offset bytes in
                          i + 1, subpage :: acc
                       ) (0, []) pages
                    )))) in
  Lwt_stream.(concat (map_s (read_single_request t) requests))

let resume t =
  let vdev = sprintf "%d" t.vdev in
  let open Lwt.Infix in
  plug vdev
  >>= fun transport ->
  let old_t = t.t in
  t.t <- transport;
  Lwt_ring.Front.shutdown old_t.client;
  return ()

let resume () =
  let devs = Hashtbl.fold (fun k v acc -> (k,v)::acc) devices [] in
  let open Lwt.Infix in
  Lwt_list.iter_p (fun (k,v) ->
    v >>= fun v ->
    resume v
  ) devs

let disconnect _id =
  Log.err (fun f -> f "Blkfront: disconnect not implement yet");
  return ()

type error = [ Mirage_block.error | `Exn of exn ]

let pp_error ppf = function
  | #Mirage_block.error as e -> Mirage_block.pp_error ppf e
  | `Exn e -> Fmt.exn ppf e

type write_error = [ Mirage_block.write_error | `Exn of exn ]

let pp_write_error ppf = function
  | #Mirage_block.write_error as e -> Mirage_block.pp_write_error ppf e
  | `Exn e -> Fmt.exn ppf e

(* [take xs n] returns [(taken, remaining)] where [taken] is as many
   elements of [xs] as possible, up to [n], and [remaining] is any
   that are left over. *)
let take xs n =
  let rec loop taken remaining n = match n, remaining with
    | 0, _
    | _, [] -> List.rev taken, remaining
    | n, x :: xs -> loop (x :: taken) xs (n - 1) in
  loop [] xs n

(* Upgrade sector_size to be at least a page to guarantee read/write
   is page-aligned as well as sector-aligned. 4k sector size disks
   are becoming more common, so we might as well be ready. *)
let minimum_sector_size = 4096

let get_sector_size t =
  max t.t.info.sector_size minimum_sector_size

let sector t x =
  if t.t.info.sector_size >= 4096
  then x
  else Int64.(div (mul x (of_int minimum_sector_size)) (of_int t.t.info.sector_size))

let get_info t =
  let sector_size = get_sector_size t in
  let size_sectors = Int64.(
      div t.t.info.size_sectors
        (of_int (sector_size / t.t.info.sector_size))) in
  let info = { t.t.info with sector_size; size_sectors } in
  return info

let rec multiple_requests_into op t start_sector = function
  | [] -> return ()
  | remaining ->
    let max_segments_per_request = max 11 t.t.max_indirect_segments in
    let pages, remaining = take remaining max_segments_per_request in
    let open Lwt.Infix in
    single_request_into op t start_sector pages
    >>= fun () ->
    let start_sector = Int64.(add start_sector (of_int (max_segments_per_request * 4096 / t.t.info.sector_size))) in
    multiple_requests_into op t start_sector remaining

let connect_already_locked id =
  let open Lwt.Infix in
  if Hashtbl.mem devices id then begin
    Hashtbl.find devices id
  end else begin
    enumerate ()
    >>= fun all ->
    params_to_frontend_ids all
    >>= fun list ->
    (* Apply a set of heuristics to locate the disk:
       if [id] is a xen virtual disk bus slot number (e.g. 51712) then use it
       if [id] is a "linux device string" (e.g. "xvda" or "/dev/xvda") then translate it
       if [id] is a unique backend "params" xenstore key then use it
    *)
    let choice =
      if List.mem id all then begin
        Log.info (fun f -> f "Blkfront.connect %s: interpreting %s as a xen virtual disk bus slot number" id id);
        Some id
      end else begin
        let id' =
          let prefix = "/dev/" in
          let prefix' = String.length prefix and id' = String.length id in
          let stripped =
            if prefix' <= id' && (String.sub id 0 prefix' = prefix)
            then String.sub id prefix' (id' - prefix') else id in
          try
            let device = Device_number.of_linux_device stripped in
            string_of_int (Device_number.to_xenstore_key device)
          with _ -> id in
        if List.mem id' all then begin
          Log.info (fun f -> f "Blkfront.connect %s: interpreting %s as a linux device string, translating to %s" id id id');
          Some id'
        end else begin
          match List.map snd (List.filter (fun (params, _) -> params = id) list), all with
          | [ id' ], _ ->
            Log.info (fun f -> f "Blkfront.connect %s: interpreting %s as a backend params key, translating to %s" id id id');
            Some id'
          | first :: rest, _ ->
            Log.err (fun f -> f "Blkfront.connect %s: name is ambiguous: it matches multiple backend params keys [ %s ]" id (String.concat "; " (first::rest)));
            None
          | _, _ ->
            Log.err (fun f -> f "Blkfront.connect %s: unable to match '%s' to any available devices [ %s ]\n" id id (String.concat "; " all));
            None
        end
      end in
    match choice with
    | Some id' when Hashtbl.mem devices id' ->
      let t = Hashtbl.find devices id' in
      Hashtbl.replace devices id t;
      t
    | Some id' ->
      Log.info (fun f -> f "Blkfront.connect %s -> %s" id id');
      let t, u = Lwt.task () in
      Hashtbl.replace devices id' t;
      Hashtbl.replace devices id t;
      (* id' is now in devices, so no-one will call plug in parallel with us *)
      plug id'
      >>= fun trans ->
      let dev = { vdev = int_of_string id'; t = trans } in
      Lwt.wakeup u dev;
      return dev
    | None ->
      Log.err (fun f -> f "Blkfront.connect %s: could not find device" id);
      fail_with (Printf.sprintf "device %s not found (available = [ %s ])"
                   id (String.concat ", " all))
  end

let connect_m = Lwt_mutex.create ()

(* For safety only allow one connect at a time, in case two threads attempt
   to connect the same device at the same time and both end up doing it.
   See #31 *)
let connect id =
  Lwt_mutex.with_lock connect_m
    (fun () ->
      connect_already_locked id
    )

let id t = string_of_int t.vdev

exception Buffer_not_exactly_one_page
let to_iopage x =
  if x.Cstruct.len <> 4096 then raise Buffer_not_exactly_one_page;
  Io_page.of_cstruct_exn x

let to_iopages x =
  try return (List.map to_iopage x)
  with e -> fail e

let read t start_sector pages =
  let open Lwt.Infix in
  to_iopages pages
  >>= fun pages ->
  Lwt.catch
    (fun () ->
      multiple_requests_into Req.Read t (sector t start_sector) pages
      >>= fun () ->
      return (Ok ())
    ) (fun e -> return (Error (`Exn e)))

let write t start_sector pages =
  let open Lwt.Infix in
  to_iopages pages
  >>= fun pages ->
  Lwt.catch
    (fun () ->
      multiple_requests_into Req.Write t (sector t start_sector) pages
      >>= fun () ->
      return (Ok ())
    ) (fun e -> return (Error (`Exn e)))

let _ =
  Log.debug (fun f -> f "Blkfront: add resume hook");
  Sched.add_resume_hook resume
