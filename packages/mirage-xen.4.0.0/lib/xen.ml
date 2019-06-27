(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2012-2014 Citrix Inc
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

external virt_to_mfn : nativeint -> nativeint = "stub_virt_to_mfn"

open Lwt

module Gntref = struct
  type t = int
  (* Possibly this should be [Int32.t], like in vchan. But it's really a table index, so it won't overflow. *)

  let of_string x =
    match Int32.of_string ("0u" ^ x) with
    | i32 when i32 >= 0l -> Ok (Int32.to_int i32)
    | i32 -> Ok (Int32.to_int i32 + 1 lsl 32)   (* Note: this will only work on 64-bit platforms *)
    | exception _ ->
      Error (`Msg (Printf.sprintf "Invalid grant ref: %S is not a valid 32-bit unsigned integer" x))

  let to_string t = Printf.sprintf "%u" t
  let pp f t = Format.fprintf f "%u" t

  let to_int32 = Int32.of_int
  let of_int32 = Int32.to_int
end

type domid = int

let console = 0 (* public/grant_table.h:GNTTAB_RESERVED_CONSOLE *)
let xenstore = 1 (* public/grant_table.h:GNTTAB_RESERVED_XENSTORE *)

let ten_seconds_in_ns = 10000000000L

type grant_handle (* handle to a mapped grant *)

let src = Logs.Src.create "gnt" ~doc:"Xen memory grants"
module Log = (val Logs.src_log src : Logs.LOG)

module Import = struct
  type t = {
    domid: domid;
    ref: Gntref.t;
  }

  module Local_mapping = struct
    type t = {
      hs : grant_handle list;
      pages: Io_page.t;
    }

    let make hs pages = { hs; pages }

    let to_buf t = t.pages

    external unmap_exn : unit -> grant_handle -> unit = "stub_gnttab_unmap"
    let unmap_exn mapping = List.iter (unmap_exn ()) mapping.hs

    let unmap mapping =
      try Ok (unmap_exn mapping)
      with ex -> Error (`Msg (Printexc.to_string ex))
  end

  external map_fresh_exn: unit -> Gntref.t -> domid -> bool -> (grant_handle * Io_page.t) = "stub_gnttab_map_fresh"

  let map_exn grant ~writable =
    let h, page = map_fresh_exn () grant.ref grant.domid writable in
    Local_mapping.make [h] page

  let map grant ~writable = try Ok (map_exn grant ~writable) with ex -> Error (`Msg (Printexc.to_string ex))

  (* We must use a special mapv function to ensure the memory is mapped contiguously. *)
  external mapv_batched_exn: unit -> int array -> bool -> (grant_handle * Io_page.t) = "stub_gnttab_mapv_batched"

  let mapv_exn grants ~writable =
    let count = List.length grants in
    let grant_array = Array.make (count * 2) 0 in
    List.iteri (fun i g ->
        grant_array.(i * 2 + 0) <- g.domid;
        grant_array.(i * 2 + 1) <- g.ref;
      ) grants;
    let h, page = mapv_batched_exn () grant_array writable in
    Local_mapping.make [h] page

  let mapv gs ~writable = try Ok (mapv_exn gs ~writable) with ex -> Error (`Msg (Printexc.to_string ex))

  let with_mapping grant ~writable fn =
    match map grant ~writable with
    | Error _ as e -> Lwt.return e
    | Ok mapping ->
      Lwt.finalize
        (fun () -> fn mapping >|= fun x -> Ok x)
        (fun () -> Local_mapping.unmap_exn mapping; Lwt.return_unit)
end

module Export = struct
  type t = {
    mutable refs: Gntref.t list;
    mapping: Io_page.t;
  }

  let refs t = t.refs
  let mapping t = t.mapping

  let free_list : Gntref.t Queue.t = Queue.create ()
  let free_list_waiters = Lwt_dllist.create ()

  let count_gntref = MProf.Counter.make ~name:"gntref"

  let put_no_count r =
    Queue.push r free_list;
    match Lwt_dllist.take_opt_l free_list_waiters with
    | None -> ()
    | Some u -> Lwt.wakeup u ()

  let put r =
    MProf.Counter.increase count_gntref (-1);
    put_no_count r

  let num_free_grants () = Queue.length free_list

  let rec get () =
    match Queue.is_empty free_list with
    | true ->
      let th, u = MProf.Trace.named_task "Wait for free gnt" in
      let node = Lwt_dllist.add_r u free_list_waiters  in
      Lwt.on_cancel th (fun () -> Lwt_dllist.remove node);
      th >>= fun () -> get ()
    | false ->
      MProf.Counter.increase count_gntref (1);
      return (Queue.pop free_list)

  let get_n num =
    let rec gen_gnts num acc =
      match num with
      | 0 -> return acc
      | n ->
        begin
          get ()
          >>= fun gnt ->
          gen_gnts (n-1) (gnt :: acc)
        end
    in gen_gnts num []

  let get_nonblock () =
    try Some (Queue.pop free_list) with Queue.Empty -> None

  let get_n_nonblock num =
    let rec aux acc num = match num with
      | 0 -> List.rev acc
      | n ->
        (match get_nonblock () with
         | Some p -> aux (p::acc) (n-1)
         (* If we can't have enough, we push them back in the queue. *)
         | None -> List.iter (fun gntref -> Queue.push gntref free_list) acc; [])
    in aux [] num

  let with_ref f =
    get ()
    >>= fun gnt ->
    Lwt.finalize
      (fun () -> f gnt)
      (fun () -> Lwt.return (put gnt))

  let with_refs n f =
    get_n n
    >>= fun gnts ->
    Lwt.finalize
      (fun () -> f gnts)
      (fun () -> Lwt.return (List.iter put gnts))

  external nr_entries : unit -> int = "stub_gnttab_nr_entries"

  (* Any page that another domain can access MUST be in this array.
     Otherwise, it could get GC'd and reused for something else. *)
  let exports : Io_page.t option array = Array.make (nr_entries ()) None

  external grant_access : Gntref.t -> Io_page.t -> int -> bool -> unit = "stub_gntshr_grant_access"

  let grant_access ~domid ~writable gntref page =
    MProf.Trace.label "Gntshr.grant_access";
    if exports.(gntref) <> None then
      Fmt.invalid_arg "Grant %a is already in use!" Gntref.pp gntref;
    exports.(gntref) <- Some page;
    try
      grant_access gntref page domid writable
    with ex ->
      exports.(gntref) <- None;
      raise ex

  (* true if access has been ended, false if page is still being used (access has not changed) *)
  external try_end_access : Gntref.t -> bool = "stub_gntshr_try_end_access"

  let try_end_access ~release_ref g =
    MProf.Trace.label "Gntshr.end_access";
    if try_end_access g then (
      exports.(g) <- None;
      if release_ref then put g;
      Ok ()
    ) else (
      Log.info (fun f -> f "Attempt to release grant %d, which is still mapped by the remote domain." g);
      Error `Busy
    )

  let rec try_unshare ~release_refs t =
    match t.refs with
    | [] -> Ok ()
    | g :: gs ->
      match try_end_access ~release_ref:release_refs g with
      | Error _ as e -> e
      | Ok () ->
        t.refs <- gs;
        try_unshare ~release_refs t

  let rec end_access ~release_ref g =
    match try_end_access ~release_ref g with
    | Ok () -> Lwt.return_unit
    | Error `Busy ->
      Time.sleep_ns ten_seconds_in_ns >>= fun () ->
      end_access ~release_ref g

  let rec unshare ~release_refs t =
    match try_unshare ~release_refs t with
    | Ok () -> Lwt.return_unit
    | Error `Busy ->
      Time.sleep_ns ten_seconds_in_ns >>= fun () ->
      unshare ~release_refs t

  let share_pages ~domid ~count ~writable =
    (* First allocate a list of n pages. *)
    let block = Io_page.get count in
    let pages = Io_page.to_pages block in
    match get_n_nonblock count with
    | [] -> Error `Grant_table_full
    | gntrefs ->
      List.iter2 (grant_access ~domid ~writable) gntrefs pages;
      Ok { refs = gntrefs; mapping = block }

  let share_pages_exn ~domid ~count ~writable =
    match share_pages ~domid ~count ~writable with
    | Ok t -> t
    | Error `Grant_table_full -> failwith "Grant table full"

  let with_grant ~domid ~writable gnt page fn =
    grant_access ~domid ~writable gnt page;
    Lwt.finalize fn
      (fun () -> end_access ~release_ref:false gnt)

  let with_grants ~domid ~writable gnts pages fn =
    List.iter2 (grant_access ~domid ~writable) gnts pages;
    Lwt.finalize fn
      (fun () -> Lwt_list.iter_s (end_access ~release_ref:false) gnts)

  external nr_entries : unit -> int = "stub_gnttab_nr_entries"
  external nr_reserved : unit -> int = "stub_gnttab_reserved"

  let () =
    for i = nr_reserved () to nr_entries () - 1 do
      put_no_count i
    done
end
