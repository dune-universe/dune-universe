(* Copyright (C) 2014, Thomas Leonard *)

open Bigarray
type log_buffer = (char, int8_unsigned_elt, c_layout) Array1.t

external get_monotonic_time : unit -> int64 = "caml_get_monotonic_time"

module Export = Os_xen.Xen.Export
module Xs = Os_xen.Xs

let timestamper buf off =
  EndianBigstring.LittleEndian.set_int64 buf off (get_monotonic_time ())

let make_shared_buffer ~size =
  let open Io_page in
  let n_pages = round_to_page_size size / round_to_page_size 1 in
  get n_pages

let share_with ~domid buffer =
  let pages = Io_page.to_pages buffer in
  let open Lwt in

  let refs =
    pages |> Lwt_list.map_s (fun page ->
      Export.get () >>= fun gnt ->
      Export.grant_access ~domid ~writable:false gnt page;
      return gnt
    ) in
  refs >>= fun refs ->

  let ring_ref = refs
    |> List.map Os_xen.Xen.Gntref.to_string
    |> String.concat "," in

  Xs.make ()
  >>= fun c ->
  Xs.(immediate c (fun h -> read h "domid")) >>= fun my_domid ->
  Xs.(immediate c (fun h -> getdomainpath h (int_of_string my_domid))) >>= fun domainpath ->
  let xs_path = Printf.sprintf "%s/data/mprof" domainpath in
  Xs.(transaction c (fun h -> write h (xs_path ^ "/ring-ref") ring_ref)) >>= fun () ->
  return ()
