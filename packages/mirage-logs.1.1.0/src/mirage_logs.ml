(* Copyright (C) 2016, Thomas Leonard <thomas.leonard@unikernel.com>
   See the README file for details. *)

type threshold_config = Logs.src -> Logs.level

let buf = Buffer.create 200
let log_fmt = Format.formatter_of_buffer buf

let string_of_level =
  let open! Logs in function
    | App -> "APP"
    | Error -> "ERR"
    | Warning -> "WRN"
    | Info -> "INF"
    | Debug -> "DBG"

module Make (C : Mirage_clock.PCLOCK) = struct

  type ring_entry =
    | Unused
    | Entry of Ptime.t * int option * string

  type ring = {
    entries : ring_entry array;
    mutable next : int;
  }

  type t = {
    reporter: Logs.reporter;
    ring    : ring option;
    ch      : out_channel;
    mutable old_hook: (exn -> unit) option;
  }

  let fmt_timestamp (posix_time, tz) =
    let printer = Ptime.pp_human ?tz_offset_s:tz () in
    Format.asprintf "%a" printer posix_time

  let pp_tags f tags =
    let pp tag () =
      let Logs.Tag.V (def, value) = tag in
      Format.fprintf f " %s=%a"
        (Logs.Tag.name def)
        (Logs.Tag.printer def)
        value;
      () in
    Logs.Tag.fold pp tags ()

  let ring_buffer size =
    let entries = Array.make size Unused in
    {entries; next = 0}

  let log_to_ring time tz msg = function
    | None -> ()
    | Some ring ->
      let i = ring.next in
      ring.entries.(i) <- Entry (time, tz, msg);
      ring.next <-
        if i = Array.length ring.entries - 1 then 0
        else i + 1

  let dump_ring t ch =
    match t.ring with
    | None -> ()
    | Some ring ->
      Printf.fprintf ch "--- Dumping log ring buffer ---\n";
      let first = ring.next in
      let rec dump_from i =
        begin match ring.entries.(i) with
          | Unused -> ()
          | Entry (posix_time, tz, msg) ->
            Printf.fprintf ch "%s: %s\n%!" (fmt_timestamp (posix_time, tz)) msg;
            ring.entries.(i) <- Unused end;
        let next = i + 1 in
        let next = if next = Array.length ring.entries then 0 else next in
        if next <> first then dump_from next in
      dump_from first;
      Printf.fprintf ch "--- End dump ---\n%!"

  let all_debug _ = Logs.Debug

  let create ?(ch=stderr) ?ring_size ?(console_threshold = all_debug) clock =
    let ring =
      match ring_size with
      | None -> None
      | Some size -> Some (ring_buffer size) in
    let report src level ~over k msgf =
      let tz = C.current_tz_offset_s clock in
      let posix_time = Ptime.v @@ C.now_d_ps clock in
      let lvl = string_of_level level in
      msgf @@ fun ?header ?(tags=Logs.Tag.empty) fmt ->
      let k _ =
        if not (Logs.Tag.is_empty tags) then
          Format.fprintf log_fmt ":%a" pp_tags tags;
        Format.pp_print_flush log_fmt ();
        let msg = Buffer.contents buf in
        Buffer.clear buf;
        if level <= console_threshold src then
          Printf.fprintf ch "%s: %s\n%!" (fmt_timestamp (posix_time, tz)) msg;
        MProf.Trace.label msg;
        log_to_ring posix_time tz msg ring;
        over ();
        k () in
      let src = Logs.Src.name src in
      match header with
      | None -> Format.kfprintf k log_fmt ("%s [%s] " ^^ fmt) lvl src
      | Some h -> Format.kfprintf k log_fmt ("%s [%s:%s] " ^^ fmt) lvl src h
    in
    let reporter = { Logs.report } in
    let old_hook = None in
    { reporter; ring; ch; old_hook }

  let reporter t = t.reporter

  let set_reporter t =
    Logs.set_reporter t.reporter;
    match t.ring with
    | None -> ()
    | Some _ ->
      let old_hook = !Lwt.async_exception_hook in
      t.old_hook <- Some old_hook;
      Lwt.async_exception_hook := (fun ex ->
          dump_ring t t.ch;
          old_hook ex
        )

  let unset_reporter t =
    match t.old_hook with
    | None   -> ()
    | Some h ->
      Lwt.async_exception_hook := h;
      t.old_hook <- None

  let run t fn =
    Logs.set_reporter t.reporter;
    match t.ring with
    | None -> fn ()
    | Some _ ->
      let old_hook = !Lwt.async_exception_hook in
      Lwt.async_exception_hook := (fun ex ->
          dump_ring t t.ch;
          old_hook ex
        );
      Lwt.finalize
        (fun () -> Lwt.catch fn (fun ex -> dump_ring t t.ch; Lwt.fail ex))
        (fun () -> Lwt.async_exception_hook := old_hook; Lwt.return ())
end
