(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rfc5424
module R = Record.Make(Capnp.BytesMessage)
open R.Builder

let iter_non_empty_string ~f = function
  | "" -> ()
  | s -> f s

let rfc5424_section = "rfc5424_section"

let build_pairs ~tydefs section tags =
  let set_value : type a. a Tag.typ -> Pair.Value.t -> a -> unit =
    fun typ v tagv ->
      match typ with
      | Tag.String -> Pair.Value.string_set v tagv
      | Tag.Bool -> Pair.Value.bool_set v tagv
      | Tag.Float -> Pair.Value.f64_set v tagv
      | Tag.I64 -> Pair.Value.i64_set v tagv
      | Tag.U64 -> Pair.Value.u64_set v tagv
      | Tag.U -> Pair.Value.null_set v in
  let create_pair :
    type a. Logs.Tag.set -> a Tag.typ -> Tag.tydef -> Logs.Tag.set * Pair.t option
    = fun tags typ tydef ->
      match Tag.find typ tydef tags with
      | None
      | Some (_, None) -> tags, None
      | Some (d, Some tagv) ->
        let p = Pair.init_root () in
        Pair.key_set p (Logs.Tag.name d) ;
        let v = Pair.value_init p in
        set_value typ v tagv ;
        Logs.Tag.rem d tags, Some p
  in
  let build_pairs tags pairs tydefs =
    List.fold_left begin fun
      ((tags, pairs) as a)
      (Tag.Dyn (t, _) as tydef) ->
      match create_pair tags t tydef with
      | _, None -> a
      | tags, Some p -> tags, p :: pairs
    end (tags, pairs) tydefs in
  let section_pair =
    let p = Pair.init_root () in
    Pair.key_set p rfc5424_section ;
    let v = Pair.value_init p in
    Pair.Value.string_set v section ;
    p in
  let tags, pairs = build_pairs tags [section_pair] tydefs in
  Logs.Tag.fold begin fun (Logs.Tag.V (d, t)) pairs ->
    let p = Pair.init_root () in
    let v = Pair.value_init p in
    Pair.key_set p (Logs.Tag.name d) ;
    Pair.Value.string_set v (Format.asprintf "%a" (Logs.Tag.printer d) t) ;
    p :: pairs
  end tags pairs

let capnp_of_syslog
    ({ header = { facility; severity; version = _; ts;
                  hostname; app_name; procid; msgid }; structured_data; msg } : Rfc5424.t) =
  let r = Record.init_root () in
  Record.facility_set_exn r (Syslog_message.int_of_facility facility) ;
  Record.severity_set_exn r (Syslog_message.int_of_severity severity) ;
  Record.ts_set r (Ptime.to_float_s ts) ;
  iter_non_empty_string hostname ~f:(Record.hostname_set r) ;
  iter_non_empty_string app_name ~f:(Record.appname_set r) ;
  iter_non_empty_string procid ~f:(Record.procid_set r) ;
  iter_non_empty_string msgid ~f:(Record.msgid_set r) ;
  iter_non_empty_string msg ~f:(Record.msg_set r) ; (* OVH needs this *)
  iter_non_empty_string msg ~f:(Record.full_msg_set r) ;
  let pairs = List.fold_left begin fun a {section ; defs ; tags } ->
      List.rev_append (build_pairs section ~tydefs:defs tags) a
    end [] structured_data in
  let _ = Record.pairs_set_list r pairs in
  r

let pp_print_int64 ppf i = Format.fprintf ppf "%Ld" i

let string_option_of_string = function
  | "" -> None
  | s -> Some s

module SM = Map.Make(String)

let syslog_of_capnp r =
  let facility =
    Syslog_message.facility_of_int (Record.facility_get r) in
  let severity =
    Syslog_message.severity_of_int (Record.severity_get r) in
  let hostname = string_option_of_string @@ Record.hostname_get r in
  let app_name = string_option_of_string @@ Record.appname_get r in
  let procid = string_option_of_string @@ Record.procid_get r in
  let msgid = string_option_of_string @@ Record.msgid_get r in
  let msg = string_option_of_string @@ Record.full_msg_get r in
  let ts =
    match Ptime.of_float_s (Record.ts_get r) with
    | None -> Ptime.epoch
    | Some ts -> ts in
  let _, tags =
    List.fold_left begin fun (c, m) p ->
      let k = Pair.key_get p in
      let v = Pair.value_get p in
      let update td d v m =
        SM.update c begin function
          | None ->
            Some (Tag.TS.singleton td, Logs.Tag.(add d v empty))
          | Some (defs, tags) ->
            Some (Tag.TS.add td defs, Logs.Tag.add d v tags)
        end m in
      match Pair.Value.get v with
      | String s when k = rfc5424_section -> s, m
      | String s ->
        let d = Logs.Tag.def k Format.pp_print_string in
        let td = Tag.string d in
        c, update td d s m
      | Bool b ->
        let d = Logs.Tag.def k Format.pp_print_bool in
        let td = Tag.bool d in
        c, update td d b m
      | F64 f ->
        let d = Logs.Tag.def k Format.pp_print_float in
        let td = Tag.float d in
        c, update td d f m
      | I64 i ->
        let d = Logs.Tag.def k pp_print_int64 in
        let td = Tag.i64 d in
        c, update td d i m
      | U64 i ->
        let d = Logs.Tag.def k Uint64.printer in
        let td = Tag.u64 d in
        c, update td d i m
      | Null ->
        let d = Logs.Tag.def k Format.pp_print_space in
        let td = Tag.u d in
        c, update td d () m
      | Undefined _ -> c, m
    end ("", SM.empty) (Record.pairs_get_list r) in
  let structured_data = SM.fold begin fun section (defs, tags) a ->
      { section ; defs = Tag.TS.elements defs ; tags } :: a
    end tags [] in
  Rfc5424.create
    ?facility ?severity ?hostname
    ?app_name ?procid ?msgid ?msg ~structured_data ~ts ()

let pp ~compression () ppf t =
  let r = capnp_of_syslog t in
  let m = R.Builder.Record.to_message r in
  Format.pp_print_string ppf (Capnp.Codecs.serialize ~compression m)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

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
