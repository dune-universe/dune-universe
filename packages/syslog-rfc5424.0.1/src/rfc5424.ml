(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Tag = struct
  open Logs.Tag

  type (_,_) eq = Eq : ('a,'a) eq

  type _ typ =
    | String : string typ
    | Bool : bool typ
    | Float : float typ
    | I64 : int64 typ
    | U64 : Uint64.t typ
    | U : unit typ

  type tydef = Dyn : 'a typ * 'a def -> tydef

  module TS = Set.Make(struct type t = tydef let compare = Stdlib.compare end)

  let string v = Dyn (String, v)
  let bool v = Dyn (Bool, v)
  let float v = Dyn (Float, v)
  let i64 v = Dyn (I64, v)
  let u64 v = Dyn (U64, v)
  let u v = Dyn (U, v)

  let eq_type : type a b. a typ -> b typ -> (a,b) eq option =
    fun a b -> match a, b with
      | String, String -> Some Eq
      | Bool, Bool -> Some Eq
      | Float, Float -> Some Eq
      | I64, I64 -> Some Eq
      | U64, U64 -> Some Eq
      | U, U -> Some Eq
      | _ -> None

  let def :
    type a. a typ -> tydef -> a def option = fun typ (Dyn (t, def)) ->
    match eq_type typ t with
    | None -> None
    | Some Eq -> Some def

  let add :
    type a. a typ -> tydef -> a -> set -> set = fun typ (Dyn (t, d)) v set ->
    match eq_type typ t with
    | None -> set
    | Some Eq -> add d v set

  let find :
    type a. a typ -> tydef -> Logs.Tag.set -> (a def * a option) option =
    fun a (Dyn(b,x)) set ->
    match eq_type a b with
    | None -> None
    | Some Eq ->
      match find x set with
      | None -> Some (x, None)
      | Some v -> Some (x, Some v)
end

type t = {
  header : header ;
  structured_data : sd_element list ;
  msg : [`Utf8 of string | `Ascii of string ] ;
}

and sd_element = {
  section: string ;
  defs : Tag.tydef list ;
  tags : Logs.Tag.set ;
}

and header = {
  facility : Syslog_message.facility ;
  severity : Syslog_message.severity ;
  version : int ;
  ts : Ptime.t ;
  tz_offset_s: int option ;
  hostname : string ;
  app_name : string ;
  procid : string ;
  msgid : string ;
}

let create_sd_element ?(defs=[]) ~section ~tags =
  { section ; defs ; tags }

let create
    ?(facility=Syslog_message.User_Level_Messages)
    ?(severity=Syslog_message.Notice)
    ?(ts=Ptime.min)
    ?(tz_offset_s)
    ?(hostname="") ?(app_name="") ?(procid="") ?(msgid="")
    ?(structured_data=[]) ?(msg=`Ascii "") () =
  let header = { facility ; severity ; version = 1 ; ts ;
                 tz_offset_s ; hostname ; app_name ; procid ; msgid } in
  { header ; structured_data ; msg }

let fcreate
    ?(facility=Syslog_message.User_Level_Messages)
    ?(severity=Syslog_message.Notice)
    ?(ts=Ptime.min)
    ?(tz_offset_s)
    ?(hostname="") ?(app_name="") ?(procid="") ?(msgid="")
    ?(structured_data=[]) () =
  Format.kasprintf begin fun msg ->
    let header = { facility ; severity ; version = 1 ; ts ;
                   tz_offset_s ; hostname ; app_name ; procid ; msgid } in
    { header ; structured_data ; msg = `Ascii msg }
  end

let equal_structured_data =
  let module SM = Map.Make(String) in
  let load m =
    List.fold_left begin fun a { section ; tags ; _ } ->
      SM.add section tags a
    end SM.empty m in
  fun tags tags' ->
    let tags = load tags in
    let tags' = load tags' in
    SM.equal begin fun a b ->
      String.equal
        (Format.asprintf "%a" Logs.Tag.pp_set a)
        (Format.asprintf "%a" Logs.Tag.pp_set b)
    end tags tags'

let equal t t' =
  t.header = t'.header &&
  t.msg = t'.msg &&
  equal_structured_data t.structured_data t'.structured_data

let pp_print_string_option ppf = function
  | "" -> Format.pp_print_char ppf '-'
  | s -> Format.pp_print_string ppf s

let pp_print_ts ppf (ts, tz_offset_s) =
  if Ptime.(equal ts min) then Format.pp_print_char ppf '-'
  else Ptime.pp_rfc3339 ?tz_offset_s ~frac_s:6 () ppf ts

let pp_print_header ppf { facility ; severity ; version ; ts ;
                          tz_offset_s ; hostname ; app_name ; procid ; msgid } =
  Format.fprintf ppf "<%d>%d %a %a %a %a %a"
    Syslog_message.(int_of_facility facility * 8 + int_of_severity severity)
    version
    pp_print_ts (ts, tz_offset_s)
    pp_print_string_option hostname
    pp_print_string_option app_name
    pp_print_string_option procid
    pp_print_string_option msgid

let pp_print_kv ppf (Logs.Tag.V (d, v)) =
  Format.fprintf ppf "%s=\"%a\""
    (Logs.Tag.name d) (Logs.Tag.printer d) v

let pp_print_tagset ?pp_space pp ppf set =
  Logs.Tag.fold begin fun t a ->
    begin match a, pp_space with
      | true, Some pp -> Format.fprintf ppf "%a" pp ()
      | _ -> ()
    end ;
    Format.fprintf ppf "%a" pp t ;
    true
  end set false |> fun _ -> ()

let pp_print_group ppf { section ; tags ; _ } =
  let pp_space ppf () = Format.pp_print_char ppf ' ' in
  Format.fprintf ppf "[%s %a]"
    section (pp_print_tagset ~pp_space pp_print_kv) tags

let pp_print_structured_data ppf = function
  | [] -> Format.pp_print_char ppf '-'
  | structured_data ->
    Format.pp_print_list
      ~pp_sep:(fun _ppf () -> ()) pp_print_group ppf structured_data

let pp ppf { header ; structured_data ; msg } = match msg with
  | `Ascii "" ->
    Format.fprintf ppf "%a %a"
      pp_print_header header
      pp_print_structured_data structured_data
  | `Ascii msg ->
    Format.fprintf ppf "%a %a %s"
      pp_print_header header
      pp_print_structured_data structured_data msg
  | `Utf8 msg ->
    Format.fprintf ppf "%a %a BOM%s"
      pp_print_header header
      pp_print_structured_data structured_data msg

let to_string t =
  Format.asprintf "%a" pp t

let show = to_string

let pri =
  let open Syslog_message in
  let parse_pri pri =
    match facility_of_int (pri / 8),
          severity_of_int (pri mod 8) with
    | Some f, Some s -> Some (f, s)
    | _ -> None in
  let parse_pri_exn pri =
    match parse_pri pri with
    | Some v -> v
    | None -> invalid_arg "parse_pri" in
  let open Tyre in
  conv
    parse_pri_exn
    (fun (f, s) -> int_of_facility f * 8 + int_of_severity s)
    Tyre.(char '<' *> int <* char '>')

let tsopt =
  let open Rresult in
  let open Tyre in
  conv begin function
    | `Left () -> (Ptime.min, None)
    | `Right s ->
      match Ptime.of_rfc3339 s with
      | Error (`RFC3339 _) as e ->
        R.error_msg_to_invalid_arg (Ptime.rfc3339_error_to_msg e)
      | Ok (t, tz_offset_s, _) -> (t, tz_offset_s)
  end
    (fun (t, tz_offset_s) ->
       if Ptime.(equal t min) then `Left () else `Right (Ptime.to_rfc3339 ?tz_offset_s t))
    (char '-' <|> pcre "[0-9+-\\.:TZtz]+")

let stropt =
  let open Tyre in
  conv
    (function `Right s -> s | `Left () -> "")
    (function "" -> `Left () | s -> `Right s)
    (char '-' <|> pcre "[[:graph:]]+")

let sd_name = Tyre.pcre "[^ =\\]\"]+"
let param_value = Tyre.pcre "[^\"\\\\\\]]*"

let sd_param =
  let open Tyre in
  sd_name <* char '=' <&> char '"' *> param_value <* char '"'

let parse_bool s =
  match String.lowercase_ascii s with
  | "true" | "t" -> Some true
  | "false" | "f" -> Some false
  | _ -> None

let uint64_of_string_opt v =
  try Some (Uint64.of_string v) with _ -> None

let pp_print_int64 ppf v =
  Format.fprintf ppf "%Ld" v

let tags_of_seq =
  let defs_table = Hashtbl.create 13 in
  fun s ->
    let open Logs.Tag in
    Seq.fold_left begin fun (tydefs, set) (k, v) ->
      match Hashtbl.find_opt defs_table k,
            parse_bool v,
            Int64.of_string_opt v,
            uint64_of_string_opt v,
            float_of_string_opt v with
      | Some t, Some b, _, _, _ ->
        Tag.TS.add t tydefs, Tag.add Bool t b set
      | None, Some b, _, _, _ ->
        let d = def k Format.pp_print_bool in
        let td = Tag.bool d in
        Hashtbl.add defs_table k td ;
        Tag.TS.add td tydefs, add d b set
      | Some t, None, Some i, _, _ ->
        Tag.TS.add t tydefs, Tag.add I64 t i set
      | None, None, Some i, _, _ ->
        let d = def k pp_print_int64 in
        let td = Tag.i64 d in
        Hashtbl.add defs_table k td ;
        Tag.TS.add td tydefs, add d i set
      | Some t, None, None, Some i, _ ->
        Tag.TS.add t tydefs, Tag.add U64 t i set
      | None, None, None, Some i, _ ->
        let d = def k Uint64.printer in
        let td = Tag.u64 d in
        Hashtbl.add defs_table k td ;
        Tag.TS.add td tydefs, add d i set
      | Some t, None, None, None, Some f ->
        Tag.TS.add t tydefs, Tag.add Float t f set
      | None, None, None, None, Some f ->
        let d = def k Format.pp_print_float in
        let td = Tag.float d in
        Hashtbl.add defs_table k td ;
        Tag.TS.add td tydefs, add d f set
      | Some t, None, None, None, None ->
        Tag.TS.add t tydefs, Tag.add String t v set
      | None, None, None, None, None ->
        let d = def k Format.pp_print_string in
        let td = Tag.string d in
        Hashtbl.add defs_table k td ;
        Tag.TS.add td tydefs, add d v set
    end (Tag.TS.empty, Logs.Tag.empty) s

let seq_of_tags s =
  let open Logs.Tag in
  fold begin fun (V (def, v)) a ->
    fun () ->
      Seq.Cons ((name def, Format.asprintf "%a" (printer def) v), a)
  end s Seq.empty

let sd_element =
  let open Tyre in
  conv
    (fun (section, tags) ->
       let defs, tags = tags_of_seq tags in
       { section ; defs = Tag.TS.elements defs ; tags })
    (fun { section ; tags ; _ } -> (section, seq_of_tags tags))
    (char '[' *> sd_name <&> rep (blanks *> sd_param) <* char ']')

let seq_of_list l =
  let rec aux l () = match l with
    | [] -> Seq.Nil
    | x :: tail -> Seq.Cons (x, aux tail)
  in
  aux l

let structured_data =
  let open Tyre in
  conv
    begin function
      | `Left () -> []
      | `Right (a, s) ->
        List.rev (Seq.fold_left (fun a s -> s :: a) [a] s)
    end
    begin function
      | [] -> `Left ()
      | h :: t -> `Right (h, seq_of_list t)
    end
    (char '-' <|> rep1 sd_element)

let msg =
  let open Tyre in
  conv
    (function `Left msg -> `Utf8 msg | `Right msg -> `Ascii msg)
    (function `Ascii msg -> `Right msg | `Utf8 msg -> `Left msg)
    (str "BOM" *> pcre ".*" <|> pcre "[^B].*")

let of_tyre (((((((((facility, severity), version), (ts, tz_offset_s)),
                  hostname), app_name), procid), msgid), structured_data), msg) =
  let msg = match msg with None -> `Ascii "" | Some m -> m in
  let header = {
    facility ; severity ; version ; ts ; tz_offset_s ;
    hostname ; app_name ; procid ; msgid } in
  { header ; structured_data ; msg }

let to_tyre { header = {
    facility ; severity ; version ; ts ; tz_offset_s ;
    hostname ; app_name ; procid ; msgid } ; structured_data ; msg } =
  let msg = match msg with `Ascii "" -> None | _ -> Some msg in
  (((((((((facility, severity), version), (ts, tz_offset_s)),
        hostname), app_name), procid), msgid), structured_data), msg)

let re =
  let open Tyre in
  conv of_tyre to_tyre
    (whole_string (pri <&>
                   int <&>
                   blanks *> tsopt <&>
                   blanks *> stropt <&> (* hostname *)
                   blanks *> stropt <&> (* app_name *)
                   blanks *> stropt <&> (* procid *)
                   blanks *> stropt <&> (* msgid *)
                   blanks *> structured_data <&>
                   opt (blanks *> msg)))
  |> compile

let of_string = Tyre.exec re

let severity_of_level = function
  | Logs.App -> Syslog_message.Notice
  | Error -> Error
  | Warning -> Warning
  | Info -> Informational
  | Debug -> Debug

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
