(* code adapted from vecho.ml in vg library *)
(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Result

let str = Printf.sprintf
(* let otfm_err_str err = *)
(*   Format.fprintf Format.str_formatter "%a" Otfm.pp_error err; *)
(*   Format.flush_str_formatter () *)

let ( >>= ) x f = match x with
  | Error _ as e -> e
  | Ok v -> f v

(* Font information *)

module Gmap = Map.Make (Caml.Int) (* glyph maps *)
module Cmap = Gmap           (* uchar maps *)

module Font = struct
  type t = {
    font_name : string ;
    raw : string;                                      (* The font bytes. *)
    cmap : int Cmap.t;           (* Maps unicode scalar values to glyphs. *)
    advs : int Gmap.t;             (* Maps glyph to advances in em space. *)
    kern : int Gmap.t Gmap.t;    (* Maps glyph pairs to kern adjustement. *)
    head : Otfm.head ;
    hhea : Otfm.hhea ;
    units_per_em : int;                        (* Number of units per em. *)
    glyph_bbox : (int * int * int * int) Gmap.t option ;
  }

  let name fi = fi.font_name
  let data fi = fi.raw

  let ascender fi = float fi.hhea.hhea_ascender /. float fi.units_per_em
  let descender fi = float fi.hhea.hhea_descender /. float fi.units_per_em
  let xmin fi = float fi.head.head_xmin /. float fi.units_per_em
  let ymin fi = float fi.head.head_ymin /. float fi.units_per_em
  let xmax fi = float fi.head.head_xmax /. float fi.units_per_em
  let ymax fi = float fi.head.head_ymax /. float fi.units_per_em

  let string_of_file inf =
    try
      let ic = if inf = "-" then stdin else open_in_bin inf in
      let close ic = if inf <> "-" then close_in ic else () in
      let buf_size = 65536 in
      let b = Buffer.create buf_size in
      let s = Bytes.create buf_size in
      try
        while true do
          let c = input ic s 0 buf_size in
          if c = 0 then raise Exit else
            Buffer.add_subbytes b s 0 c
        done;
        assert false
      with
      | Exit -> close ic; Ok (Buffer.contents b)
      | Failure _ -> close ic; Error (`Read_error (str "%s: input file too large" inf))
      | Sys_error e -> close ic; Error (`Read_error (str "%s: %s" inf e))
    with
    | Sys_error e -> Error (`Read_error (str "%s: %s" inf e))

  let add_adv acc g adv _ = Gmap.add g adv acc
  let add_cmap acc kind (u0, u1) g =
    let acc = ref acc in
    begin match kind with
      | `Glyph_range ->
        for i = 0 to (u1 - u0) do acc := Cmap.add (u0 + i) (g + i) !acc done;
      | `Glyph ->
        for u = u0 to u1 do acc := Cmap.add u g !acc done
    end;
    !acc

  let add_ktable acc i =
    (if i.Otfm.kern_dir = `H && i.Otfm.kern_kind = `Kern then `Fold else `Skip),
    acc

  let add_kpair acc g0 g1 kv =
    let m = try Gmap.find g0 acc with Not_found -> Gmap.empty in
    Gmap.add g0 (Gmap.add g1 kv m) acc

  let int_seq a b =
    Seq.unfold (fun i -> if i < b then Some (i, i + 1) else None) a

  let seq_fold xs ~init ~f =
    let rec loop acc = function
      | Seq.Nil -> Ok acc
      | Cons (x, xs) ->
        match f acc x with
        | Ok y -> loop y (xs ())
        | Error _ as e -> e
    in
    loop init (xs ())

  let glyph_bbox d =
    Otfm.glyph_count d >>= fun gc ->
    int_seq 0 gc
    |> seq_fold ~init:Gmap.empty ~f:(fun acc i ->
        Otfm.loca d i >>= fun maybe_glyph_loc ->
        match maybe_glyph_loc with
        | Some glyph_loc ->
          Otfm.glyf d glyph_loc >>= fun (_, bbox) ->
          Ok (Gmap.add i bbox acc)
        | None -> Ok acc
      )

  let maybe_glyph_bbox d =
    match glyph_bbox d with
    | Ok r -> Ok (Some r)
    | Error (`Missing_required_table _) -> Ok None
    | Error _ as e -> e

  let load_from_string raw =
    let d = Otfm.decoder (`String raw) in
    let r =
      Otfm.postscript_name d                      >>= fun font_name ->
      Otfm.head d                                 >>= fun head ->
      Otfm.cmap d add_cmap Cmap.empty             >>= fun (_, cmap) ->
      Otfm.hmtx d add_adv Gmap.empty              >>= fun advs ->
      Otfm.kern d add_ktable add_kpair Gmap.empty >>= fun kern ->
      Otfm.hhea d                                 >>= fun hhea ->
      let font_name = match font_name with None -> "Unknown" | Some n -> n in
      let units_per_em = head.Otfm.head_units_per_em in
      maybe_glyph_bbox d                          >>= fun glyph_bbox ->
      Ok { font_name ; raw; cmap; advs; kern; hhea ; units_per_em ; head ; glyph_bbox }
    in
    (r : (_, Otfm.error) result :> (_, [> Otfm.error]) result)

  let load_from_file fn =
    match string_of_file fn with
    | Error _ as e -> e
    | Ok raw -> load_from_string raw
end

let get_glyph fi g = try Gmap.find g fi.Font.cmap with Not_found -> 0
let get_adv fi g = try Gmap.find g fi.Font.advs with Not_found -> 0
let get_kern fi g g' =
  try Gmap.find g' (Gmap.find g fi.Font.kern) with Not_found -> 0

module Layout = struct
  type t = {
    font : Font.t ;
    size : float ;
    text : string ;
    glyphs : Otfm.glyph_id list ;
    advances : v2 list ;
    width : float ;
    maxy : float ;
    miny : float ;
  }

  let rev_glyphs_of_string fi text =
    let f acc _ = function
      | `Malformed _ -> get_glyph fi (Uchar.to_int Uutf.u_rep) :: acc
      | `Uchar u ->
        get_glyph fi (Uchar.to_int u) :: acc
    in
    Uutf.String.fold_utf_8 f [] text

  let glyphs_of_string fi text =
    rev_glyphs_of_string fi text
    |> List.rev

  let maxy_and_miny_of_glyphs (fi : Font.t) glyphs =
    match fi.glyph_bbox with
    | None -> (fi.hhea.hhea_descender, fi.hhea.hhea_descender)
    | Some table ->
      let foreach_glyph ((maxy, miny) as acc) gl =
        match Gmap.find_opt gl table with
        | None -> acc
        | Some (_, miny', _, maxy') -> (max maxy maxy', min miny miny')
      in
      List.fold_left foreach_glyph (0, 0) glyphs

  let rel_advances_and_kernings_of_glyphs fi glyphs =
    let foreach_glyph (prev, advs, kerns) g =
      let advs = get_adv fi g :: advs in
      let kerns = if prev = -1 then kerns else (get_kern fi prev g) :: kerns in
      (g, advs, kerns)
    in
    let _, rev_rel_advs, rev_kernings =
      List.fold_left foreach_glyph (-1, [], []) glyphs in
    List.rev rev_rel_advs,
    List.rev rev_kernings

  let make fi ~size text =
    let u_to_em = float fi.Font.units_per_em in
    let scale x = (size *. float x) /. u_to_em in
    let glyphs = glyphs_of_string fi text in
    let rel_advs, kernings =
      rel_advances_and_kernings_of_glyphs fi glyphs in
    let advances, len =
      let rec loop acc len advs kerns = match advs, kerns with
        | adv :: advs, k :: kerns ->
          let adv = adv + k in
          let sadv = V2.v (scale adv) 0. in
          loop (sadv :: acc) (len + adv) advs kerns
        | adv :: [], [] -> List.rev acc, len + adv
        | _ -> assert false
      in
      loop [] 0 rel_advs kernings
    in
    let maxy, miny = maxy_and_miny_of_glyphs fi glyphs in
    let width = scale len in
    let maxy = scale maxy in
    let miny = scale miny in
    { font = fi ; size ; text ; glyphs ; width ; advances ; maxy ; miny }

  let width l = l.width
  let maxy l = l.maxy
  let miny l = l.miny
end

let cut ?(col = Color.black) { Layout.font ; advances ; glyphs ; text ; size ; _ } =
  let vg_font = { Vg.Font.name = Font.name font ;
                  slant = `Normal;
                  weight = `W400;
                  size } in
  Vg.I.const col |>
  Vg.I.cut_glyphs ~text ~advances vg_font glyphs
