(* Copyright (C) 2018--2021  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

module Int_order = struct type t = int let compare = compare end
module Int_set = Set.Make (Int_order)
module Int_map = struct
  include Map.Make (Int_order)
  let update k f m =
    (match (try f (Some (find k m)) with Not_found -> f None) with
     | None -> m
     | Some v -> add k v m)
end

let invert_map m = Int_map.fold (fun k v -> Int_map.add v k) m Int_map.empty

module Tsv = struct

  let trim_bom s =
    let l = String.length s in
    if l = 0 || s.[0] <> '\xef' then s else begin
      assert (l >= 3 && s.[1] = '\xbb' && s.[2] = '\xbf');
      String.sub s 3 (l - 3)
    end

  let trim_cr s =
    let l = String.length s in
    if l > 0 && s.[l - 1] = '\r' then String.sub s 0 (l - 1) else s

  let read_row ?(sep = '\t') ic =
    let line = trim_cr (trim_bom (input_line ic)) in
    String.split_on_char sep line

  let iteri ?sep ?header f fp =
    let ic = open_in fp in
    try
      (match header with
       | Some header ->
          let row0 = read_row ?sep ic in
          if row0 <> header then begin
            eprintf "%s:1: Unexpected header.\n%!" fp;
            exit 65
          end
       | None -> ());
      let rec loop lnum = f lnum (read_row ?sep ic); loop (lnum + 1) in
      loop 2
    with
     | End_of_file -> close_in ic
     | exn -> close_in ic; raise exn
end

let warn fmt = ksprintf (fun msg -> prerr_endline ("Warning: " ^ msg)) fmt

let gen_is_iso639p3_bits lang3_set =
  let is_iso639p3_bits = Bytes.make 4096 '\x00' in
  lang3_set |> Int_set.iter begin function lang ->
    let i = lang / 8 in
    let bits = Char.code (Bytes.get is_iso639p3_bits i) in
    Bytes.set is_iso639p3_bits i (Char.chr (bits lor (1 lsl (lang mod 8))))
  end;
  printf "let is_iso639p3_bits = %S\n\n" (Bytes.to_string is_iso639p3_bits)

let gen_predicate fn lang_set =
  printf "let %s = function" fn;
  let index = ref 0 in
  lang_set |> Int_set.iter begin fun lang ->
    if !index mod 8 = 0 then printf "\n";
    printf " | %#x" lang;
    incr index;
  end;
  printf " -> true\n | _ -> false\n\n"

let gen_conversion fn m =
  printf "let %s = function\n" fn;
  Int_map.iter (fun k v -> printf " | %#x -> %#x\n" k v) m;
  printf " | lang -> lang\n\n"

let gen_scope_classifier fn m =
  printf "let %s = function\n" fn;
  let ctor_name = function
   | `Special -> "Special"
   | `Macro -> "Macro" in
  Int_map.iter (fun k v -> printf " | %#x -> `%s\n" k (ctor_name v)) m;
  printf " | _ -> `Individual\n\n"

let gen_one_to_many fn m =
  printf "let %s = function\n" fn;
  m |> Int_map.iter begin fun k v ->
    printf " | %#x -> \"" k;
    v |> List.iter (fun x -> printf "\\x%02x\\x%02x" (x lsr 8) (x land 0xff));
    printf "\"\n"
  end;
  print_string " | _ -> \"\"\n"

let alpha_of_int x = Char.chr (x mod 32 + 0x60)

let int_of_alpha = function
 | 'a'..'z' as c -> Char.code c - 0x60
 | _ -> raise Not_found

let int_of_alphaN s =
  let l = String.length s in
  assert (2 <= l && l <= 3);
  let rec loop i acc =
    if i = l then acc else
    loop (i + 1) (acc lsl 5 lor (int_of_alpha s.[i])) in
  loop 0 0

let int_option_of_alphaN = function "" -> None | s -> Some (int_of_alphaN s)

let alpha2_of_int x = String.init 2 (fun i -> alpha_of_int (x lsr (5*(1 - i))))
let alpha3_of_int x = String.init 3 (fun i -> alpha_of_int (x lsr (5*(2 - i))))

let merge_v3v2 ~what map_v3 map_v2 =
  let aux lang v3 v2 =
    (match v3, v2 with
     | None, None -> assert false
     | Some v3, Some v2 -> assert (v3 = v2); Some v3
     | Some v3, None ->
        warn "Code %S seen in ISO-639-3 to %s mapping \
              but not in the corresponding ISO-639-2 data."
             (alpha3_of_int lang) what;
        Some v3
     | None, Some v2 ->
        if lang < 0x8000 then
          warn "Non-collective language %S seen in ISO-639-2 to %s mapping \
                but not in the corresponding ISO-639-3 data."
               (alpha3_of_int lang) what;
        Some v2) in
  Int_map.merge aux map_v3 map_v2

let header3 = [
  "Id"; "Part2B"; "Part2T"; "Part1"; "Scope";
  "Language_Type"; "Ref_Name"; "Comment";
]
let header3m = ["M_Id"; "I_Id"; "I_Status"]
let header5 = [
  "URI"; "code"; "Label (English)"; "Label (French)";
]
let () =
  let lang3_set = ref Int_set.empty in
  let lang5_set = ref Int_set.empty in
  let lang2t_set = ref Int_set.empty in
  let lang_to_lang1_v2 = ref Int_map.empty in
  let lang_to_lang1_v3 = ref Int_map.empty in
  let lang_to_lang2b_v2 = ref Int_map.empty in
  let lang_to_lang2b_v3 = ref Int_map.empty in
  let lang3_scope_map = ref Int_map.empty in
  let lang3_macrolanguage_map = ref Int_map.empty in
  let lang3_macrolanguage_members_map = ref Int_map.empty in

  (* Load ISO-639-3 Data *)
  Sys.argv.(2) |> Tsv.iteri ~header:header3 begin fun lnum -> function
   | lang3 :: lang2b :: lang2t :: lang1 :: scope :: language_type :: ref_name
           :: comment :: litter when List.for_all ((=) "") litter ->
      let lang3 = int_of_alphaN lang3 in
      lang3_set := Int_set.add lang3 !lang3_set;
      (match int_option_of_alphaN lang2t, int_option_of_alphaN lang2b with
       | None, None -> ()
       | Some lang2t, Some lang2b ->
          if lang2t <> lang2b then
            lang_to_lang2b_v3 := Int_map.add lang2t lang2b !lang_to_lang2b_v3;
          (match int_option_of_alphaN lang1 with
           | None -> ()
           | Some lang1 ->
              lang_to_lang1_v3 := Int_map.add lang2t lang1 !lang_to_lang1_v3)
       | _ -> assert false);
      (match scope with
       | "I" -> ()
       | "S" -> lang3_scope_map := Int_map.add lang3 `Special !lang3_scope_map
       | "M" -> lang3_scope_map := Int_map.add lang3 `Macro !lang3_scope_map
       | _ -> assert false)
   | _ ->
      eprintf "%s:%d: Wrong number of columns.\n%!" Sys.argv.(2) lnum;
      exit 65
  end;
  let lang3_scope lang =
    (match Int_map.find lang !lang3_scope_map with
     | exception Not_found -> `Individual
     | `Special -> `Special | `Macro -> `Macro)
  in
  Sys.argv.(3) |> Tsv.iteri ~header:header3m begin fun lnum -> function
   | [sM; sI; ("A" | "R")] ->
      let langM = int_of_alphaN sM in
      let langI = int_of_alphaN sI in
      assert (lang3_scope langM = `Macro);
      assert (lang3_scope langI = `Individual);
      assert (not (Int_map.mem langI !lang3_macrolanguage_map));
      lang3_macrolanguage_map :=
        Int_map.add langI langM !lang3_macrolanguage_map;
      let aux = function None -> Some [langI] | Some xs -> Some (langI :: xs) in
      lang3_macrolanguage_members_map :=
        Int_map.update langM aux !lang3_macrolanguage_members_map
   | _ ->
      eprintf "%s:%d: Wrong number of columns.\n%!" Sys.argv.(2) lnum;
      exit 65
  end;

  (* Load ISO-639-5 Data *)
  Sys.argv.(4) |> Tsv.iteri ~header:header5 begin fun lnum -> function
   | [_uri; part5; en; _fr] ->
      let lang = int_of_alphaN part5 lor 0x8000 in
      lang5_set := Int_set.add lang !lang5_set
   | _ ->
      eprintf "%s:%d: Wrong number of columns.\n%!" Sys.argv.(2) lnum;
      exit 65
  end;
  let lang5_set = !lang5_set in

  (* Load ISO-639-2 Data *)
  Sys.argv.(1) |> Tsv.iteri ~sep:'|' begin fun lnum -> function
   | ["qaa-qtz"; _; _; _; _] -> ()
   | [lang2b; lang2t; lang1; _label_en; _label_fr] ->
      let lang2b = int_of_alphaN lang2b in
      let lang2t = int_option_of_alphaN lang2t in
      let lang2t, lang2b =
        (match lang2t with
         | None -> (lang2b, None)
         | Some lang2t -> (lang2t, Some lang2b)) in
      let lang2t =
        if Int_set.mem (lang2t lor 0x8000) lang5_set
        then lang2t lor 0x8000 else lang2t in
      lang2t_set := Int_set.add lang2t !lang2t_set;
      (match int_option_of_alphaN lang1 with
       | Some lang1 ->
          lang_to_lang1_v2 := Int_map.add lang2t lang1 !lang_to_lang1_v2
       | None -> ());
      (match lang2b with
       | Some lang2b ->
          lang_to_lang2b_v2 := Int_map.add lang2t lang2b !lang_to_lang2b_v2
       | None -> ())
   | _ ->
      eprintf "%s:%d: Wrong number of columns.\n%!" Sys.argv.(2) lnum;
      exit 65
  end;

  let lang3_set = !lang3_set in
  let lang2t_set = !lang2t_set in
  let lang_to_lang1 =
    merge_v3v2 ~what:"ISO-639-1" !lang_to_lang1_v3 !lang_to_lang1_v2 in
  let lang_to_lang2b =
    merge_v3v2 ~what:"ISO-639-2B" !lang_to_lang2b_v3 !lang_to_lang2b_v2 in

  let lang_set = Int_set.union lang3_set lang5_set in
  Int_set.diff lang2t_set lang_set |> Int_set.iter begin fun lang ->
    warn "Exclunding %S from ISO-639-2 \
          as it is present in neither ISO-639-3 nor ISO-639-5 tables."
      (alpha3_of_int lang)
  end;

  gen_is_iso639p3_bits lang3_set;
  gen_predicate "is_iso639p5" lang5_set;
  gen_predicate "is_iso639p2t" (Int_set.inter lang2t_set lang_set);
  gen_conversion "to_iso639p2b" lang_to_lang2b;
  gen_conversion "of_iso639p2b" (invert_map lang_to_lang2b);
  gen_conversion "to_iso639p1" lang_to_lang1;
  gen_conversion "of_iso639p1" (invert_map lang_to_lang1);
  gen_scope_classifier "lang3_scope" !lang3_scope_map;
  gen_conversion "lang3_macrolanguage" !lang3_macrolanguage_map;
  gen_one_to_many "lang3_macrolanguage_members" !lang3_macrolanguage_members_map
