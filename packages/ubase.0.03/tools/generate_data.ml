(* San VU NGOC 2019. Bilbiothèque "Ubase".

Script utilisé pour mettre les tables à jour. 
Sous emacs, "evaluate buffer", puis generate_ubase_data ()

*)

#require "unix";;
#require "str";;
#require "uucp";;
#require "uutf";;

let default o d =
  match o with
  | None -> d
  | Some x -> x

let rec print_names first last =
  let name = Uucp.Name.name first in
  print_endline name;
  if not (Uchar.equal first last)
  then print_names (Uchar.succ first) last

let uchar_to_string u : string  =
  let b = Buffer.create 3 in
  Uutf.Buffer.add_utf_8 b u;
  Buffer.to_bytes b

let is_latin ?name u =
  let name = default name (Uucp.Name.name u) in
  Uucp.Script.script u = `Latn ||
  try let _ = Str.search_forward (Str.regexp "\\bLATIN\\b") name 0 in
    true with
  | Not_found -> false

let rec dump_latin first last =
  let name = Uucp.Name.name first in
  if is_latin ~name first
  then Printf.sprintf "%s = %i = %s"
      (uchar_to_string first) (Uchar.to_int first) name
       |> print_endline;
  if not (Uchar.equal first last)
  then dump_latin (Uchar.succ first) last

let is_diacritical_or_combining u name =
  (* Almost all diacritical have "COMBINING" in their name, but not quite
     all... *)
  let dia = Uucp.Block.block u in
  dia = `Diacriticals
  || dia = `Diacriticals_Ext
  || (try let _ = Str.search_forward (Str.regexp "\\COMBINING\\b") name 0 in
        true with
     | Not_found -> false)
     && not
       (try  let _ = Str.search_forward (Str.regexp "\\LETTER\\b") name 0 in
          true with
       | Not_found -> false)

let unwanted_adjectives = [
  "TURNED"; "REVERSED"; "CLOSED"; "OPEN"; "GLOTTAL"; "SMALL"; "SHARP"; "DOTLESS"; "LONG"; "TONE"; "AFRICAN"; "SIDEWAYS"; "DIAERESIZED"; "INSULAR"; "BOTTOM HALF"; "TOP HALF"; "BARRED"; "DENTAL"; "LATERAL"; "ALVEOLAR"; "PHARYNGEAL"; "BILABIAL"; "INVERTED"; "CAPITAL"; "SCRIPT"; "ARCHAIC"; "BASELINE"; "BLACKLETTER"; "FLATTENED"; "IOTIFIED"; "ANGLICANA"; "DOUBLE"; "STIRRUP"; "LIGATURE"; "SINOLOGICAL"; "LENIS"; "RETROFLEX"; "VOLAPUK"; "VISIGOTHIC"; "BROKEN"; "EGYPTOLOGICAL"; "MIDDLE-WELSH"; "HALF"; "TAILLESS"; "VOICED LARYNGEAL"; "STRETCHED"
]

let unwanted_adjectives_regexp =
  unwanted_adjectives
  |> List.map (fun w -> "\\(" ^ w ^ " \\)")
  |> String.concat "\\|"
  |> Str.regexp;;

(* We keep "ALEF" because it's almost a common name. We keep "CON", "DUM",
   "RUM", "TUM" because they are abbreviations. *)
let letter_replacement = [
  "ALPHA", "A";
  "BETA", "B";
  "DELTA", "D";
  "GAMMA", "G";
  "IOTA", "I";
  "LAMBDA", "L";
  "UPSILON", "U";
  "PHI", "PH";
  "CHI", "X";
  "OMEGA", "O";
  "ETH", "D";
  "THORN", "TH";
  "ENG", "NG";
  "ESH", "SH";
  "TESH", "TSH";
  "EZH", "Z";
  "DEZH", "DZ";
  "LEZH", "LZ";
  "STOP", "TS";
  "WYNN", "W";
  "SCHWA", "E";
  "HWAIR", "HW";
  "YOGH", "GH";
  "AIN", "O";
  "HENG", "H";
  "CLICK", "|";
  "RUM", "R";
  "VEND", "V";
  "TRESILLO", "3";
  "CUATRILLO", "4";
  "TWO", "2";
  "FIVE", "5";
  "SIX", "6";
  "SALTILLO", "'";
  "DOT", ":";
  "SAKHA", "WA";
]


let cleanup_name name =
  Str.global_replace unwanted_adjectives_regexp "" name

(* Convert a latin Uchar to a string containing its base letter. Uchars that
   don't have a latin base letter are kept unmodified. Rare letters with no
   obvious equivalent are put between braces, like "spirant" => "{spirant}" *)
let latin_to_base ?name u =
  let name = default name (Uucp.Name.name u) in
  if is_diacritical_or_combining u name
  then ""
  else
  if not (is_latin ~name u) then uchar_to_string u
  else
    let name = cleanup_name name in
    try
      let _ = Str.search_forward
          (Str.regexp "\\bLETTER \\([A-Z]+\\)\\b") name 0 in
      let letter = Str.matched_group 1 name in
      let letter = default (List.assoc_opt letter letter_replacement) letter in
      let letter = if String.length letter > 3 then "{" ^ letter ^ "}"
        else letter in
      if Uucp.Case.is_lower u then String.lowercase_ascii letter else letter
    with
    | Not_found -> uchar_to_string u

let dump_all_latin_to_base ?(channel = stdout) () =
  let rec loop u =
    let name = Uucp.Name.name u in
    let s = latin_to_base ~name u in
    let t = uchar_to_string u in
    if s <> t then begin
      Printf.fprintf channel
        "0x%04x, \"%s\"; (* \"%s\" = %s *)\n" (Uchar.to_int u) s t name;
    end;
    if not (Uchar.equal u Uchar.max) then loop (Uchar.succ u)
  in
  loop Uchar.min;;

let dump_space ?(channel = stdout) () =
  let rec loop u =
    let name = Uucp.Name.name u in
    let t = uchar_to_string u in
    if Uucp.White.is_white_space u then begin
      Printf.fprintf channel
        "0x%04x; (* \"%s\" = %s *)\n" (Uchar.to_int u)
        (if Uchar.to_int u < 32 then String.escaped t else t) name;
    end;
    if not (Uchar.equal u Uchar.max) then loop (Uchar.succ u)
  in
  loop Uchar.min;;

(* à effectuer pour mettre à jour ubase_data *)
let generate_ubase_data () =
  let t = Unix.gettimeofday () in
  let file = "../lib/ubase_data.ml" in
  let channel = open_out file in
  print_endline "Generating table, please wait...";
  {|(* This file is part of Ubase. It is automatically generated *)

let latin_uchar_to_base_alist = [
|}
  |> output_string channel;
  dump_all_latin_to_base ~channel ();
  print_endline (Printf.sprintf "Latin Base Time = %f" (Unix.gettimeofday () -. t));
  output_string channel "(* end of latin_uchar_to_base_alist *)
]

let white_space = [
";
  dump_space ~channel ();
  print_endline (Printf.sprintf "+ White Space Time = %f" (Unix.gettimeofday () -. t));
  output_string channel "(* end of white space *)
]

(* End of ubase_data.ml *)
";
  close_out channel;
  print_endline ("Saved in " ^ file);
  file;;


#require "uunf";;

(****)
(* CF_D145 *)
(* This is taken from
   https://erratique.ch/software/uucp/doc/Uucp.Case.html#caselesseq *)

let canonical_caseless_key s =
  let b = Buffer.create (String.length s * 2) in
  let to_nfd_and_utf_8 =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
      | `Await | `End -> ()
      | `Uchar u -> Uutf.Buffer.add_utf_8 b u; add `Await
    in
    add
  in
  let add =
    let n = Uunf.create `NFD in
    let rec add v = match Uunf.add n v with
      | `Await | `End -> ()
      | `Uchar u ->
        begin match Uucp.Case.Fold.fold u with
          | `Self -> to_nfd_and_utf_8 (`Uchar u)
          | `Uchars us -> List.iter (fun u -> to_nfd_and_utf_8 (`Uchar u)) us
        end;
        add `Await
    in
    add
  in
  let add_uchar _ _ = function
    | `Malformed  _ -> add (`Uchar Uutf.u_rep)
    | `Uchar _ as u -> add u
  in
  Uutf.String.fold_utf_8 add_uchar () s;
  add `End;
  to_nfd_and_utf_8 `End;
  Buffer.contents b

(****)

let dump_casefolding ?(channel = stdout) cf_fn =
  let rec loop u =
    let name = Uucp.Name.name u in
    if is_latin ~name u then begin
      let t = uchar_to_string u in
      let cf = cf_fn t in
      if cf <> t then begin
        Printf.fprintf channel
          "0x%04x, \"%s\"; (* \"%s\" = %s *)\n" (Uchar.to_int u) cf t name;
      end;
    end;
    if not (Uchar.equal u Uchar.max) then loop (Uchar.succ u)
  in
  loop Uchar.min;;

let dump_d145 ?(channel = stdout) () = dump_casefolding ~channel canonical_caseless_key
