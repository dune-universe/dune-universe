(* This file is part of Ubase *)

let non_ascii_white_space = List.filter (fun i -> i > 127) Ubase_data.white_space

(* This list is manually created and overrides the automatically generated list
   [latin_uchar_to_base_alist]. Please add your own. (Only non-ascii codes) *)
let misc_to_ascii_alist = [
  (* Letters *)
  0x00aa, "a"; (* "ª" = FEMININE ORDINAL INDICATOR *)
  0x00ba, "o"; (* "º" = MASCULINE ORDINAL INDICATOR *)
  0x00df, "ss";  (* "ß" = LATIN SMALL LETTER SHARP S *)

  (* Ponctuation & guillemets *)
  0x2010, "-"; (* HYPHEN *)
  0x2013, "-"; (* EN DASH *)
  0x2014, "-"; (* EM DASH *)
  0x2018, "'"; (* LEFT SINGLE QUOTATION MARK *)
  0x2019, "'"; (* RIGHT SINGLE QUOTATION MARK *)
  0x201c, "\""; (* LEFT DOUBLE QUOTATION MARK *)
  0x201d, "\""; (* RIGHT DOUBLE QUOTATION MARK *)
  0x2026, "..."; (* HORIZONTAL ELLIPSIS *)
  0x2212, "-"; (* MINUS SIGN *)
]
  |> List.append (List.map (fun i -> (i, " ")) non_ascii_white_space)


