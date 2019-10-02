(* Copyright (c) 1998-2007 INRIA *)
(* Copyright (c) 2018-2019 Geneanet *)

let nbc c =
  if Char.code c < 0b10000000 then 1
  else if Char.code c < 0b11000000 then -1
  else if Char.code c < 0b11100000 then 2
  else if Char.code c < 0b11110000 then 3
  else if Char.code c < 0b11111000 then 4
  else if Char.code c < 0b11111100 then 5
  else if Char.code c < 0b11111110 then 6
  else -1

let decode
    (fns : int -> string -> int -> int -> 'a)
    (fnc : int -> char -> 'a)
    (unsupported : int -> string -> int -> int -> 'a)
    (s : string)
    (i : int)
    (len : int)
  : 'a
  =
  let c = String.unsafe_get s i in
  let nbc = nbc c in
  if nbc < 0 || i + nbc > len
  then fnc (i + 1) c
  else

    match Char.code c with

    (* A..Z *)
    |0x41|0x42|0x43|0x44|0x45|0x46|0x47|0x48|0x49|0x4A|0x4B|0x4C|0x4D|0x4E|0x4F
    |0x50|0x51|0x52|0x53|0x54|0x55|0x56|0x57|0x58|0x59|0x5A

    (* a..z *)
    |0x61|0x62|0x63|0x64|0x65|0x66|0x67|0x68|0x69|0x6A|0x6B|0x6C|0x6D|0x6E|0x6F
    |0x70|0x71|0x72|0x73|0x74|0x75|0x76|0x77|0x78|0x79|0x7A

    (* 0..9 *)
    |0x30|0x31|0x32|0x33|0x34|0x35|0x36|0x37|0x38|0x39
    as c -> fnc (i + nbc) (Char.unsafe_chr c)

    | 0xC2 -> unsupported (i + nbc) s i nbc
    | 0xC3 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 -> fnc (i + nbc) 'A'
        | 0x86 -> fns (i + nbc) "AE" 0 2
        | 0x87 -> fnc (i + nbc) 'C'
        | 0x88 | 0x89 | 0x8A | 0x8B -> fnc (i + nbc) 'E'
        | 0x8C | 0x8D | 0x8E | 0x8F -> fnc (i + nbc) 'I'
        | 0x90 -> fnc (i + nbc) 'D'
        | 0x91 -> fnc (i + nbc) 'N'
        | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x98 -> fnc (i + nbc) 'O'
        | 0x99 | 0x9A | 0x9B | 0x9C -> fnc (i + nbc) 'U'
        | 0x9D -> fnc (i + nbc) 'Y'
        | 0x9E -> fns (i + nbc) "TH" 0 2
        | 0x9F -> fns (i + nbc) "sz" 0 2
        | 0xA0 | 0xA1 | 0xA2 | 0xA3 | 0xA4 | 0xA5 -> fnc (i + nbc) 'a'
        | 0xA6 -> fns (i + nbc) "ae" 0 2
        | 0xA7 -> fnc (i + nbc) 'c'
        | 0xA8 | 0xA9 | 0xAA | 0xAB -> fnc (i + nbc) 'e'
        | 0xAC | 0xAD | 0xAE | 0xAF -> fnc (i + nbc) 'i'
        | 0xB0 -> fnc (i + nbc) 'd'
        | 0xB1 -> fnc (i + nbc) 'n'
        | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB8 -> fnc (i + nbc) 'o'
        | 0xB9 | 0xBA | 0xBB | 0xBC -> fnc (i + nbc) 'u'
        | 0xBD | 0xBF -> fnc (i + nbc) 'y'
        | 0xBE -> fns (i + nbc) "th" 0 2
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xC4 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 | 0x82 | 0x84 -> fnc (i + nbc) 'A'
        | 0x81 | 0x83 | 0x85 -> fnc (i + nbc) 'a'
        | 0x86 | 0x88 | 0x8A | 0x8C -> fnc (i + nbc) 'C'
        | 0x87 | 0x89 | 0x8B | 0x8D -> fnc (i + nbc) 'c'
        | 0x8E | 0x90 -> fnc (i + nbc) 'D'
        | 0x8F | 0x91 -> fnc (i + nbc) 'd'
        | 0x92 | 0x94 | 0x96 | 0x98 | 0x9A -> fnc (i + nbc) 'E'
        | 0x93 | 0x95 | 0x97 | 0x99 | 0x9B -> fnc (i + nbc) 'e'
        | 0x9C | 0x9E | 0xA0 | 0xA2 -> fnc (i + nbc) 'G'
        | 0x9D | 0x9F | 0xA1 | 0xA3 -> fnc (i + nbc) 'g'
        | 0xA4 | 0xA6 -> fnc (i + nbc) 'H'
        | 0xA5 | 0xA7 -> fnc (i + nbc) 'h'
        | 0xA8 | 0xAA | 0xAC | 0xAE | 0xB0 -> fnc (i + nbc) 'I'
        | 0xA9 | 0xAB | 0xAD | 0xAF | 0xB1 -> fnc (i + nbc) 'i'
        | 0xB2 -> fns (i + nbc) "IJ" 0 2
        | 0xB3 -> fns (i + nbc) "ij" 0 2
        | 0xB4 -> fnc (i + nbc) 'J'
        | 0xB5 -> fnc (i + nbc) 'j'
        | 0xB6 -> fnc (i + nbc) 'K'
        | 0xB7 | 0xB8 -> fnc (i + nbc) 'k'
        | 0xB9 | 0xBB | 0xBD | 0xBF -> fnc (i + nbc) 'L'
        | 0xBA | 0xBC | 0xBE -> fnc (i + nbc) 'l'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xC5 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 | 0x82 -> fnc (i + nbc) 'l'
        | 0x81 -> fnc (i + nbc) 'L'
        | 0x83 | 0x85 | 0x87 | 0x8A -> fnc (i + nbc) 'N'
        | 0x84 | 0x86 | 0x88 | 0x89 | 0x8B -> fnc (i + nbc) 'n'
        | 0x8C | 0x8E | 0x90 -> fnc (i + nbc) 'O'
        | 0x8D | 0x8F | 0x91 -> fnc (i + nbc) 'o'
        | 0x92 -> fns (i + nbc) "OE" 0 2
        | 0x93 -> fns (i + nbc) "oe" 0 2
        | 0x94 | 0x96 | 0x98 -> fnc (i + nbc) 'R'
        | 0x95 | 0x97 | 0x99 -> fnc (i + nbc) 'r'
        | 0x9A | 0x9C | 0x9E | 0xA0 -> fnc (i + nbc) 'S'
        | 0x9B | 0x9D | 0x9F | 0xA1 -> fnc (i + nbc) 's'
        | 0xA2 | 0xA4 | 0xA6 -> fnc (i + nbc) 'T'
        | 0xA3 | 0xA5 | 0xA7 -> fnc (i + nbc) 't'
        | 0xA8 | 0xAA | 0xAC | 0xAE | 0xB0 | 0xB2 -> fnc (i + nbc) 'U'
        | 0xA9 | 0xAB | 0xAD | 0xAF | 0xB1 | 0xB3 -> fnc (i + nbc) 'u'
        | 0xB4 -> fnc (i + nbc) 'W'
        | 0xB5 -> fnc (i + nbc) 'w'
        | 0xB6 | 0xB8 -> fnc (i + nbc) 'Y'
        | 0xB7 -> fnc (i + nbc) 'y'
        | 0xB9 | 0xBB | 0xBD -> fnc (i + nbc) 'Z'
        | 0xBA | 0xBC | 0xBE -> fnc (i + nbc) 'z'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xC6 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x86 | 0x9F | 0xA0 -> fnc (i + nbc) 'O'
        | 0x90 -> fnc (i + nbc) 'E'
        | 0x96 | 0x97 -> fnc (i + nbc) 'I'
        | 0xA1 -> fnc (i + nbc) 'o'
        | 0xAF | 0xB1 -> fnc (i + nbc) 'U'
        | 0xB0 -> fnc (i + nbc) 'u'
        | 0xB3 -> fnc (i + nbc) 'Y'
        | 0xB4 -> fnc (i + nbc) 'y'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xC7 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x8D | 0x9E | 0xA0 | 0xBA -> fnc (i + nbc) 'A'
        | 0x8E | 0x9F | 0xA1 | 0xBB -> fnc (i + nbc) 'a'
        | 0x8F -> fnc (i + nbc) 'I'
        | 0x90 -> fnc (i + nbc) 'i'
        | 0x91 | 0xAA | 0xAC | 0xBE -> fnc (i + nbc) 'O'
        | 0x92 | 0xAB | 0xAD | 0xBF -> fnc (i + nbc) 'o'
        | 0x93 | 0x95 | 0x97 | 0x99 | 0x9B -> fnc (i + nbc) 'U'
        | 0x94 | 0x96 | 0x98 | 0x9A | 0x9C -> fnc (i + nbc) 'u'
        | 0xBC | 0xA2 -> fns (i + nbc) "AE" 0 2
        | 0xBD | 0xA3 -> fns (i + nbc) "ae" 0 2
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xC8 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 | 0x82 | 0xA6 | 0xBA -> fnc (i + nbc) 'A'
        | 0x81 | 0x83 | 0xA7 -> fnc (i + nbc) 'a'
        | 0x84 | 0x86 | 0xA8 -> fnc (i + nbc) 'E'
        | 0x85 | 0x87 | 0xA9 -> fnc (i + nbc) 'e'
        | 0x88 | 0x8A -> fnc (i + nbc) 'I'
        | 0x89 | 0x8B -> fnc (i + nbc) 'i'
        | 0x8C | 0x8E | 0xAA | 0xAC | 0xAE | 0xB0 -> fnc (i + nbc) 'O'
        | 0x8D | 0x8F | 0xAB | 0xAD | 0xAF | 0xB1 -> fnc (i + nbc) 'o'
        | 0x94 | 0x96 -> fnc (i + nbc) 'U'
        | 0x95 | 0x97 -> fnc (i + nbc) 'u'
        | 0xA2 -> fns (i + nbc) "OU" 0 2
        | 0xA3 -> fns (i + nbc) "ou" 0 2
        | 0xB2 -> fnc (i + nbc) 'Y'
        | 0xB3 -> fnc (i + nbc) 'y'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xC9 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x84 -> fnc (i + nbc) 'U'
        | 0x86 -> fnc (i + nbc) 'E'
        | 0x87 | 0x9B -> fnc (i + nbc) 'e'
        | 0x8E -> fnc (i + nbc) 'Y'
        | 0x8F -> fnc (i + nbc) 'y'
        | 0x94 -> fnc (i + nbc) 'o'
        | 0xA8 | 0xAE -> fnc (i + nbc) 'i'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xCA ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x84 -> fnc (i + nbc) 'u'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xCE ->
      (* Greek *)
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x91 -> fnc (i + nbc) 'A'
        | 0x92 -> fnc (i + nbc) 'B'
        | 0x93 -> fnc (i + nbc) 'G'
        | 0x94 -> fnc (i + nbc) 'D'
        | 0x95 -> fnc (i + nbc) 'E'
        | 0x96 -> fns (i + nbc) "DZ" 0 2
        | 0x97 -> fnc (i + nbc) 'E'
        | 0x98 -> fns (i + nbc) "TH" 0 2
        | 0x99 -> fnc (i + nbc) 'I'
        | 0x9A -> fnc (i + nbc) 'K'
        | 0x9B -> fnc (i + nbc) 'L'
        | 0x9C -> fnc (i + nbc) 'M'
        | 0x9D -> fnc (i + nbc) 'N'
        | 0x9E -> fnc (i + nbc) 'X'
        | 0x9F -> fnc (i + nbc) 'O'
        | 0xA0 -> fnc (i + nbc) 'P'
        | 0xA1 -> fnc (i + nbc) 'R'
        | 0xA2 | 0xA3 -> fnc (i + nbc) 'S'
        | 0xA4 -> fnc (i + nbc) 'T'
        | 0xA5 -> fnc (i + nbc) 'U'
        | 0xA6 -> fns (i + nbc) "PH" 0 2
        | 0xA7 -> fns (i + nbc) "KH" 0 2
        | 0xA8 -> fns (i + nbc) "PS" 0 2
        | 0xA9 -> fnc (i + nbc) 'O'
        | 0xB1 -> fnc (i + nbc) 'a'
        | 0xB2 -> fnc (i + nbc) 'b'
        | 0xB3 -> fnc (i + nbc) 'g'
        | 0xB4 -> fnc (i + nbc) 'd'
        | 0xB5 -> fnc (i + nbc) 'e'
        | 0xB6 -> fns (i + nbc) "dz" 0 2
        | 0xB7 -> fnc (i + nbc) 'e'
        | 0xB8 -> fns (i + nbc) "th" 0 2
        | 0xB9 -> fnc (i + nbc) 'i'
        | 0xBA -> fnc (i + nbc) 'k'
        | 0xBB -> fnc (i + nbc) 'l'
        | 0xBC -> fnc (i + nbc) 'm'
        | 0xBD -> fnc (i + nbc) 'n'
        | 0xBE -> fnc (i + nbc) 'x'
        | 0xBF -> fnc (i + nbc) 'o'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xCF ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 -> fnc (i + nbc) 'p'
        | 0x81 -> fnc (i + nbc) 'r'
        | 0x82 | 0x83 -> fnc (i + nbc) 's'
        | 0x84 -> fnc (i + nbc) 't'
        | 0x85 -> fnc (i + nbc) 'u'
        | 0x86 -> fns (i + nbc) "ph" 0 2
        | 0x87 -> fns (i + nbc) "kh" 0 2
        | 0x88 -> fns (i + nbc) "ps" 0 2
        | 0x89 -> fnc (i + nbc) 'o'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xD0 ->
      (* Cyrillic *)
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x81 -> fnc (i + nbc) 'E'
        | 0x86 -> fnc (i + nbc) 'I'
        | 0x90 -> fnc (i + nbc) 'A'
        | 0x91 -> fnc (i + nbc) 'B'
        | 0x92 -> fnc (i + nbc) 'V'
        | 0x93 -> fnc (i + nbc) 'G'
        | 0x94 -> fnc (i + nbc) 'D'
        | 0x95 -> fnc (i + nbc) 'E'
        | 0x96 -> fnc (i + nbc) 'J'
        | 0x97 -> fnc (i + nbc) 'Z'
        | 0x98 | 0x99 -> fnc (i + nbc) 'I'
        | 0x9A -> fnc (i + nbc) 'K'
        | 0x9B -> fnc (i + nbc) 'L'
        | 0x9C -> fnc (i + nbc) 'M'
        | 0x9D -> fnc (i + nbc) 'N'
        | 0x9E -> fnc (i + nbc) 'O'
        | 0x9F -> fnc (i + nbc) 'P'
        | 0xA0 -> fnc (i + nbc) 'R'
        | 0xA1 -> fnc (i + nbc) 'S'
        | 0xA2 -> fnc (i + nbc) 'T'
        | 0xA3 -> fns (i + nbc) "OU" 0 2
        | 0xA4 -> fnc (i + nbc) 'F'
        | 0xA5 -> fns (i + nbc) "KH" 0 2
        | 0xA6 -> fns (i + nbc) "TS" 0 2
        | 0xA7 -> fns (i + nbc) "TCH" 0 2
        | 0xA8 -> fns (i + nbc) "CH" 0 2
        | 0xA9 -> fns (i + nbc) "CHT" 0 2
        | 0xAB -> fnc (i + nbc) 'Y'
        | 0xAD -> fnc (i + nbc) 'E'
        | 0xAE -> fns (i + nbc) "YOU" 0 3
        | 0xAF -> fns (i + nbc) "YA" 0 2
        | 0xB0 -> fnc (i + nbc) 'a'
        | 0xB1 -> fnc (i + nbc) 'b'
        | 0xB2 -> fnc (i + nbc) 'v'
        | 0xB3 -> fnc (i + nbc) 'g'
        | 0xB4 -> fnc (i + nbc) 'd'
        | 0xB5 -> fnc (i + nbc) 'e'
        | 0xB6 -> fnc (i + nbc) 'j'
        | 0xB7 -> fnc (i + nbc) 'z'
        | 0xB8 | 0xB9 -> fnc (i + nbc) 'i'
        | 0xBA -> fnc (i + nbc) 'k'
        | 0xBB -> fnc (i + nbc) 'l'
        | 0xBC -> fnc (i + nbc) 'm'
        | 0xBD -> fnc (i + nbc) 'n'
        | 0xBE -> fnc (i + nbc) 'o'
        | 0xBF -> fnc (i + nbc) 'p'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xD1 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 -> fnc (i + nbc) 'r'
        | 0x81 -> fnc (i + nbc) 's'
        | 0x82 -> fnc (i + nbc) 't'
        | 0x83 -> fns (i + nbc) "ou" 0 2
        | 0x84 -> fnc (i + nbc) 'f'
        | 0x85 -> fns (i + nbc) "kh" 0 2
        | 0x86 -> fns (i + nbc) "ts" 0 2
        | 0x87 -> fns (i + nbc) "tch" 0 3
        | 0x88 -> fns (i + nbc) "ch" 0 2
        | 0x89 -> fns (i + nbc) "cht" 0 3
        | 0x8B -> fnc (i + nbc) 'y'
        | 0x8D -> fnc (i + nbc) 'e'
        | 0x8E -> fns (i + nbc) "you" 0 3
        | 0x8F -> fns (i + nbc) "ya" 0 2
        | 0x91 -> fnc (i + nbc) 'e'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xD3 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 -> fnc (i + nbc) 'I'
        | 0x95 -> fns (i + nbc) "ae" 0 2
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xD4 ->
      (* Armenian *)
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0xB1 -> fnc (i + nbc) 'A'
        | 0xB2 -> fnc (i + nbc) 'B'
        | 0xB3 -> fnc (i + nbc) 'G'
        | 0xB4 -> fnc (i + nbc) 'D'
        | 0xB5 -> fnc (i + nbc) 'E'
        | 0xB6 -> fnc (i + nbc) 'Z'
        | 0xB7 -> fnc (i + nbc) 'E'
        | 0xB8 -> fnc (i + nbc) 'E'
        | 0xB9 -> fnc (i + nbc) 'T'
        | 0xBA -> fnc (i + nbc) 'Z'
        | 0xBB -> fnc (i + nbc) 'I'
        | 0xBC -> fnc (i + nbc) 'L'
        | 0xBD -> fnc (i + nbc) 'X'
        | 0xBE -> fnc (i + nbc) 'C'
        | 0xBF -> fnc (i + nbc) 'K'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xD5 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 -> fnc (i + nbc) 'H'
        | 0x81 -> fnc (i + nbc) 'J'
        | 0x82 -> fnc (i + nbc) 'L'
        | 0x83 -> fnc (i + nbc) 'C'
        | 0x84 -> fnc (i + nbc) 'M'
        | 0x85 -> fnc (i + nbc) 'Y'
        | 0x86 -> fnc (i + nbc) 'N'
        | 0x87 -> fnc (i + nbc) 'S'
        | 0x88 -> fnc (i + nbc) 'O'
        | 0x89 -> fnc (i + nbc) 'C'
        | 0x8A -> fnc (i + nbc) 'P'
        | 0x8B -> fnc (i + nbc) 'J'
        | 0x8C -> fnc (i + nbc) 'R'
        | 0x8D -> fnc (i + nbc) 'S'
        | 0x8E -> fnc (i + nbc) 'V'
        | 0x8F -> fnc (i + nbc) 'T'
        | 0x90 -> fnc (i + nbc) 'R'
        | 0x91 -> fnc (i + nbc) 'C'
        | 0x92 -> fnc (i + nbc) 'W'
        | 0x93 -> fnc (i + nbc) 'P'
        | 0x94 -> fnc (i + nbc) 'K'
        | 0x95 -> fnc (i + nbc) 'O'
        | 0x96 -> fnc (i + nbc) 'F'
        | 0xA1 -> fnc (i + nbc) 'a'
        | 0xA2 -> fnc (i + nbc) 'b'
        | 0xA3 -> fnc (i + nbc) 'g'
        | 0xA4 -> fnc (i + nbc) 'd'
        | 0xA5 -> fnc (i + nbc) 'e'
        | 0xA6 -> fnc (i + nbc) 'z'
        | 0xA7 -> fnc (i + nbc) 'e'
        | 0xA8 -> fnc (i + nbc) 'e'
        | 0xA9 -> fnc (i + nbc) 't'
        | 0xAA -> fnc (i + nbc) 'z'
        | 0xAB -> fnc (i + nbc) 'i'
        | 0xAC -> fnc (i + nbc) 'l'
        | 0xAD -> fnc (i + nbc) 'x'
        | 0xAE -> fnc (i + nbc) 'c'
        | 0xAF -> fnc (i + nbc) 'k'
        | 0xB0 -> fnc (i + nbc) 'h'
        | 0xB1 -> fnc (i + nbc) 'j'
        | 0xB2 -> fnc (i + nbc) 'l'
        | 0xB3 -> fnc (i + nbc) 'c'
        | 0xB4 -> fnc (i + nbc) 'm'
        | 0xB5 -> fnc (i + nbc) 'y'
        | 0xB6 -> fnc (i + nbc) 'n'
        | 0xB7 -> fnc (i + nbc) 's'
        | 0xB8 -> fnc (i + nbc) 'o'
        | 0xB9 -> fnc (i + nbc) 'c'
        | 0xBA -> fnc (i + nbc) 'p'
        | 0xBB -> fnc (i + nbc) 'j'
        | 0xBC -> fnc (i + nbc) 'r'
        | 0xBD -> fnc (i + nbc) 's'
        | 0xBE -> fnc (i + nbc) 'v'
        | 0xBF -> fnc (i + nbc) 't'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | 0xD6 ->
      begin match Char.code @@ String.unsafe_get s (i+1) with
        | 0x80 -> fnc (i + nbc) 'r'
        | 0x81 -> fnc (i + nbc) 'c'
        | 0x82 -> fnc (i + nbc) 'w'
        | 0x83 -> fnc (i + nbc) 'p'
        | 0x84 -> fnc (i + nbc) 'k'
        | 0x85 -> fnc (i + nbc) 'o'
        | 0x86 -> fnc (i + nbc) 'f'
        | _ -> unsupported (i + nbc) s i nbc
      end

    | _ -> unsupported (i + nbc) s i nbc
