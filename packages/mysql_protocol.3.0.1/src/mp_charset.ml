
type charset_name = 
    Armscii8
  | Ascii
  | Binary_charset (* /!\ : renamed to avoid conflict with binary collation *)
  | Cp1250
  | Cp1251
  | Cp1256
  | Cp1257
  | Cp850
  | Cp852
  | Cp866
  | Dec8
  | Geostd8
  | Greek
  | Hebrew
  | Hp8
  | Keybcs2
  | Koi8r
  | Koi8u
  | Latin1
  | Latin2
  | Latin5
  | Latin7
  | Macce
  | Macroman
  | Swe7
  | Utf8

type collation_name =
    Armscii8_bin
  | Armscii8_general_ci
  | Ascii_bin
  | Ascii_general_ci
  | Binary_collation (* /!\ : renamed to avoid conflict with binary charset *)
  | Cp1250_bin
  | Cp1250_croatian_ci
  | Cp1250_general_ci
  | Cp1251_bin
  | Cp1251_bulgarian_ci
  | Cp1251_general_ci
  | Cp1251_general_cs
  | Cp1251_ukrainian_ci
  | Cp1256_bin
  | Cp1256_general_ci
  | Cp1257_bin
  | Cp1257_general_ci
  | Cp1257_lithuanian_ci
  | Cp850_bin
  | Cp850_general_ci
  | Cp852_bin
  | Cp852_general_ci
  | Cp866_bin
  | Cp866_general_ci
  | Dec8_bin
  | Dec8_swedish_ci
  | Geostd8_bin
  | Geostd8_general_ci
  | Greek_bin
  | Greek_general_ci
  | Hebrew_bin
  | Hebrew_general_ci
  | Hp8_bin
  | Hp8_english_ci
  | Keybcs2_bin
  | Keybcs2_general_ci
  | Koi8r_bin
  | Koi8r_general_ci
  | Koi8u_bin
  | Koi8u_general_ci
  | Latin1_bin
  | Latin1_danish_ci
  | Latin1_general_ci
  | Latin1_general_cs
  | Latin1_german1_ci
  | Latin1_german2_ci
  | Latin1_spanish_ci
  | Latin1_swedish_ci
  | Latin2_bin
  | Latin2_croatian_ci
  | Latin2_general_ci
  | Latin2_hungarian_ci
  | Latin5_bin
  | Latin5_turkish_ci
  | Latin7_bin
  | Latin7_estonian_cs
  | Latin7_general_ci
  | Latin7_general_cs
  | Macce_bin
  | Macce_general_ci
  | Macroman_bin
  | Macroman_general_ci
  | Swe7_bin
  | Swe7_swedish_ci
  | Utf8_bin
  | Utf8_czech_ci
  | Utf8_danish_ci
  | Utf8_esperanto_ci
  | Utf8_estonian_ci
  | Utf8_general_ci
  | Utf8_hungarian_ci
  | Utf8_icelandic_ci
  | Utf8_latvian_ci
  | Utf8_lithuanian_ci
  | Utf8_persian_ci
  | Utf8_polish_ci
  | Utf8_romanian_ci
  | Utf8_roman_ci
  | Utf8_slovak_ci
  | Utf8_slovenian_ci
  | Utf8_spanish2_ci
  | Utf8_spanish_ci
  | Utf8_swedish_ci
  | Utf8_turkish_ci
  | Utf8_unicode_ci

let charset_name_to_string cn = 
  match cn with
  | Armscii8 -> "Armscii8"
  | Ascii -> "Ascii"
  | Binary_charset  -> "Binary_charset" (* /!\ : renamed to avoid conflict with binary collation *)
  | Cp1250 -> "Cp1250"
  | Cp1251 -> "Cp1251"
  | Cp1256 -> "Cp1256"
  | Cp1257 -> "Cp1257"
  | Cp850 -> "Cp850"
  | Cp852 -> "Cp852"
  | Cp866 -> "Cp866"
  | Dec8 -> "Dec8"
  | Geostd8 -> "Geostd8"
  | Greek -> "Greek"
  | Hebrew -> "Hebrew"
  | Hp8 -> "Hp8"
  | Keybcs2 -> "Keybcs2"
  | Koi8r -> "Koi8r"
  | Koi8u -> "Koi8u"
  | Latin1 -> "Latin1"
  | Latin2 -> "Latin2"
  | Latin5 -> "Latin5"
  | Latin7 -> "Latin7"
  | Macce -> "Macce"
  | Macroman -> "Macroman"
  | Swe7 -> "Swe7"
  | Utf8 -> "Utf8"

type charset = (charset_name * collation_name)

let collation_name_to_string c =
  match c with
  | Armscii8_bin -> "Armscii8_bin"
  | Armscii8_general_ci -> "Armscii8_general_ci"
  | Ascii_bin -> "Ascii_bin"
  | Ascii_general_ci -> "Ascii_general_ci"
  | Binary_collation -> "Binary_collation" (* /!\ : renamed to avoid conflict with binary charset *)
  | Cp1250_bin -> "Cp1250_bin"
  | Cp1250_croatian_ci -> "Cp1250_croatian_ci"
  | Cp1250_general_ci -> "Cp1250_general_ci"
  | Cp1251_bin -> "Cp1251_bin"
  | Cp1251_bulgarian_ci -> "Cp1251_bulgarian_ci"
  | Cp1251_general_ci -> "Cp1251_general_ci"
  | Cp1251_general_cs -> "Cp1251_general_cs"
  | Cp1251_ukrainian_ci -> "Cp1251_ukrainian_ci"
  | Cp1256_bin -> "Cp1256_bin"
  | Cp1256_general_ci -> "Cp1256_general_ci"
  | Cp1257_bin -> "Cp1257_bin"
  | Cp1257_general_ci -> "Cp1257_general_ci"
  | Cp1257_lithuanian_ci -> "Cp1257_lithuanian_ci"
  | Cp850_bin -> "Cp850_bin"
  | Cp850_general_ci -> "Cp850_general_ci"
  | Cp852_bin -> "Cp852_bin"
  | Cp852_general_ci -> "Cp852_general_ci"
  | Cp866_bin -> "Cp866_bin"
  | Cp866_general_ci -> "Cp866_general_ci"
  | Dec8_bin -> "Dec8_bin"
  | Dec8_swedish_ci -> "Dec8_swedish_ci"
  | Geostd8_bin -> "Geostd8_bin"
  | Geostd8_general_ci -> "Geostd8_general_ci"
  | Greek_bin -> "Greek_bin"
  | Greek_general_ci -> "Greek_general_ci"
  | Hebrew_bin -> "Hebrew_bin"
  | Hebrew_general_ci -> "Hebrew_general_ci"
  | Hp8_bin -> "Hp8_bin"
  | Hp8_english_ci -> "Hp8_english_ci"
  | Keybcs2_bin -> "Keybcs2_bin"
  | Keybcs2_general_ci -> "Keybcs2_general_ci"
  | Koi8r_bin -> "Koi8r_bin"
  | Koi8r_general_ci -> "Koi8r_general_ci"
  | Koi8u_bin -> "Koi8u_bin"
  | Koi8u_general_ci -> "Koi8u_general_ci"
  | Latin1_bin -> "Latin1_bin"
  | Latin1_danish_ci -> "Latin1_danish_ci"
  | Latin1_general_ci -> "Latin1_general_ci"
  | Latin1_general_cs -> "Latin1_general_cs"
  | Latin1_german1_ci -> "Latin1_german1_ci"
  | Latin1_german2_ci -> "Latin1_german2_ci"
  | Latin1_spanish_ci -> "Latin1_spanish_ci"
  | Latin1_swedish_ci -> "Latin1_swedish_ci"
  | Latin2_bin -> "Latin2_bin"
  | Latin2_croatian_ci -> "Latin2_croatian_ci"
  | Latin2_general_ci -> "Latin2_general_ci"
  | Latin2_hungarian_ci -> "Latin2_hungarian_ci"
  | Latin5_bin -> "Latin5_bin"
  | Latin5_turkish_ci -> "Latin5_turkish_ci"
  | Latin7_bin -> "Latin7_bin"
  | Latin7_estonian_cs -> "Latin7_estonian_cs"
  | Latin7_general_ci -> "Latin7_general_ci"
  | Latin7_general_cs -> "Latin7_general_cs"
  | Macce_bin -> "Macce_bin"
  | Macce_general_ci -> "Macce_general_ci"
  | Macroman_bin -> "Macroman_bin"
  | Macroman_general_ci -> "Macroman_general_ci"
  | Swe7_bin -> "Swe7_bin"
  | Swe7_swedish_ci -> "Swe7_swedish_ci"
  | Utf8_bin -> "Utf8_bin"
  | Utf8_czech_ci -> "Utf8_czech_ci"
  | Utf8_danish_ci -> "Utf8_danish_ci"
  | Utf8_esperanto_ci -> "Utf8_esperanto_ci"
  | Utf8_estonian_ci -> "Utf8_estonian_ci"
  | Utf8_general_ci -> "Utf8_general_ci"
  | Utf8_hungarian_ci -> "Utf8_hungarian_ci"
  | Utf8_icelandic_ci -> "Utf8_icelandic_ci"
  | Utf8_latvian_ci -> "Utf8_latvian_ci"
  | Utf8_lithuanian_ci -> "Utf8_lithuanian_ci"
  | Utf8_persian_ci -> "Utf8_persian_ci"
  | Utf8_polish_ci -> "Utf8_polish_ci"
  | Utf8_romanian_ci -> "Utf8_romanian_ci"
  | Utf8_roman_ci -> "Utf8_roman_ci"
  | Utf8_slovak_ci -> "Utf8_slovak_ci"
  | Utf8_slovenian_ci -> "Utf8_slovenian_ci"
  | Utf8_spanish2_ci -> "Utf8_spanish2_ci"
  | Utf8_spanish_ci -> "Utf8_spanish_ci"
  | Utf8_swedish_ci -> "Utf8_swedish_ci"
  | Utf8_turkish_ci -> "Utf8_turkish_ci"
  | Utf8_unicode_ci -> "Utf8_unicode_ci"

let charset_to_string (charset, collation) =
  "(" ^ (charset_name_to_string charset) ^ ", " ^ (collation_name_to_string collation) ^ ")"

let charset_number (charset, collation) =
  match (charset, collation) with
  | (Dec8, Dec8_swedish_ci) -> 3
  | (Cp850, Cp850_general_ci) -> 4
  | (Latin1, Latin1_german1_ci) -> 5
  | (Hp8, Hp8_english_ci) -> 6
  | (Koi8r, Koi8r_general_ci) -> 7
  | (Latin1, Latin1_swedish_ci) -> 8
  | (Latin2, Latin2_general_ci) -> 9
  | (Swe7, Swe7_swedish_ci) -> 10
  | (Ascii, Ascii_general_ci) -> 11
  | (Cp1251, Cp1251_bulgarian_ci) -> 14
  | (Latin1, Latin1_danish_ci) -> 15
  | (Hebrew, Hebrew_general_ci) -> 16
  | (Latin7, Latin7_estonian_cs) -> 20
  | (Latin2, Latin2_hungarian_ci) -> 21
  | (Koi8u, Koi8u_general_ci) -> 22
  | (Cp1251, Cp1251_ukrainian_ci) -> 23
  | (Greek, Greek_general_ci) -> 25
  | (Cp1250, Cp1250_general_ci) -> 26
  | (Latin2, Latin2_croatian_ci) -> 27
  | (Cp1257, Cp1257_lithuanian_ci) -> 29
  | (Latin5, Latin5_turkish_ci) -> 30
  | (Latin1, Latin1_german2_ci) -> 31
  | (Armscii8, Armscii8_general_ci) -> 32
  | (Utf8, Utf8_general_ci) -> 33
  | (Cp866, Cp866_general_ci) -> 36
  | (Keybcs2, Keybcs2_general_ci) -> 37
  | (Macce, Macce_general_ci) -> 38
  | (Macroman, Macroman_general_ci) -> 39
  | (Cp852, Cp852_general_ci) -> 40
  | (Latin7, Latin7_general_ci) -> 41
  | (Latin7, Latin7_general_cs) -> 42
  | (Macce, Macce_bin) -> 43
  | (Cp1250, Cp1250_croatian_ci) -> 44
  | (Latin1, Latin1_bin) -> 47
  | (Latin1, Latin1_general_ci) -> 48
  | (Latin1, Latin1_general_cs) -> 49
  | (Cp1251, Cp1251_bin) -> 50
  | (Cp1251, Cp1251_general_ci) -> 51
  | (Cp1251, Cp1251_general_cs) -> 52
  | (Macroman, Macroman_bin) -> 53
  | (Cp1256, Cp1256_general_ci) -> 57
  | (Cp1257, Cp1257_bin) -> 58
  | (Cp1257, Cp1257_general_ci) -> 59
  | (Binary_charset, Binary_collation) -> 63
  | (Armscii8, Armscii8_bin) -> 64
  | (Ascii, Ascii_bin) -> 65
  | (Cp1250, Cp1250_bin) -> 66
  | (Cp1256, Cp1256_bin) -> 67
  | (Cp866, Cp866_bin) -> 68
  | (Dec8, Dec8_bin) -> 69
  | (Greek, Greek_bin) -> 70
  | (Hebrew, Hebrew_bin) -> 71
  | (Hp8, Hp8_bin) -> 72
  | (Keybcs2, Keybcs2_bin) -> 73
  | (Koi8r, Koi8r_bin) -> 74
  | (Koi8u, Koi8u_bin) -> 75
  | (Latin2, Latin2_bin) -> 77
  | (Latin5, Latin5_bin) -> 78
  | (Latin7, Latin7_bin) -> 79
  | (Cp850, Cp850_bin) -> 80
  | (Cp852, Cp852_bin) -> 81
  | (Swe7, Swe7_bin) -> 82
  | (Utf8, Utf8_bin) -> 83
  | (Geostd8, Geostd8_general_ci) -> 92
  | (Geostd8, Geostd8_bin) -> 93
  | (Latin1, Latin1_spanish_ci) -> 94
  | (Utf8, Utf8_unicode_ci) -> 192
  | (Utf8, Utf8_icelandic_ci) -> 193
  | (Utf8, Utf8_latvian_ci) -> 194
  | (Utf8, Utf8_romanian_ci) -> 195
  | (Utf8, Utf8_slovenian_ci) -> 196
  | (Utf8, Utf8_polish_ci) -> 197
  | (Utf8, Utf8_estonian_ci) -> 198
  | (Utf8, Utf8_spanish_ci) -> 199
  | (Utf8, Utf8_swedish_ci) -> 200
  | (Utf8, Utf8_turkish_ci) -> 201
  | (Utf8, Utf8_czech_ci) -> 202
  | (Utf8, Utf8_danish_ci) -> 203
  | (Utf8, Utf8_lithuanian_ci) -> 204
  | (Utf8, Utf8_slovak_ci) -> 205
  | (Utf8, Utf8_spanish2_ci) -> 206
  | (Utf8, Utf8_roman_ci) -> 207
  | (Utf8, Utf8_persian_ci) -> 208
  | (Utf8, Utf8_esperanto_ci) -> 209
  | (Utf8, Utf8_hungarian_ci) -> 210
  | (_, _) -> failwith "Unknown (charset, collation) tuple"

let number_charset n =
  match n with
  | 3 -> (Dec8, Dec8_swedish_ci)
  | 4 -> (Cp850, Cp850_general_ci)
  | 5 -> (Latin1, Latin1_german1_ci)
  | 6 -> (Hp8, Hp8_english_ci)
  | 7 -> (Koi8r, Koi8r_general_ci)
  | 8 -> (Latin1, Latin1_swedish_ci)
  | 9 -> (Latin2, Latin2_general_ci)
  | 10 -> (Swe7, Swe7_swedish_ci)
  | 11 -> (Ascii, Ascii_general_ci)
  | 14 -> (Cp1251, Cp1251_bulgarian_ci)
  | 15 -> (Latin1, Latin1_danish_ci)
  | 16 -> (Hebrew, Hebrew_general_ci)
  | 20 -> (Latin7, Latin7_estonian_cs)
  | 21 -> (Latin2, Latin2_hungarian_ci)
  | 22 -> (Koi8u, Koi8u_general_ci)
  | 23 -> (Cp1251, Cp1251_ukrainian_ci)
  | 25 -> (Greek, Greek_general_ci)
  | 26 -> (Cp1250, Cp1250_general_ci)
  | 27 -> (Latin2, Latin2_croatian_ci)
  | 29 -> (Cp1257, Cp1257_lithuanian_ci)
  | 30 -> (Latin5, Latin5_turkish_ci)
  | 31 -> (Latin1, Latin1_german2_ci)
  | 32 -> (Armscii8, Armscii8_general_ci)
  | 33 -> (Utf8, Utf8_general_ci)
  | 36 -> (Cp866, Cp866_general_ci)
  | 37 -> (Keybcs2, Keybcs2_general_ci)
  | 38 -> (Macce, Macce_general_ci)
  | 39 -> (Macroman, Macroman_general_ci)
  | 40 -> (Cp852, Cp852_general_ci)
  | 41 -> (Latin7, Latin7_general_ci)
  | 42 -> (Latin7, Latin7_general_cs)
  | 43 -> (Macce, Macce_bin)
  | 44 -> (Cp1250, Cp1250_croatian_ci)
  | 47 -> (Latin1, Latin1_bin)
  | 48 -> (Latin1, Latin1_general_ci)
  | 49 -> (Latin1, Latin1_general_cs)
  | 50 -> (Cp1251, Cp1251_bin)
  | 51 -> (Cp1251, Cp1251_general_ci)
  | 52 -> (Cp1251, Cp1251_general_cs)
  | 53 -> (Macroman, Macroman_bin)
  | 57 -> (Cp1256, Cp1256_general_ci)
  | 58 -> (Cp1257, Cp1257_bin)
  | 59 -> (Cp1257, Cp1257_general_ci)
  | 63 -> (Binary_charset, Binary_collation)
  | 64 -> (Armscii8, Armscii8_bin)
  | 65 -> (Ascii, Ascii_bin)
  | 66 -> (Cp1250, Cp1250_bin)
  | 67 -> (Cp1256, Cp1256_bin)
  | 68 -> (Cp866, Cp866_bin)
  | 69 -> (Dec8, Dec8_bin)
  | 70 -> (Greek, Greek_bin)
  | 71 -> (Hebrew, Hebrew_bin)
  | 72 -> (Hp8, Hp8_bin)
  | 73 -> (Keybcs2, Keybcs2_bin)
  | 74 -> (Koi8r, Koi8r_bin)
  | 75 -> (Koi8u, Koi8u_bin)
  | 77 -> (Latin2, Latin2_bin)
  | 78 -> (Latin5, Latin5_bin)
  | 79 -> (Latin7, Latin7_bin)
  | 80 -> (Cp850, Cp850_bin)
  | 81 -> (Cp852, Cp852_bin)
  | 82 -> (Swe7, Swe7_bin)
  | 83 -> (Utf8, Utf8_bin)
  | 92 -> (Geostd8, Geostd8_general_ci)
  | 93 -> (Geostd8, Geostd8_bin)
  | 94 -> (Latin1, Latin1_spanish_ci)
  | 192 -> (Utf8, Utf8_unicode_ci)
  | 193 -> (Utf8, Utf8_icelandic_ci)
  | 194 -> (Utf8, Utf8_latvian_ci)
  | 195 -> (Utf8, Utf8_romanian_ci)
  | 196 -> (Utf8, Utf8_slovenian_ci)
  | 197 -> (Utf8, Utf8_polish_ci)
  | 198 -> (Utf8, Utf8_estonian_ci)
  | 199 -> (Utf8, Utf8_spanish_ci)
  | 200 -> (Utf8, Utf8_swedish_ci)
  | 201 -> (Utf8, Utf8_turkish_ci)
  | 202 -> (Utf8, Utf8_czech_ci)
  | 203 -> (Utf8, Utf8_danish_ci)
  | 204 -> (Utf8, Utf8_lithuanian_ci)
  | 205 -> (Utf8, Utf8_slovak_ci)
  | 206 -> (Utf8, Utf8_spanish2_ci)
  | 207 -> (Utf8, Utf8_roman_ci)
  | 208 -> (Utf8, Utf8_persian_ci)
  | 209 -> (Utf8, Utf8_esperanto_ci)
  | 210 -> (Utf8, Utf8_hungarian_ci)
  | _  -> failwith "Unknown (charset, collation) number"
