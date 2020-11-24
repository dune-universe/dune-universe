(**
   Available charset and collation.
*)

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

type charset = (charset_name * collation_name);;

val charset_name_to_string : charset_name -> string
val collation_name_to_string : collation_name -> string
val charset_to_string : (charset_name * collation_name) -> string
val charset_number : (charset_name * collation_name) -> int
val number_charset : int -> (charset_name * collation_name)
