type typ =
    ContentType of string
  | ContentEncoding of string
  | Special of string
val default_mime_types : string
val read_suffix_file : string -> unit
val guess : string -> typ
