type mask =
  | DEBUG
  | SYMLINK
  | COMPRESS
  | DEVICES
  | MIME_TYPE
  | MIME_ENCODING
  | CONTINUE
  | CHECK
  | PRESERVE_ATIME
  | RAW
  | ERROR
  | NO_CHECK_APPTYPE
  | NO_CHECK_ASCII
  | NO_CHECK_COMPRESS
  | NO_CHECK_ELF
  | NO_CHECK_FORTRAN
  | NO_CHECK_SOFT
  | NO_CHECK_TAR
  | NO_CHECK_TOKENS
  | NO_CHECK_TROFF

type magic

external magic_init
  : bytes -> mask list -> magic
  = "caml_magic_init" [@@noalloc]

external magic_load
  : magic -> string option -> unit
  = "caml_magic_load" [@@noalloc]

external magic_file
  : magic -> string -> string
  = "caml_magic_file"

external magic_close
  : magic -> unit
  = "caml_magic_close" [@@noalloc]

(* / *)

let make ?database flags =
  let r = Bytes.make 8 '\000' in
  let m = magic_init r flags in
  magic_load m database ; m

let info m filename =
  if not (Sys.file_exists filename)
  then Fmt.invalid_arg "%S must be exist" filename ;
  magic_file m filename
