(** OCaml OpenCC, bindings for opencc (v1) - Open Chinese Convert

  Open Chinese Convert (OpenCC, 開放中文轉換) is an opensource project for conversion between Traditional Chinese and Simplified Chinese, supporting character-level conversion, phrase-level conversion, variant conversion and regional idioms among Mainland China, Taiwan and Hong kong. *)

(** {2 interface} *)

type t
(** the type of opencc instances *)

val create : string -> t
(** [Opencc1.create config] creates a new opencc instance. [config] should be the path of the appropriate config file. *)

val convert_utf8 : t -> string -> string
(** [Opencc1.convert_utf8 opencc text] returns the converted string. [text] is UTF-8 encoded input string. *)

val error : unit -> string
(** [Opencc1.error ()] returns the last error message. *)

(** {2 example}
On a debian jessie x86_64 system, the source code below:
{[
let ()=
  let opencc= Opencc1.create "/usr/share/opencc/s2t.json" in
  print_endline (Opencc1.convert_utf8 opencc "汉字");;
]}

will print out {b 漢字}
*)
