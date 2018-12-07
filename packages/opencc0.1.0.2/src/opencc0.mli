(** OCaml OpenCC, bindings for opencc (v0) - Open Chinese Convert

  Open Chinese Convert (OpenCC, 開放中文轉換) is an opensource project for conversion between Traditional Chinese and Simplified Chinese, supporting character-level conversion, phrase-level conversion, variant conversion and regional idioms among Mainland China, Taiwan and Hong kong. *)

(** {2 interface} *)

type t
(** the type of opencc instances *)

(** the type of opencc conversion modes *)
type conversion =
  | Fast
  | SegmentOnly
  | ListCandidates

val create : string -> t
(** [Opencc0.create config] creates a new opencc instance. [config] should be the path of the appropriate config file. *)

val convert_utf8 : t -> string -> string
(** [Opencc0.convert_utf8 opencc text] returns the converted string. [text] is UTF-8 encoded input string. *)

val set_conversion_mode : t -> conversion -> unit
(** [Opencc0.set_conversion_mode opencc mode] changes the conversion [mode] of [opencc]. *)



(** {2 example}
On a debian jessie x86_64 system, the source code below:
{[
let ()=
  let opencc= Opencc0.create "/usr/lib/x86_64-linux-gnu/opencc/zhs2zht.ini" in
  Opencc0.set_conversion_mode opencc Opencc0.Fast;
  print_endline (Opencc0.convert_utf8 opencc "汉字");;
]}

will print out {b 漢字}
*)
