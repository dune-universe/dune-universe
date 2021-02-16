module LowLevel : sig
  val move : string -> string -> unit

  val copy : string -> string -> unit

  val rmdir : string -> unit

  val rm : string -> unit

  val read_from : string -> (in_channel -> 'a) -> 'a

  val write_to : string -> (out_channel -> 'a) -> 'a

  val write_to_formatted : string -> (Format.formatter -> 'a) -> 'a
end

val dir_sep : char

val dir_sep_string : string

(* Filename.dir_sep in Ocaml >= 3.11.2 *)

module Dir : sig
  type t

  val compare : t -> t -> int

  val rm : t -> unit

  val concat : t -> t -> t

  val from_string : string -> t

  val to_string : t -> string

  val mk : t -> int -> unit

  val temp : t

  val empty : t

  val ch : t -> unit

  val cwd : unit -> t
end

type t
(** a file name, including directory information *)

val mk : ?dir:Dir.t -> string -> string -> t
(** give a directory, a base name and an extension, and obtain a file name *)

val from_string : string -> t

val to_string : t -> string

val debug_to_string : t -> string

val place : Dir.t -> t -> t
(** replace the current directory information of the file by the one given *)

val concat : Dir.t -> t -> t
(** concat directory information given to the one of the file *)

val append : t -> string -> t

val prepend : t -> string -> t
(** append string to file name - do not use this for file extensions *)

val move : t -> t -> unit
(** move a file to another place *)

val copy : t -> t -> unit

val read_from : t -> (in_channel -> 'a) -> 'a

val compare : t -> t -> int

val basename : t -> string

val extension : t -> string

val dir : t -> Dir.t

val exists : t -> bool

val rm : t -> unit

val open_out : t -> out_channel

val open_in : t -> in_channel

val open_in_gen : open_flag list -> int -> t -> in_channel

val clear_dir : t -> t

val set_ext : t -> string -> t
(** clear extension if passed empty string *)

val write_to : t -> (out_channel -> 'a) -> 'a

val write_to_formatted : t -> (Format.formatter -> 'a) -> 'a

module Map : Map.S with type key = t
