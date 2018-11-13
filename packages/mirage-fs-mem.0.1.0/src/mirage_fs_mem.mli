type write_error = [ Mirage_fs.write_error | `Directory_not_empty ]
type error = Mirage_fs.error

module Pure : sig
  type t
  val empty : unit -> t
  val read : t -> string -> int -> int -> (Cstruct.t list, Mirage_fs.error) result
  val size : t -> string -> (int64, Mirage_fs.error) result
  val create : t -> string -> (t, write_error) result
  val mkdir : t -> string -> (t, write_error) result
  val destroy : t -> string -> (t, write_error) result
  val stat : t -> string -> (Mirage_fs.stat, Mirage_fs.error) result
  val listdir : t -> string -> (string list, Mirage_fs.error) result
  val write : t -> string -> int -> Cstruct.t -> (t, write_error) result

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

include Mirage_fs_lwt.S
  with type write_error := write_error
   and type error := error

val connect : string -> t Lwt.t
val pp : t Fmt.t

val equal : t -> t -> bool
