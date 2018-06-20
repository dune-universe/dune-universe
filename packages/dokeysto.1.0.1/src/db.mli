
type filename = string

(** {4 Read-only persistent string key to string value hash table} *)

module RO: sig

  type t

  (** [open_existing fn] open in read-only mode the persistent
      hashtbl whose data are stored in file [fn] and whose
      index is stored in [fn ^ ".idx"]. *)
  val open_existing: filename -> t

  (** [close db] close the previously opened [db]. *)
  val close: t -> unit

  (** [mem db k] check if [k] is bound in [db]. *)
  val mem: t -> string -> bool

  (** [find db k] get the current binding of [k] in [db]
      or raise [Not_found]. *)
  val find: t -> string -> string

  (** [iter f db] apply [f] to all key-value pairs in [db].
      Cf. Hashtbl.iter for details. *)
  val iter: (string -> string -> unit) -> t -> unit

  (** [fold f db init] fold [f] over all key-value pairs in [db].
      Cf. Hashtbl.fold for details. *)
  val fold: (string -> string -> 'a -> 'a) -> t -> 'a -> 'a

end

(** {4 Read-only persistent string key to string value hash table
    (values are compressed on disk)} *)

module ROZ: sig

  type t

  (** [open_existing fn] open in read-only mode the persistent
      hashtbl whose data are stored compressed in file [fn] and whose
      index is stored in [fn ^ ".idx"]. *)
  val open_existing: filename -> t

  (** [close db] close the previously opened [db]. *)
  val close: t -> unit

  (** [mem db k] check if [k] is bound in [db]. *)
  val mem: t -> string -> bool

  (** [find db k] get and uncompress the current binding of [k] in [db]
      or raise [Not_found]. *)
  val find: t -> string -> string

  (** [iter f db] apply [f] to all key-value pairs in [db].
      Values are uncompressed on the fly.
      Cf. Hashtbl.iter for details. *)
  val iter: (string -> string -> unit) -> t -> unit

  (** [fold f db init] fold [f] over all key-value pairs in [db].
      Values are uncompressed on the fly.
      Cf. Hashtbl.fold for details. *)
  val fold: (string -> string -> 'a -> 'a) -> t -> 'a -> 'a

end

(** {4 Read-write persistent string key to string value hash table} *)

module RW: sig

  type t

  (** [create fn] create in read-write mode the persistent
      hashtbl whose data are stored in file [fn] and whose
      index is stored in [fn ^ ".idx"]. *)
  val create: filename -> t

  (** [open_existing fn] open in read-write mode the persistent
      hashtbl whose data are stored in file [fn] and whose
      index is stored in [fn ^ ".idx"]. *)
  val open_existing: filename -> t

  (** [close db] close the previously opened [db]. *)
  val close: t -> unit

  (** [sync db] sync to disk the data and metadata (index) of [db]. *)
  val sync: t -> unit

  (** [destroy db] rm data and metadata files of [db] and clear [db]'s
      index hashtbl. *)
  val destroy: t -> unit

  (** [mem db k] check if [k] is bound in [db]. *)
  val mem: t -> string -> bool

  (** [add db k v] add the key-value binding [(k,v)] to [db]. *)
  val add: t -> string -> string -> unit

  (** [replace db k v] replace the current binding for [k] in [db]
      by a binding from [k] to [v].
      Cf. Hashtbl.replace for details. *)
  val replace: t -> string -> string -> unit

  (** [remove tbl k] remove the current binding for [k] in [db].
      Cf. Hashtbl.replace for details. *)
  val remove: t -> string -> unit

  (** [find db k] get the current binding of [k] in [db]
      or raise [Not_found]. *)
  val find: t -> string -> string

  (** [iter f db] apply [f] to all key-value pairs in [db].
      Cf. Hashtbl.iter for details. *)
  val iter: (string -> string -> unit) -> t -> unit

  (** [fold f db init] fold [f] over all key-value pairs in [db].
      Cf. Hashtbl.fold for details. *)
  val fold: (string -> string -> 'a -> 'a) -> t -> 'a -> 'a

end

(** {4 Read-write persistent string key to string value hash table
    (values are compressed on disk)} *)

module RWZ: sig

  type t

  (** [create fn] create in read-write mode the persistent
      hashtbl whose data are stored compressed in file [fn] and whose
      index is stored in [fn ^ ".idx"]. *)
  val create: filename -> t

  (** [open_existing fn] open in read-write mode the persistent
      hashtbl whose data are stored compressed in file [fn] and whose
      index is stored in [fn ^ ".idx"]. *)
  val open_existing: filename -> t

  (** [close db] close the previously opened [db]. *)
  val close: t -> unit

  (** [sync db] sync to disk the data and metadata (index) of [db]. *)
  val sync: t -> unit

  (** [destroy db] rm data and metadata files of [db] and clear [db]'s
      index hashtbl. *)
  val destroy: t -> unit

  (** [mem db k] check if [k] is bound in [db]. *)
  val mem: t -> string -> bool

  (** [add db k v] add the key-value binding [(k,v)] to [db].
      [v] is compressed on the fly. *)
  val add: t -> string -> string -> unit

  (** [replace db k v] replace the current binding for [k] in [db]
      by a binding from [k] to [v].
      [v] is compressed on the fly.
      Cf. Hashtbl.replace for details. *)
  val replace: t -> string -> string -> unit

  (** [remove tbl k] remove the current binding for [k] in [db].
      Cf. Hashtbl.replace for details. *)
  val remove: t -> string -> unit

  (** [find db k] get and uncompress the current binding of [k] in [db]
      or raise [Not_found]. *)
  val find: t -> string -> string

  (** [iter f db] apply [f] to all key-value pairs in [db].
      Values are uncompressed on the fly.
      Cf. Hashtbl.iter for details. *)
  val iter: (string -> string -> unit) -> t -> unit

  (** [fold f db init] fold [f] over all key-value pairs in [db].
      Values are uncompressed on the fly.
      Cf. Hashtbl.fold for details. *)
  val fold: (string -> string -> 'a -> 'a) -> t -> 'a -> 'a

end
