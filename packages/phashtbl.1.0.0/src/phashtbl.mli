(** {2 Persistent hash tables stored on disk using dbm under the carpet.} *)

type filename = string

(** {4 Polymorphic keys and values.} *)

module GenKeyToGenVal : sig

  type ('a, 'b) t

  (** [open_new filename] creates a new persistent hashtbl.
      dbm will create [filename.dir] and [filename.pag] files. *)
  val open_new: filename -> ('a, 'b) t

  (** [open_existing filename] opens an existing persistent hashtbl
      for reading and writing.
      The files [filename.dir] and [filename.pag] must already exist. *)
  val open_existing: filename -> ('a, 'b) t

  (** [close pht] closes the previously opened [pht]. *)
  val close: ('a, 'b) t -> unit

  (** [mem pht key] checks if [key] is bound in [pht]. *)
  val mem: ('a, 'b) t -> 'a -> bool

  (** [add pht key value] binds [key] to [value] in [pht].
      Raises Dbm_error if [key] is already bound in [pht]. *)
  val add: ('a, 'b) t -> 'a -> 'b -> unit

  (** [replace pht key value] binds [key] to [value] in [pht].
      If [pht] already contains a binding for [key], that previous
      binding is discarded and replaced by [value]. *)
  val replace: ('a, 'b) t -> 'a -> 'b -> unit

  (** [remove pht key] removes [key] and its bound value from [pht].
      If [key] is unbound in [pht], raises Dbm_error. *)
  val remove: ('a, 'b) t -> 'a -> unit

  (** [find pht key] finds the value bound to [key] in [pht] or
      raises Not_found if [key] is unbound. *)
  val find: ('a, 'b) t -> 'a -> 'b

  (** [iter f pht] calls [f key value] on each
      [(key, value)] binding from [pht]. *)
  val iter: ('a -> 'b -> unit) -> ('a, 'b) t -> unit

  (** [fold f pht init] folds [f] over [pht]
      with [init] as the initial accumulator. *)
  val fold: ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

end

(** {4 String keys and polymorphic values.} *)

module StrKeyToGenVal : sig

  type 'b t

  (** [open_new filename] creates a new persistent hashtbl.
      dbm will create [filename.dir] and [filename.pag] files. *)
  val open_new: filename -> 'b t

  (** [open_existing filename] opens an existing persistent hashtbl
      for reading and writing.
      The files [filename.dir] and [filename.pag] must already exist. *)
  val open_existing: filename -> 'b t

  (** [close pht] closes the previously opened [pht]. *)
  val close: 'b t -> unit

  (** [mem pht key] checks if [key] is bound in [pht]. *)
  val mem: 'b t -> string -> bool

  (** [add pht key value] binds [key] to [value] in [pht].
      Raises Dbm_error if [key] is already bound in [pht]. *)
  val add: 'b t -> string -> 'b -> unit

  (** [replace pht key value] binds [key] to [value] in [pht].
      If [pht] already contains a binding for [key], that previous
      binding is discarded and replaced by [value]. *)
  val replace: 'b t -> string -> 'b -> unit

  (** [remove pht key] removes [key] and its bound value from [pht].
      If [key] is unbound in [pht], raises Dbm_error. *)
  val remove: 'b t -> string -> unit

  (** [find pht key] finds the value bound to [key] in [pht] or
      raises Not_found if [key] is unbound. *)
  val find: 'b t -> string -> 'b

  (** [iter f pht] calls [f key value] on each
      [(key, value)] binding from [pht]. *)
  val iter: (string -> 'b -> unit) -> 'b t -> unit

  (** [fold f pht init] folds [f] over [pht]
      with [init] as the initial accumulator. *)
  val fold: (string -> 'b -> 'c -> 'c) -> 'b t -> 'c -> 'c

end

(** {4 Polymorphic keys and string values.} *)

module GenKeyToStrVal : sig

  type 'a t

  (** [open_new filename] creates a new persistent hashtbl.
      dbm will create [filename.dir] and [filename.pag] files. *)
  val open_new: filename -> 'a t

  (** [open_existing filename] opens an existing persistent hashtbl
      for reading and writing.
      The files [filename.dir] and [filename.pag] must already exist. *)
  val open_existing: filename -> 'a t

  (** [close pht] closes the previously opened [pht]. *)
  val close: 'a t -> unit

  (** [mem pht key] checks if [key] is bound in [pht]. *)
  val mem: 'a t -> 'a -> bool

  (** [add pht key value] binds [key] to [value] in [pht].
      Raises Dbm_error if [key] is already bound in [pht]. *)
  val add: 'a t -> 'a -> string -> unit

  (** [replace pht key value] binds [key] to [value] in [pht].
      If [pht] already contains a binding for [key], that previous
      binding is discarded and replaced by [value]. *)
  val replace: 'a t -> 'a -> string -> unit

  (** [remove pht key] removes [key] and its bound value from [pht].
      If [key] is unbound in [pht], raises Dbm_error. *)
  val remove: 'a t -> 'a -> unit

  (** [find pht key] finds the value bound to [key] in [pht] or
      raises Not_found if [key] is unbound. *)
  val find: 'a t -> 'a -> string

  (** [iter f pht] calls [f key value] on each
      [(key, value)] binding from [pht]. *)
  val iter: ('a -> string -> unit) -> 'a t -> unit

  (** [fold f pht init] folds [f] over [pht]
      with [init] as the initial accumulator. *)
  val fold: ('a -> string -> 'c -> 'c) -> 'a t -> 'c -> 'c

end

(** {4 String keys and values.} *)

module StrKeyToStrVal : sig

  type t

  (** [open_new filename] creates a new persistent hashtbl.
      dbm will create [filename.dir] and [filename.pag] files. *)
  val open_new: filename -> t

  (** [open_existing filename] opens an existing persistent hashtbl
      for reading and writing.
      The files [filename.dir] and [filename.pag] must already exist. *)
  val open_existing: filename -> t

  (** [close pht] closes the previously opened [pht]. *)
  val close: t -> unit

  (** [mem pht key] checks if [key] is bound in [pht]. *)
  val mem: t -> string -> bool

  (** [add pht key value] binds [key] to [value] in [pht].
      Raises Dbm_error if [key] is already bound in [pht]. *)
  val add: t -> string -> string -> unit

  (** [replace pht key value] binds [key] to [value] in [pht].
      If [pht] already contains a binding for [key], that previous
      binding is discarded and replaced by [value]. *)
  val replace: t -> string -> string -> unit

  (** [remove pht key] removes [key] and its bound value from [pht].
      If [key] is unbound in [pht], raises Dbm_error. *)
  val remove: t -> string -> unit

  (** [find pht key] finds the value bound to [key] in [pht] or
      raises Not_found if [key] is unbound. *)
  val find: t -> string -> string

  (** [iter f pht] calls [f key value] on each
      [(key, value)] binding from [pht]. *)
  val iter: (string -> string -> unit) -> t -> unit

  (** [fold f pht init] folds [f] over [pht]
      with [init] as the initial accumulator. *)
  val fold: (string -> string -> 'c -> 'c) -> t -> 'c -> 'c

end
