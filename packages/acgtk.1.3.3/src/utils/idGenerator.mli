(** This module implements various useful modules to generate IDs and
    to keep track of there association with string as in a symbol table *)

(** Signature of modules encoding symbol tables *)
module type CorrespondanceTableTYPE=
sig
  (** [identifier] is the type of the identifier stored in the
      table. It is meant to be associated with a [string] *)
  type identifier

  (** The type of the table *)
  type table

  (** This exception can be raised when some identifier or some symbol
      is not found in a query *)
  exception Not_found

  (** [empty] is an empty table *)
  val empty:table

  (** [find_id_of_sym sym t] returns the identifier of the string
      [sym] stored in [t]. Raises [Not_found] if no such identifier
      exists. *)
  val find_id_of_sym : string -> table -> identifier

  (** [find_sym_from_id id t] returns the string (i.e. the symbol)
      corresponding to the identifier [id] in table [t] *)
  val find_sym_from_id : identifier -> table -> string

  (** [add_sym sym t] returns a pair [(id,t')] where [id] is the
      identifier associated with [sym] in [t']. If [sym] already was
      in [t] then [t']=[t] and [id] is the identifier which it was
      associated with. Otherwise, a new identifier is generated and
      the new association is stored in [t'].*)
  val add_sym : string -> table -> identifier*table

  (** [to_string t] outputs the table [t] in a string.*)
  val to_string : table -> string

  (** [fold f table a] returns [f id1 sym1 (f id2 sym2 ( ... ( f idN
      symN a) ... ))] where the [(id,sym)] pairs are the ones that are
      stored in the table [table]. The order of these key-value pairs in
      the table is unspecified. *)
  val fold : (identifier -> string -> 'a -> 'a) -> table -> 'a -> 'a
end

(** Signature of modules encoding a generator of identifiers *)
module type IdGen_TYPE =
sig
  (** The type of the identifier generated *)
  type id

  (** The type of the generator *)
  type t

  (** [init ()] returns a new generator *)
  val init : unit -> t

  (** [get_fresh_id gen] returnds a pair [(id,gen')] where [id] is a
      fresh [id] and [gen'] a new generator that knows [id] was already
      generated.*)
  val get_fresh_id : t -> (id*t)

  (** [eq id1 id2] returns [true] if [id1=id2] and [fase] otherwise. *)
  val eq : id -> id -> bool

  (** [compare id1 id2] returns an integer which is [0] if [id1=id2],
      negative of [id1] is less than [id2] and positive otherwise. *)
  val compare : id -> id -> int

  val id_to_string : id -> string
    
  (** [IdMap] implements maps whose keys are identifiers *)
  module IdMap : Map.S with type key=id

  (** [Table] implements correspondance tables with the current
      identifiers *)
  module Table : CorrespondanceTableTYPE with type identifier=id
end

(** Signature of encoding identifiers *)
module type IdType=
sig
  (** The type of the identifiers *)
  type t

  (** [compare id1 id2] returns an integer which is [0] if [id1=id2],
      negative of [id1] is less than [id2] and positive otherwise. *)
  val compare : t -> t -> int

  (** [succ id] returns a new identifer strictly greater than [id] *)
  val succ: t -> t

  (** [start] is some identifer *)
  val start: t

  (** [to_string id] returns a string describing the identifier *)
  val to_string: t -> string
end


(** This module is a functor that generates a identifier generator
    from a module implementing these identifiers *)
module IdGen(ID:IdType) : IdGen_TYPE with type id=ID.t

(** Module implementing the special case where identifiers ar
    integers. *)
module IntIdGen:IdGen_TYPE with type id=int
