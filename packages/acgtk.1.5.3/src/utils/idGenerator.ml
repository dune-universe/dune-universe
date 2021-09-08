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

  (** [log_content level t] logs the content of table [t].*)
  val log_content : Logs.level -> table -> unit

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
  val to_string: t->string
end

module Log = (val Logs.src_log (Logs.Src.create "ACGtkLib.idGenerator" ~doc:"logs ACGtkLib idGenerator events") : Logs.LOG) 
  
(** This module is a functor that generates a identifier generator
    from a module implementing these identifiers *)

module IdGen(ID:IdType) =
struct
  type id=ID.t
  type t=Generator of id
  let init () = Generator ID.start
  let get_fresh_id (Generator n) = n, Generator (ID.succ n)
  let eq i j = ID.compare i j=0
  let compare = ID.compare
  let id_to_string = ID.to_string
  module IdMap=Map.Make(ID)
  module Table=
    struct
      type identifier=id
      type table= {symbols:id Tries.Tries.t;
	       ids:string IdMap.t;
	       gen: t}
      exception Not_found

      let empty = {symbols=Tries.Tries.empty;
		   ids=IdMap.empty;
		   gen=init ()}
	
      let find_id_of_sym symbol {symbols=table;ids=_;gen=_} = 
	try
	  Tries.Tries.find symbol table
	with
	| Tries.Tries.Not_found -> raise Not_found
	  
      let find_sym_from_id id {symbols=_;ids=table;gen=_} = 
	try
	  IdMap.find id table
	with
	| Not_found -> raise Not_found
	  
      let add_sym sym ({symbols=syms;ids=ids;gen=vargen} as table) = 
	try
	  Tries.Tries.find sym syms,table
	with
	| Tries.Tries.Not_found -> 
	  let new_var,new_vargen=get_fresh_id vargen in
	  new_var,{symbols=Tries.Tries.add sym new_var syms;
		   ids=IdMap.add new_var sym ids;
		   gen=new_vargen}
	    
      let to_string {symbols=syms;ids=ids;gen=_} =
	let buff=Buffer.create 20 in
	let () = Buffer.add_string buff "Table from symbols to ids\n" in
	let () = Tries.Tries.fold
	  (fun key value () -> Buffer.add_string buff (Printf.sprintf "\t%s\t<->\t%s\n" key (ID.to_string value)))
	  ()
	  syms in
	let () = Buffer.add_string buff "Table from symbols to ids\n" in
	let () =
	  IdMap.iter
	    (fun key value -> Buffer.add_string buff (Printf.sprintf "\t%s\t<->\t%s\n%!" (ID.to_string key) value))
	    ids in
	Buffer.contents buff

      let log_content level {symbols=syms;ids=ids;gen=_} =
        Log.msg level (fun m -> m "Table from symbols to ids");
        Log.msg level (fun m ->
                  let () = 
                    Tries.Tries.fold
	              (fun key value () -> Log.msg level (fun m -> m "\t%s\t<->\t%s\n" key (ID.to_string value)))
	              ()
	              syms in
                  m "Done.");
        Log.msg level (fun m -> m "Table from symbols to ids");
        Log.msg level (fun m ->
                  let () = 
	            IdMap.iter
	              (fun key value -> Log.msg level (fun m -> m "\t%s\t<->\t%s\n%!" (ID.to_string key) value))
	              ids in
                  m "Done.")
                                  
      let fold f table start =
	IdMap.fold f table.ids start
    end
end

module IntId=
struct
  type t = int
  let compare i j = i-j
  let succ i = i+1
  let start =0
  let to_string = string_of_int
end

(** Module implementing the special case where identifiers ar
    integers. *)
module IntIdGen=IdGen(IntId)
