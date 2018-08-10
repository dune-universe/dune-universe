(** Type of node, the id should be a hash of the nodes host : port tuple, by default it uses a 64 bit FNV1A implementation*)
type t = {
    id: Cstruct.t; 
    host: string;
    port: int
  } [@@deriving fields]


val to_string: t -> string
val of_string: string -> t

val to_cstruct: t -> Cstruct.t
val of_cstruct: Cstruct.t -> t
                                                          
val make: ?id: Cstruct.t -> host: string -> port: int -> unit -> t

                                                            
val compare: t -> t -> int
