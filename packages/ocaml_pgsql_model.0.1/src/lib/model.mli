module Pcre = Pcre
module Sql_supported_types = Sql_supported_types.Sql_supported_types
module Types_we_emit = Types_we_emit.Types_we_emit
module Model : sig
  type t = {
    col_name : string; 
    table_name : string;
    (*a type, such as Uint8.t, but as a string that we can use in directly in output.*)
    data_type : Types_we_emit.t;
    (*In our ml, if true, then the type is optional.*)
    is_nullable : bool;
    is_primary_key: bool;
  } [@@deriving show, fields]

  val get_fields_map_for_all_tables :
    regexp_opt:string option -> table_list_opt:string option ->
    host:string -> user:string -> password:string -> database:string -> schema:string -> t list Core.String.Map.t 
  val get_fields_for_given_table :
    host:string -> user:string -> password:string -> database:string -> schema:string ->
    table_name:Core.String.Map.Key.t ->
    (t list Core.String.Map.t, string) Core.Result.t 
  val construct_body : table_name:string -> map:t list Core.String.Map.t -> 
		       ppx_decorators:string list -> fields2ignore: string list option ->
		       comparable_modules:string list -> allcomparable:bool ->
		       host:string -> user:string -> password:string ->
		       database:string -> schema:string -> string
  val construct_mli : table_name:string -> map:t list Core.String.Map.t ->
		      ppx_decorators:string list -> fields2ignore: string list option ->
		      comparable_modules:string list -> allcomparable:bool -> string
  val write_appending_module : outputdir:string -> fname:string -> body:string -> unit
  val write_module : outputdir:string -> fname:string -> body:Core.Bytes.t -> unit
  (*For each key in the multi-map, construct the body of an Ocaml module
  val construct_modules : tables_and_fields:string * t list Core.String.Map.t -> string list*)
  (*val copy_utilities : destinationdir:string -> unit*)
end 
