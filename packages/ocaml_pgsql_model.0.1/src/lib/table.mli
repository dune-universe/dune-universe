module Table : sig
  type t = {
    tablename : string;
  } [@@deriving fields]

  (*Change this to return a multimap where key is table name, values are tuples of field names and types*)
  val get_tables : host:string -> user:string -> password:string -> database:string (*~schema*) -> (t list, string) Core.Result.t
end
