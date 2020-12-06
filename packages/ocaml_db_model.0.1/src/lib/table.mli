module Table : sig
  type t = {
    table_name : string;
    table_type: string;
    engine : string;
  } [@@deriving fields]

  (*Change this to return a multimap where key is table name, values are tuples of field names and types*)
  val get_tables : ?conn:Mysql.dbd -> schema:string -> (t list, string) Core.Result.t
end
