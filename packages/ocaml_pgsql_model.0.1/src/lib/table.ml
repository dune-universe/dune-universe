module Utilities = Utilities.Utilities
module Table = struct
  type t = {
    tablename : string;
  } [@@deriving fields]
   
  let get_tables ~host ~user ~password ~database (*~schema*) =
    let open Core in
    let table_query =
      Core.String.concat
        ["SELECT tablename, schemaname, tableowner, tablespace, hasindexes, hasrules, hastriggers, rowsecurity \
          FROM pg_tables \
          WHERE schemaname NOT IN ('pg_catalog','information_schema')"] in
    let rec table_helper accum results tupleindex count =
      if tupleindex >= count then
        Core.Result.Ok accum
      else
        try
	  (let tablename =
	     Utilities.extract_field_as_string_exn
	       ~fieldname:"tablename" ~qresult:results ~tuple:tupleindex in
           let () = Utilities.print_n_flush (Core.String.concat ["\nFound table: ";tablename]) in
	   let new_table_t =
	     Fields.create ~tablename in
	   table_helper (new_table_t::accum) results (tupleindex+1) count
	  )
	with err ->
	  let () = Utilities.print_n_flush ("\ntable.ml::Error " ^ (Exn.to_string err) ^
				              " getting tables from db.") in
	  Core.Result.Error "table.ml::get_tables() line 30" in
    let conn = Utilities.getcon ~host ~user ~password ~dbname:database in 
    let queryresult = conn#exec table_query in
    let isSuccess = queryresult#status in
    match isSuccess with
    | Tuples_ok
      | Single_tuple ->
       (match queryresult#ntuples with
	| 0 -> (*should actually be impossible*)
           let () = Utilities.closecon conn in Core.Result.Ok []
	| _ ->
           let map_result = table_helper [] queryresult 0 (queryresult#ntuples) in
	   let () = Utilities.closecon conn in
           let () = Gc.full_major () in
	   map_result
       )
    | Bad_response 
      | Nonfatal_error 
      | Fatal_error ->
       let s = queryresult#error in 
       let () = Utilities.print_n_flush
                  (Core.String.concat ["table.ml::get_tables() \
			                Query of past scan requests failed. Sql error? %s \n";s]) in
       let () = Gc.full_major () in
       let () = Utilities.closecon conn in Core.Result.Ok []
    (*raise (Assert_failure ("model::get_fields_for_given_table()",98,0))*)
    (*None of the below should ever happen, at least not with plain vanilla queries*)
    | Empty_query 
      | Copy_out 
      | Copy_in 
      | Copy_both 
      | Command_ok -> 
       (* let () = print_n_flush (Core.String.concat ["model::get_fields_for_given_table() \
	  Unexpected branch. Error line 106.\n"]) in *)
       let () = Gc.full_major () in
       let () = Utilities.closecon conn in
       Core.Result.Error "table::get_tables() unuexpected retur value."
    (*raise (Failure "model::get_fields_for_given_table() \
      Unexpected return code from db.") *)
end
