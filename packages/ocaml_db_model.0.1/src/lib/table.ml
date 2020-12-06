module Utilities = Utilities.Utilities
		     
module Table = struct
  type t = {
    table_name : string;
    table_type: string;
    engine : string;
  } [@@deriving fields]
   
  let get_tables ?conn ~schema =
    let open Mysql in
    let open Core in
    let open Core.Result in 
    let table_query ~schema =
      "SELECT table_name, table_schema, table_type, engine FROM 
       information_schema.tables WHERE table_schema='" ^ schema ^ "';" in
    let rec table_helper accum results nextrow =
      (match nextrow with
       | None -> Ok accum
       | Some arrayofstring ->
	  try
	    (let table_name =
	       String.strip
		 ~drop:Char.is_whitespace
		 (Option.value_exn
		    ~message:"Failed to get table name."
		    (Mysql.column results
				  ~key:"table_name" ~row:arrayofstring)) in
	     let table_type =
	       String.strip
		 ~drop:Char.is_whitespace
		 (Option.value_exn
		    ~message:"Failed to get table_type."
		    (Mysql.column results
				  ~key:"table_type" ~row:arrayofstring)) in
	     let engine =
	       String.strip
		 ~drop:Char.is_whitespace
		   (Option.value_exn
		      ~message:"Failed to get table engine."
		      (Mysql.column results
				    ~key:"engine" ~row:arrayofstring)) in
	     let new_table_t =
	       Fields.create ~table_name ~table_type ~engine in
	     table_helper (new_table_t::accum) results (fetch results)
	    )
	  with err ->
	    let () = Utilities.print_n_flush ("\nError " ^ (Exn.to_string err) ^
				      " getting tables from db.") in
	    Error "table.ml::get_tables() line 46"
      ) in
    let conn = (fun c -> if is_none c then Utilities.getcon_defaults () else Option.value_exn c) conn in 
    let queryresult = exec conn (table_query ~schema) in
    let isSuccess = status conn in
    match isSuccess with
    | StatusEmpty ->
       let () = Utilities.print_n_flush ("Query for table names returned nothing  ... \n") in
       Ok []
    | StatusError _ -> 
       let () = Utilities.print_n_flush ("Error in query for table  ... \n") in
       Error "table.ml::get_tables() SQL error"
    | StatusOK ->
       let () = Utilities.print_n_flush "\nGot table names..." in 
       table_helper [] queryresult (fetch queryresult);;
end
