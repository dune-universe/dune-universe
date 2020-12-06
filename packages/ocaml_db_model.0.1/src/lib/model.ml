module Pcre = Pcre
module Utilities = Utilities.Utilities
module Table = Table.Table
module Sql_supported_types = Sql_supported_types.Sql_supported_types
module Types_we_emit = Types_we_emit.Types_we_emit
module Mysql = Mysql
module Model = struct
  type t = {
    col_name : string; 
    table_name : string;
    data_type : Types_we_emit.t;
    is_nullable : bool;
    is_primary_key : bool;
  } [@@deriving show, fields]

  let get_fields_for_given_table ?conn ~table_name =
    let open Mysql in
    let open Core in 
    (*Only column_type gives us the acceptable values of an enum type if present, 
      unsigned; use the column_comment to input per field directives for ppx 
      extensions...way down the road, such as key or default for json ppx extension. 
      In future for compare ppx extension, perhaps set all fields to return zero 
      EXCEPT for the primary key of table? This is also useful for Core Comparable 
      interface.*)
    let fields_query = String.concat [
			   "SELECT column_name, is_nullable, column_comment,
			    column_type, data_type, column_key, extra, column_comment FROM 
			    information_schema.columns 
			    WHERE table_name='";table_name;"';"] in
    (* numeric_scale, column_default, character_maximum_length, 
    character_octet_length, numeric_precision,*)
    let conn = (fun c -> if is_none c then
			   Utilities.getcon_defaults ()
			 else
			   Option.value_exn c) conn in 
    let rec helper accum results nextrow =
      (match nextrow with
       | None -> Core.Result.Ok accum
       | Some arrayofstring ->
	  try
	    (let col_name =
	       Utilities.extract_field_as_string_exn
		 ~fieldname:"column_name" ~results ~arrayofstring in 
	     let data_type =
	       Utilities.extract_field_as_string_exn
		 ~fieldname:"data_type" ~results ~arrayofstring in 
	     let col_type =
	       Utilities.extract_field_as_string_exn
		 ~fieldname:"column_type" ~results ~arrayofstring in 
	     let is_nullable =
	       Utilities.parse_bool_field_exn
		 ~fieldname:"is_nullable" ~results ~arrayofstring in 
	     let is_primary_key =
	       let is_pri = Utilities.extract_field_as_string_exn
			      ~fieldname:"column_key" ~results ~arrayofstring in 
	       (fun x -> match x with "pri" -> true | _ -> false) is_pri in
	     (*--todo--convert data types and nullables into ml types as 
               strings for use in writing a module*)
	     let type_for_module =
	       Sql_supported_types.one_step ~data_type ~col_type ~col_name in
	     let new_field_record =
	       Fields.create
		 ~col_name
		 ~table_name
		 ~data_type:type_for_module
		 ~is_nullable
		 ~is_primary_key in
	     let newmap = String.Map.add_multi accum ~key:table_name ~data:new_field_record in 
	     helper newmap results (fetch results)
	    )
	  with err ->
	    let () = Utilities.print_n_flush
		       (String.concat ["\nError ";(Exn.to_string err);
				       " getting tables from db."]) in
	    Core.Result.Error "Failed to get tables from db."
      ) in
    let queryresult = exec conn fields_query in
    let isSuccess = status conn in
    match isSuccess with
    | StatusEmpty ->  Core.Result.Ok String.Map.empty
    | StatusError _ -> 
       let () = Utilities.print_n_flush
		  ("Query for table names returned nothing.  ... \n") in
       let () = Utilities.closecon conn in
       Core.Result.Error "model.ml::get_fields_for_given_table() Error in sql"
    | StatusOK -> (*let () = Utilities.print_n_flush "\nGot fields for table." in *)
		  helper String.Map.empty queryresult (fetch queryresult);;

  let make_regexp s =
    let open Core in 
    match s with
    | Some sr ->
       (try
	   let () = Utilities.print_n_flush
		      (String.concat ["make_regexp() from ";sr]) in 
	   let regexp = Pcre.regexp sr in Some regexp
	 with
	 | err -> let () = Utilities.print_n_flush "\nFailed to parse regexp..." in
	raise err
       )
    | None -> None;;
    
  let parse_list s =
    let open Core in 
    try
      match s with
      | Some sl ->
	 (try
	     let () = Utilities.print_n_flush (String.concat ["parse_list() from ";sl]) in
	     let l = Core.String.split sl ~on:',' in
	     let len = Core.List.length l in 
	     if len > 0 then Some l else None
	   with
	   | err ->
	      let () = Utilities.print_n_flush
			 "\nFailed parsing optional list..." in
	      raise err
	 )
      | None -> None
    with
    | _ -> None;;
    
  let get_fields_map_for_all_tables ~regexp_opt ~table_list_opt ~conn ~schema =
    let open Core in
    let open Core.Result in 
    let table_list_result = Table.get_tables ~conn ~schema in
    if is_ok table_list_result then
      let tables = ok_or_failwith table_list_result in
      let regexp_opt = make_regexp regexp_opt in
      let table_list_opt = parse_list table_list_opt in 
      let rec helper ltables map =
	let update_map ~table_name =
	  let fs_result = get_fields_for_given_table ~conn ~table_name in
	  if is_ok fs_result then
	    let newmap = ok_or_failwith fs_result in
	     let combinedmaps =
	       Map.merge
		 map newmap
		 ~f:
		 (fun ~key:_key vals ->
		  match vals with
		  | `Left v1 -> Some v1
		  | `Right v2 -> Some v2
		  | `Both (_,_) -> raise (Failure "Duplicate table name!?!") 
		 ) in  
	     combinedmaps
	  else  
	    map in 
	match ltables with
	| [] -> map
	| h::t ->
	   (**---filter on regexp or list here, if present at all---*)
	   (match regexp_opt, table_list_opt with
	    | None, Some l ->
	       if List.mem l h.Table.table_name ~equal:String.equal then
		 let newmap = update_map ~table_name:h.Table.table_name in
		 helper t newmap
	       else
		 helper t map
	    | Some r, None ->
	       (try
		   let _intarray =
		     Pcre.pcre_exec ?rex:(Some r) h.Table.table_name in
		   let newmap = update_map ~table_name:h.Table.table_name in
		   helper t newmap
		 with
		 | _ -> helper t map
	       )
	    | Some r, Some _l -> (*--presume regexp over list---*)
	       (try
		   let _intarray =
		     Pcre.pcre_exec ?rex:(Some r) h.Table.table_name in
		   let newmap = update_map ~table_name:h.Table.table_name in
		   helper t newmap
		 with
		 | _ -> helper t map
	       )
	    | None, None -> 
	       let newmap = update_map ~table_name:h.Table.table_name in
	       helper t newmap
	   ) in
      helper tables String.Map.empty
    else
      let () = Utilities.print_n_flush "\nFailed to get list of tables.\n" in
      String.Map.empty;;
    
  (**Construct an otherwise tedious function that creates instances of type t from
     a query; for each field in struct, get the string option from the string 
     option array provided by Mysql under the same name, parse it into it's correct 
     type using the correct conversion function, and then use Fields.create to 
     create a new record, add it to an accumulator, and finally return that 
     accumulator after we have exhausted all the records returned by the query. 
     Cannot use long line continuation backslashes here; screws up the formatting 
     in the output.*)
  let construct_sql_query_function ~table_name ~fields_list ~host
				   ~user ~password ~database =
    let open Core in 
    let preamble =
      String.concat ["  let get_from_db ~query =\n";
		     "    let open Mysql in \n";
		     "    let open Core in \n";
		     "    let conn = Utilities.getcon ~host:\"";host;"\" ~user:\"";user;"\" \n";
		     "                               ~password:\"";password;"\" ~database:\"";database;"\" in"] in
    let helper_preamble =
      Core.String.concat
	["    let rec helper accum results nextrow = \n";
	 "      (match nextrow with \n";
	 "       | None -> Core.Result.Ok accum \n       | Some arrayofstring ->\n";
	 "          try "] in
    let suffix =
      String.concat 
	["    let queryresult = exec conn query in\n";
	 "    let isSuccess = status conn in\n";
	 "    match isSuccess with\n    | StatusEmpty ->  Core.Result.Ok [] \n";
	 "    | StatusError _ -> \n";
	 "       let () = Utilities.print_n_flush (\"Error during query of table ";table_name;"...\") in\n";
	 "       let () = Utilities.closecon conn in\n";
	 "       Core.Result.Error \"get_from_db() Error in sql\"\n";
	 "    | StatusOK -> \n";
	 "       let () = Utilities.print_n_flush \"Query successful from ";table_name;" table.\" in \n";
         "       let () = Utilities.closecon conn in\n";
	 "       helper [] queryresult (fetch queryresult);;\n"] in
(*    let rec build_a_line_from_a_list_wrap_at_x_columns parts maxwidth length_of_growingline
						       indentation acc =
      match parts with
      | [] -> String.concat ~sep:" " (List.rev acc)
      | h :: t ->
	 let newlength = ((String.length h) + length_of_growingline) in
	 if newlength > maxwidth && ((String.length h) > 2) then
	   let elem = (Core.String.concat [h;"\n";indentation]) in 
	   build_a_line_from_a_list_wrap_at_x_columns
	     t maxwidth (String.length indentation) indentation (elem::acc)
	 else
	   build_a_line_from_a_list_wrap_at_x_columns
	     t maxwidth newlength indentation (h::acc) in *)
    let rec for_each_field ~flist ~accum =
      match flist with
      | [] -> String.concat ~sep:"\n" accum
      | h :: t ->
	 let parser_function_call =
	   Types_we_emit.converter_of_string_of_type
	     ~is_optional:h.is_nullable ~t:h.data_type ~fieldname:h.col_name in
	 (*support breaking line at least into 2 parts if too long
	 let parser_function_call_split_on_spaces =
	   String.split ~on:' ' parser_function_call in*)
	 (*let len_so_far_of_line = 12 + (String.length h.col_name) + 7 in
	 let tabs = len_so_far_of_line / 8 in
	 let spaces = len_so_far_of_line % 8 in
	 let indentation = Core.String.concat
			     [(Core.String.make tabs '\t');(Core.String.make spaces ' ')] in
	 let output = build_a_line_from_a_list_wrap_at_x_columns
			parser_function_call_split_on_spaces 110
			(*These tabs are specific to the parser functions*)
			len_so_far_of_line indentation [] in *)
	 (*let output =
	   if ((String.length (List.hd_exn parser_function_call_split_on_spaces))
	       + len_so_far_of_line > 90) then
	     String.concat
	       ["            let ";h.col_name;" = \n";
		(List.hd_exn parser_function_call_split_on_spaces);"\n";
		(List.slice parser_function_call_split_on_spaces 1 0);" in "] else
	     String.concat
	       ["            let ";h.col_name;" = \n";
		parser_function_call;" in "] in *)
	 for_each_field
	   ~flist:t
	   ~accum:((String.concat
		      ["            let ";h.col_name;" = \n              ";parser_function_call;" in "])::accum) in
    let rec make_fields_create_line ~flist ~accum =
      match flist with
      | [] -> let fields = String.concat ~sep:" " accum in
	 (*let fields_w_newlines =
	   build_a_line_from_a_list_wrap_at_x_columns accum 120 38 "\t\t\t\t     " accum in*)
	 String.concat ["            let new_t = Fields.create ";fields;" in "]
      | h :: t ->
	 let onef = String.concat ["~";h.col_name] in
	 make_fields_create_line ~flist:t ~accum:(onef::accum) in 
    let creation_line = make_fields_create_line ~flist:fields_list ~accum:[] in
    let recursive_call = "            helper (new_t :: accum) results (fetch results) " in 
    let parser_lines = for_each_field ~flist:fields_list ~accum:[] in
    String.concat ~sep:"\n" [preamble;helper_preamble;parser_lines;creation_line;
			     recursive_call;"          with\n          | err ->";
			     "             let () = Utilities.print_n_flush (String.concat [\"\\nError: \";(Exn.to_string err);\"Skipping a record...\"]) in";
			     "             helper accum results (fetch results)\n";
			     "      ) in";suffix];;

  (**Construct an otherwise tedious function that creates SQL needed to save 
     records of type t to a db.*)
  let construct_sql_serialize_function ~fields_list =
    let open Core in 
    let preamble =
      String.concat ["  let generateSQLvalue_for_insert t conn =\n";
		     "    let open Core in \n";
		     "    let serialize t =\n";
		     "      let conv to_s = fun acc f ->\n";
		     "        (sprintf \"%s\" (to_s (Field.get f t))) :: acc in\n";
		     "      let fs = Fields.fold\n";
		     "                 ~init:[] (*conversion functions must single quote where needed*)"] in
    let suffix =
      String.concat 
	["      let reversed = List.rev fs in\n";
	 "      let onerecord = String.concat [\"(\";(String.concat reversed ~sep:\",\");\")\"] in \n";
	 "      onerecord in\n";
	 "    serialize t;;\n"] in
    let rec for_each_field ~flist ~accum =
      match flist with
      | [] -> String.concat ~sep:"\n" accum
      | h :: t ->
	 let serialize_function_call =
	   Types_we_emit.converter_to_string_of_type
	     ~is_optional:h.is_nullable ~t:h.data_type (*~fieldname:h.col_name*) in
	 let output = String.concat ["                 ~";h.col_name;":";
				     serialize_function_call] in
	 for_each_field ~flist:t ~accum:(output::accum) in
    let fields_lines = for_each_field ~flist:fields_list ~accum:[] in
    String.concat ~sep:"\n" [preamble;fields_lines;"      in";suffix];;

  let list_other_modules () =
    (*Refer to this project's implementation where possible; utilities MUST be locally provided using an include 
      statement (include Ocaml_db_model.Utilities) and customized over-ridden versions of the connections 
      establishment functions that require db  credentials, plus any additional functions a user may want.*)
    Core.String.concat ~sep:"\n" ["(*===DO NOT EDIT===This file was autogenerated by ocaml-db-model and might be overwritten on subsequent builds.
                                  DONT FORGET to create your own Utilities module and to 'include Ocaml_db_model.Utilities' alongside";
                                  "any customized functions you may need*)";
                                  "module Utilities = Utilities.Utilities";
                                  "module CoreInt32_extended = Coreint32_extended.CoreInt32_extended";
                                  "module CoreInt64_extended = Coreint64_extended.CoreInt64_extended";
				  "module Uint64_extended = Ocaml_db_model.Uint64_extended";
				  "module Uint32_extended = Ocaml_db_model.Uint32_extended";
				  "module Uint16_extended = Ocaml_db_model.Uint16_extended";
				  "module Uint8_extended = Ocaml_db_model.Uint8_extended";
				  "module Date_extended = Ocaml_db_model.Date_extended";
				  "module Date_time_extended = Ocaml_db_model.Date_time_extended";
				  "module Bignum_extended = Ocaml_db_model.Bignum_extended";
				  "open Sexplib.Std\n"];;
  let list_other_modules_for_mli () =
    (*Refer to this project's implementation where possible; utilities MUST be locally provided using an include 
      statement and customized over-ridden versions of the connections establishment functions that require db 
      credentials, plus any additional functions a user may want.*)                  
    Core.String.concat ~sep:"\n" ["===DO NOT EDIT===This file was autogenerated by ocaml-db-model and might be overwritten on subsequent builds.";
                                  "module Utilities = Utilities.Utilities";
                                  "module CoreInt32_extended = Coreint32_extended.CoreInt32_extended";
                                  "module CoreInt64_extended = Coreint64_extended.CoreInt64_extended";
				  "module Uint64_extended = Ocaml_db_model.Uint64_extended";
				  "module Uint32_extended = Ocaml_db_model.Uint32_extended";
				  "module Uint16_extended = Ocaml_db_model.Uint16_extended";
				  "module Uint8_extended = Ocaml_db_model.Uint8_extended";
				  "module Date_extended = Ocaml_db_model.Date_extended";
				  "module Date_time_extended = Ocaml_db_model.Date_time_extended";
				  "module Bignum_extended = Ocaml_db_model.Bignum_extended\n"];;
  let duplicate_clause_function ~fields () =
    let rec find_primary_key_field_name fs =
      match fs with
      | h :: t ->
	 if h.is_primary_key then h.col_name
	 else find_primary_key_field_name t
      (*if we get here it's b/c there is no primary key on the table...warn the user but use 
        any old even imaginary name...*)
      | [] -> "id" in
    let primary_key_name = find_primary_key_field_name fields in 
    Core.String.concat
      ~sep:"\n"
      ["  (*This has to be MANUALLY MODIFIED -- depends on semantics of the fields and which ";
       "    we want to update if any and how and which is a key, or which are keys.*)";
       "  let get_sql_insert_on_duplicate_clause () =";
       (Core.String.concat ["    let fields_less_key = Core.List.filter (Fields.names) ~f:(fun x -> not (x = \"";primary_key_name;"\")) in"]);
       "    let rec create_set_values fieldslist clause = ";
       "      match fieldslist with";
       "      | [] -> Core.String.concat ~sep:\",\" clause";
       "      | h :: t ->";
       "	 let onefield = Core.String.concat ~sep:\"\" [h;\"=VALUES(\";h;\")\"] in";
       "         create_set_values t (onefield::clause) in";
       "    let set_clause = create_set_values fields_less_key [] in ";
       "    Core.String.concat [\" ON DUPLICATE KEY UPDATE \";set_clause;\";\"];;\n"]

  let construct_save_function ~table_name =
    Core.String.concat
      ~sep:"\n"
      ["  let rec save2db ~records =";
       "    let open Core in ";
       "    let open Mysql in ";
       "    (*====MANUALLY ALTER MAX PACKET SIZE====";
       "      If we have many records we're limited by smaller of either 1000 records OR ";
       "      the max packet size, which is a configuration setting on the sql server; ";
       "      here we hard code the max packet size and consume just enough records to ";
       "      stay just under the limit and repeat until all have been saved.*)";
       "    let insert_statement_start = get_sql_insert_statement () in";
       "    let overhead = Core.String.length insert_statement_start in";
       "    let max_packet_size = ((1024 * 1024 * 16) - overhead) in";
       "    let count = List.length records in";
       "    let () = Utilities.print_n_flush";
       "               (String.concat [\"\\n\";tablename;\"::save2db() invoked with \";";
       "  			     (string_of_int count);\" records remaining...\"]) in";
       "    (*helper function that incrementally builds the VALUES() portion of ";
       "      INSERT statement and returns the partial statement and the index into the list";
       "      of the last record included, which is where we should pickup again*)";
       "    let rec consume_records records index sqlvalues conn =";
       "      match records with";
       "      | [] -> sqlvalues, None";
       "      | h :: t ->";
       "         let nextvalue = generateSQLvalue_for_insert h conn in";
       "         let len = Core.List.fold sqlvalues ~init:0 ~f:(fun len x -> (Core.String.length x) + len) in";
       "         let marginal_len = Core.String.length nextvalue in";
       "         if ((marginal_len + len) > max_packet_size) || index >= 999 then";
       "           sqlvalues, Some (index)";
       "         else ";
       "           consume_records t (index+1) (nextvalue::sqlvalues) conn in ";
       "    if count > 0 then ";
       "      let conn = Utilities.getcon_w_defaults () in ";
       "      (* If we need to delete any records, do so here...you'll have to write this ";
       "      non-existent function on your own to create the DELETE statement.";
       "      let prefix = get_sql_insert_command_prefix in *) ";
       "      (*let values = generate_values_for_sql_of_list ~records ~conn in*)";
       "      let values_list, last_index = consume_records records 0 [] conn in";
       "      let values= Core.String.concat ~sep:\",\"   values_list in ";
       "      let on_update_clause = get_sql_insert_on_duplicate_clause () in ";
       "      let insert_statement = String.concat [insert_statement_start;values;on_update_clause] in ";
       "      (*Print to inspect the sql:";
       "      let thecommand =";
       "        String.concat [\"START TRANSACTION;\"(*;prefix*);insert_statement;\"COMMIT;\"] in";
       "      let () = Utilities.print_n_flush (String.concat [\"\\n\";thecommand]) in*)";
       "      let _result = exec conn \"START TRANSACTION;\" in";
       "      (*let _ = exec conn prefix in*)";
       "      let _result2 = exec conn insert_statement in";
       "      let _result3 = exec conn \"COMMIT;\" in";
       "      let isSuccess = status conn in";
       "      match isSuccess with";
       "      | StatusOK ->";
       "         let i64opt = (Int64.to_int (affected conn)) in";
       "         (match i64opt with";
       "          | Some _affected ->";
       "             (*Returns a zero even if successful and inserts > 0 records...not ";
       "               sure why...need to read documentation, or source code.*)";
       "  	   let () = Utilities.print_n_flush";
       "             (Core.String.concat [\"\\nSuccessfully inserted new records into \";";
       "  		                  tablename]) in ";
       "             let () = Utilities.closecon conn in";
       "             (match last_index with";
       "              | None -> Core.Result.Ok ()";
       "              | Some i ->";
       "                 let balance_of_records = Core.List.sub records ~pos:i ~len:(count-i) in";
       "                 save2db ~records:balance_of_records";
       "	     )";
       "	  | None ->";
       "             let () = Utilities.print_n_flush";
       "	                (String.concat [\"\\nNone affected; failed to insert new \\ ";
       "                                        records in \";tablename]) in";
       "	     let () = Utilities.closecon conn in";
       "             Core.Result.Error (String.concat [\"\\nNone affected; failed to insert \\ ";
       "				           new records in \";tablename])";
       "         )"; 
       "      | StatusEmpty";
       "      | StatusError _ ->";
       "         let () = Utilities.print_n_flush";
       "	            (String.concat [\"\\nEmpty result; failed to insert \\ ";
       "				     new records into \";tablename]) in";
       "         let () = Utilities.closecon conn in";
       "         Core.Result.Error";
       "	   (String.concat [\"\\nEmpty result; failed to insert new \\ ";
       "	  	           records into \";tablename])";
       "    else";
       "      (*--nothing to do with empty list here--*)";
       "      let () = Utilities.print_n_flush";
       "                \"\\n"^table_name^"::save2db() \\ ";
       "	         Empty list of type t; nothing to do; not saving anything to db.\" in ";
       "      Core.Result.Ok ();;\n"];;

  let construct_body ~table_name ~map ~ppx_decorators ~fields2ignore ~comparable_modules ~allcomparable
		     ~host ~user ~password ~database =
    let open Core in
    let module_first_char = String.get table_name 0 in
    let uppercased_first_char = Char.uppercase module_first_char in
    let module_name = Bytes.of_string table_name in
    let make_module_comparable =
      (allcomparable || (Core.List.mem comparable_modules ~equal:Core.String.equal table_name)) in
    let are_using_make_ppx = Core.List.mem ppx_decorators "make" ~equal:String.equal in
    let () = Bytes.set module_name 0 uppercased_first_char in
    let prefix_notice =
      "(*Auto-generated module; any edits would be overwritten and lost at build time \n \
       without revision control or without disabling the generation of this code at \n \
       build time. It might be better to include this module in another module in a different \n \
       directory. *)\n\n" in 
    let start_module =
      String.concat [prefix_notice;"module ";(Bytes.to_string module_name);
		       " = struct\n"] in
    let other_modules = list_other_modules () in
    let indentation = "  " in 
    let start_type_t =
      if make_module_comparable then
	Core.String.concat [indentation;"module T = struct\n";indentation;indentation;"type t = {"]
      else 
	Core.String.concat [indentation;"type t = {"] in
    let end_type_t =
      if make_module_comparable then
	Core.String.concat [indentation;indentation;"}"]
      else 
	Core.String.concat [indentation;"}"] in
    let fields_to_omit =
      match fields2ignore with
      | None -> []
      | Some l -> l in 
    (*Supply only keys that exist else find_exn will fail.*)
    let tfields_list_reversed = String.Map.find_exn map table_name in
    (*filter the list of fields against any that user indicated should be excluded*)
    let tfields_list =
      let tfields_list_temp = List.rev tfields_list_reversed in
      List.filter tfields_list_temp
		  ~f:(fun x ->
		      let matches =
			List.count fields_to_omit
				   ~f:(fun colname2omit -> String.equal x.col_name colname2omit) in 
		      matches = 0 
		     ) in 
    (*let () = Utilities.print_n_flush
	       (String.concat ["\nList of fields found of length:";
			       (Int.to_string (List.length tfields_list))]) in *)
    let rec helper l tbody =
      match l with
      | [] -> tbody
      | h :: t ->	 
	 let string_of_data_type =
	   Types_we_emit.to_string ~t:h.data_type ~is_nullable:h.is_nullable in
	 let total_indent =
	   if make_module_comparable then
	     Core.String.concat [indentation;indentation;indentation]
	   else Core.String.concat [indentation;indentation] in 
	 let tbody_new =
	   (*add ppx directives on a per field basis in ml file HERE*)
	   (*make the use of ppx make easier with default None for all optional fields*)
	   (*If Comparable, add [@compare fun x y -> 0] for all fields
             that are NOT the table key.*)
	   (match h.is_nullable, are_using_make_ppx, make_module_comparable, h.is_primary_key with 
	    | true, true, false, _ ->
	       Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				   string_of_data_type;" [@default None]";";"]
	    | true, true, true, false ->
	       Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				   string_of_data_type;" [@compare fun _x _y -> 0][@default None]";";"]
	    | true, true, true, true ->
	       Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				   string_of_data_type;" [@default None]";";"]
	    | true, false, true, false ->
	       Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				   string_of_data_type;" [@compare fun _x _y -> 0]";";"]
	    | true, false, true, true 
	    | true, false, false, _ 
	    | false, _, false, _ ->
	       Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				   string_of_data_type;";"]
	    | false, _, true, false ->
	       Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				   string_of_data_type;" [@compare fun _x _y -> 0]";";"]
	    | false, _, true, true ->
	       Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				   string_of_data_type;";"]
	   ) in 
	   (*if h.is_nullable &&
		(Core.List.mem ppx_decorators "make" ~equal:String.equal) then
	     Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				 string_of_data_type;" [@default None]";";"]
	   else 
	     Core.String.concat [tbody;"\n";total_indent;h.col_name;" : ";
				 string_of_data_type;";"] in*)
	 helper t tbody_new in 
    let tbody = helper tfields_list "" in
    let almost_done =
      Core.String.concat [other_modules;start_module;start_type_t;
			      tbody;"\n";end_type_t] in
    let finished_type_t =
      match ppx_decorators with
      | [] ->
	 if make_module_comparable then
	   String.concat [almost_done;"end";"  include T";"  module T2 = Core.Comparable.Make(T)"]
	 else
	   String.concat [almost_done;"end"]
      | _h :: _t ->
	 let ppx_extensions = String.concat ~sep:"," ppx_decorators in
	 if make_module_comparable then
	   String.concat [almost_done;" [@@deriving ";ppx_extensions;"]";"\n  end\n";
			  "\n  include T";"\n  module T2 = Core.Comparable.Make(T)\n"]
	 else 
	   String.concat [almost_done;" [@@deriving ";ppx_extensions;"]\n"] in
    (*Insert a few functions and variables.*)
    let table_related_lines =
      String.concat ["  let tablename=\"";table_name;
		     "\" \n\n  let get_tablename () = tablename;;\n"] in
    (*General purpose query...client code can create others*)
    let sql_query_function =
      Core.String.concat ["  let get_sql_query () = \n";
		     "    let open Core in\n";
		     "    let fs = Fields.names in \n";
		     "    let fs_csv = String.concat ~sep:\",\" fs in \n";
		     "    String.concat [\"SELECT \";fs_csv;\" FROM \";tablename;\" WHERE TRUE;\"];;\n"] in
    let query_function = construct_sql_query_function ~table_name ~fields_list:tfields_list ~host
						      ~user ~password ~database in
    (*Saving records to SQL would also be useful*)
    let toSQLfunction =
      construct_sql_serialize_function ~fields_list:tfields_list in
    let insert_prefix =
      String.concat
	["  let get_sql_insert_statement () =\n";
	 "    let fs = Fields.names in\n";
	 "    let csv_fields = Core.String.concat fs ~sep:\",\" in\n";
	 "    Core.String.concat [\"INSERT INTO \";tablename;\" (\";\n";
	 "                        csv_fields;\") VALUES \"];;\n";
	] in
    let generate_values_of_list =
      String.concat
	["  let generate_values_for_sql_of_list ~records ~conn =\n";
	 "    let rec helper l acc = \n";
	 "      match l with\n";
	 "      | [] -> Core.String.concat ~sep:\",\" acc\n";
	 "      | h :: t -> \n";
	 "         let one_record_values = generateSQLvalue_for_insert h conn in\n";
	 "         helper t (one_record_values::acc) in\n";
	 "    helper records [];;\n"
	] in
    let duplicate_clause = duplicate_clause_function ~fields:tfields_list () in
    let save_function = construct_save_function ~table_name in
    String.concat ~sep:"\n" [finished_type_t;table_related_lines;sql_query_function;
			     query_function;insert_prefix;duplicate_clause;toSQLfunction;
			     generate_values_of_list;save_function;"end"];;
    
  let construct_mli ~table_name ~map ~ppx_decorators ~fields2ignore ~comparable_modules ~allcomparable =
    let open Core in
    let module_first_char = String.get table_name 0 in
    let uppercased_first_char = Char.uppercase module_first_char in
    let module_name = Bytes.of_string table_name in
    let () = Bytes.set module_name 0 uppercased_first_char in
    let other_modules = list_other_modules_for_mli () in 
    let start_module = String.concat ["module ";(Bytes.to_string module_name);" : sig \n"] in 
    let start_type_t = "  type t = {" in
    let end_type_t = "  }" in
    let fields_to_omit =
      match fields2ignore with
      | None -> []
      | Some l -> l in 
    (*Supply only keys that exist else find_exn will fail.*)
    let tfields_list_reversed = String.Map.find_exn map table_name in
    (*filter the list of fields against any that user indicated should be excluded*)
    let tfields_list =
      let tfields_list_temp = List.rev tfields_list_reversed in
      List.filter tfields_list_temp
		  ~f:(fun x ->
		      let matches =
			List.count fields_to_omit
				   ~f:(fun colname2omit -> String.equal x.col_name colname2omit) in 
		      matches = 0 
		     ) in 
(*    let () = Utilities.print_n_flush
	       (String.concat ["\nList of fields found of length:";
			       (Int.to_string (List.length tfields_list))]) in *)
    let rec helper l tbody =
      match l with
      | [] -> tbody
      | h :: t ->
	 let string_of_data_type =
	   Types_we_emit.to_string ~t:h.data_type ~is_nullable:h.is_nullable in 
	 let tbody_new =
	   (*add ppx directives on a per field basis in ml file HERE*)
	   (*make the use of make easier with default None for all optional fields*)
	   if h.is_nullable && 
		(Core.List.mem ppx_decorators "make" ~equal:String.equal) then
	     Core.String.concat [tbody;"\n    ";h.col_name;" : ";
				 string_of_data_type;" [@default None]";";"]
	   else 
	     Core.String.concat [tbody;"\n    ";h.col_name;" : ";
				 string_of_data_type;";"] in
	 helper t tbody_new in 
    let tbody = helper tfields_list "" in
    let almost_done = String.concat [other_modules;start_module;start_type_t;tbody;"\n";end_type_t] in
    let with_ppx_decorators = 
      match ppx_decorators with
      | [] -> String.concat [almost_done;"end"]
      | _h :: _t ->
	 let ppx_extensions = String.concat ~sep:"," ppx_decorators in
	 String.concat [almost_done;" [@@deriving ";ppx_extensions;"]\n"] in
    let function_lines =
      String.concat
	~sep:"\n"
	["  val get_tablename : unit -> string";
	 "  val get_sql_query : unit -> string";
	 "  val get_sql_insert_statement : unit -> string";
	 "  val get_sql_insert_on_duplicate_clause : unit -> string";
	 "  val get_from_db : query:string -> (t list, string) Core.Result.t";
	 "  val generateSQLvalue_for_insert : t -> Mysql.dbd -> string";
	 "  val generate_values_for_sql_of_list : records:t list -> conn:Mysql.dbd -> Core.String.t";
	 "  val save2db : records: t list -> (unit, string) Core.Result.t";
	 "end"] in
    if (allcomparable || (Core.List.mem comparable_modules ~equal:Core.String.equal table_name)) then 
      String.concat ~sep:"\n" [with_ppx_decorators;"  module T2 : sig ";
			       "    include Core.Comparable.S with type t := t";"  end\n";function_lines]
    else
      String.concat ~sep:"\n" [with_ppx_decorators;function_lines]

  (*Intention is for invocation from root dir of a project from Make file. 
    In which case current directory sits atop src and build subdirs.*)
  let write_module ~outputdir ~fname ~body = 
    let open Core.Unix in
    let myf sbuf fd = single_write fd ~buf:sbuf in
    let check_or_create_dir ~dir =
      try 
	let _stats = stat dir in ()	
      with _ ->
	mkdir ~perm:0o774 dir in
    try
      let () = check_or_create_dir ~dir:outputdir in 
      let _bytes_written =
	with_file (Core.String.concat [outputdir;fname]) ~mode:[O_RDWR;O_CREAT;O_TRUNC]
		  ~perm:0o664 ~f:(myf body) in ()
    with err -> Utilities.print_n_flush (Core.String.concat ["\nFailed to write to file:";(Core.Exn.to_string err)])

  (*NOT USED YET -- do NOT use while testing in place else we'll overwrite our own version.
    ON SECOND THOUGHT -- NO NEED TO ever do this...once this is an installed package, just
    use the package maintained utilities file or else include and extend it.
  let copy_utilities ~destinationdir =
    let open Core in 
    let open Core.Unix in
    (*--how to specify the (opam install) path to utilities.ml---most likely 
     would need to use ocamlfind query <thispackagename> just to get the directory.*)
    let r = system (String.concat ["cp src/lib/utilities.ml ";destinationdir]) in
    let result = Core.Unix.Exit_or_signal.to_string_hum r in 
    let () = Utilities.print_n_flush result in 
    match r with
    | Result.Ok () -> Utilities.print_n_flush "\nCopied the utilities file."
    | Error _e -> Utilities.print_n_flush "\nFailed to copy the utilities file." *)

  let write_appending_module ~outputdir ~fname ~body =
    let open Unix in
    let myf sbuf fd = single_write fd (Bytes.of_string sbuf) 0 (Core.String.length sbuf) in
    let check_or_create_dir ~dir =
      try 
        let _stats = stat dir in ()     
      with _ ->
        mkdir dir 0o770 in
    try
      let () = check_or_create_dir ~dir:outputdir in
      let name = (Core.String.concat [outputdir;fname]) in
      let f = openfile name [O_RDWR;O_CREAT;O_APPEND] 0o664 in
      let _bytes_written = myf body f in
      close f
    with _ -> Utilities.print_n_flush (Core.String.concat ["\nFailed to write (appending) to file:";fname])
      
end
