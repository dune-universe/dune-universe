open DatalogLib.Datalog_AbstractSyntax

(*module Store = (*: UnionFind.Store with type 'a t ='a PersistentArray.PersistentArray.t *)
struct 
(*  module type PA_SIG=module type of PersistentArray.PersistentArray*)
  include PersistentArray.PersistentArray (*: PA_SIG (*with type 'a t = ConstGen.id PersistentArray.PersistentArray.t*)*)
  let empty i =
  let value,_ = ConstGen.get_fresh_id (ConstGen.init ()) in
  init i (fun _ -> value)
  end
*)

module Store = DatalogLib.UnionFind.StoreAsMap
(*struct 
  include PersistentArray.PersistentArray
  let empty i =
    let value,_ = ConstGen.get_fresh_id (ConstGen.init ()) in
    init i (fun _ -> value)
end
*)


module Datalog=DatalogLib.Datalog.Make(Store)

let parse_file query edb filename =
  let in_ch = 
    let fullname = UtilsLib.Utils.find_file filename [""]  in
    open_in fullname in
  let lexbuf = Lexing.from_channel in_ch in
  Logs.app (fun m -> m "Parsing \"%s\"..." filename);
  let prog=DatalogLib.Db_parser.program DatalogLib.Db_lexer.lexer lexbuf AbstractSyntax.Proto_Program.empty in 
  Logs.app (fun m -> m "Done.");
  Logs.debug (fun m -> m "Current symbol tables:");
  AbstractSyntax.Predicate.PredIdTable.log_content Logs.Debug prog.AbstractSyntax.Proto_Program.pred_table;
  let sep=String.make 15 '*' in
  Logs.app (fun m -> m "%s%!" sep);
  Logs.app (fun m -> m "Create the abstract program and print it...");
  let abs_program = AbstractSyntax.Program.make_program prog in
  let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer abs_program) in
  Logs.app (fun m -> m "Done.");
  Logs.app (fun m -> m "%s" sep);
  Logs.app (fun m -> m "Create the internal program and print it...");
  let program=Datalog.Program.make_program abs_program in 
  let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract program)) in 
  Logs.app (fun m -> m "Done.");
  Logs.app (fun m -> m "%s" sep);
  let program =
    match edb with
    | None ->
	Logs.debug (fun m -> m "I didn't find an edb file to parse.");
       program
    | Some edb_filename ->
      Logs.debug (fun m -> m "I found an edb file to parse.");
      let edb_in_ch = 
	let edb_fullname = UtilsLib.Utils.find_file edb_filename [""]  in
	open_in edb_fullname in
      let edb_lexbuf = Lexing.from_channel edb_in_ch in
	Logs.app (fun m -> m "Parsing \"%s\"..." edb_filename);
      let to_be_added=DatalogLib.Db_parser.extensional_facts DatalogLib.Db_lexer.lexer edb_lexbuf Datalog.Program.(program.pred_table,program.const_table,program.rule_id_gen) in 
      Logs.app (fun m -> m "Done.");
       Datalog.Program.add_e_facts program to_be_added in
  let derived_facts,derivations = Datalog.Program.seminaive program in
  Logs.app (fun m -> m "I could derive the following facts:\n%s" (Datalog.Predicate.facts_to_string derived_facts program.Datalog.Program.pred_table program.Datalog.Program.const_table));
  let buff = Buffer.create 80 in
  let () = Datalog.Predicate.add_map_to_premises_to_buffer buff program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations in
  Logs.app (fun m -> m "With the following derivations:\n%s\n" (Buffer.contents buff));
  let query,_new_pred_table,_new_cons_table =
    match query with
    | None -> 
       None,program.Datalog.Program.pred_table,program.Datalog.Program.const_table
    | Some s -> 
       let q,t1,t2=DatalogLib.Db_parser.query DatalogLib.Db_lexer.lexer (Lexing.from_string s) Datalog.Program.(program.pred_table,program.const_table) in 
       Some q,t1,t2 in
  let () = Datalog.Predicate.format_derivations2 ?query:query program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations in
  Logs.app (fun m -> m "%s" (Buffer.contents (Format.stdbuf)));
  
  ()


let main query edb filename =
  match DatalogLib.Dl_parse_functions.parse_program filename with
  | None -> ()
  | Some p -> 
     let prog= p AbstractSyntax.Proto_Program.empty in
     Logs.debug (fun m -> m "Current symbol tables:");
     AbstractSyntax.Predicate.PredIdTable.log_content Logs.Debug prog.AbstractSyntax.Proto_Program.pred_table;
     let sep=String.make 15 '*' in
     Logs.app (fun m -> m "%s%!" sep);
     Logs.app (fun m -> m "Create the abstract program and print it...");
     let abs_program = AbstractSyntax.Program.make_program prog in
     let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer abs_program) in
     Logs.app (fun m -> m "Done.");
     Logs.app (fun m -> m "%s" sep);
     Logs.app (fun m -> m "Create the internal program and print it...");
     let program=Datalog.Program.make_program abs_program in 
     let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract program)) in 
     Logs.app (fun m -> m "Done.");
     Logs.app (fun m -> m "%s" sep);
     let program =
       match edb with
       | None ->
          let () = Logs.debug (fun m -> m "I didn't find an edb file to parse.") in
          program
       | Some edb_filename ->
          let () = Logs.debug (fun m -> m "I found an edb file to parse.") in
          let to_be_added=DatalogLib.Dl_parse_functions.parse_edb edb_filename in
          match to_be_added with
          | None -> program
          | Some add -> Datalog.Program.add_e_facts program (add Datalog.Program.(program.pred_table,program.const_table,program.rule_id_gen)) in
     let derived_facts,derivations = Datalog.Program.seminaive program in
     Logs.app (fun m -> m "I could derive the following facts:\n%s" (Datalog.Predicate.facts_to_string derived_facts program.Datalog.Program.pred_table program.Datalog.Program.const_table));
     let buff = Buffer.create 80 in
     let () = Datalog.Predicate.add_map_to_premises_to_buffer buff program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations in
     Logs.app (fun m -> m "With the following derivations:\n%s\n" (Buffer.contents buff));
     Logs.app (fun m -> m "%s" (Buffer.contents (Format.stdbuf)));
     match query with
     | None ->
        Datalog.Predicate.format_derivations2 program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations
     | Some s ->
        let as_q = DatalogLib.Dl_parse_functions.parse_query s in
        match as_q with
        | None -> ()
        | Some pred ->
           let () =  Logs.app (fun m -> m "Facts (and their derivations) matching the query \"%s\":\n" s) in
           let q,_t1,_t2= pred Datalog.Program.(program.pred_table,program.const_table) in
           Datalog.Predicate.format_derivations2 ~query:q program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations

let usage_msg="Usage: db_test [-edb edb_file] file"
let edb_file=ref None
let query=ref None

let options =
  [
    ("-edb",Arg.String (fun s -> edb_file:=Some s),"Add the specified file as an edb (it should include only extensional facts).");
    ("-q",Arg.String (fun s -> query:=Some s),"Only outputs the derivations satisfying the specified query")
  ]

let () =
  let () = UtilsLib.Log.set_level ~app:"db_test" Logs.Warning in
  (*  let () = UtilsLib.Log.set_level ~app:"db_test" Logs.Debug in *)
  if Array.length (Sys.argv) > 1 then
    (* Arg.parse options (fun s -> parse_file !query !edb_file s) usage_msg *)
    Arg.parse options (fun s -> main !query !edb_file s) usage_msg
  else parse_file None None "/home/pogodall/work/dev/ACGtk/src/datalog.prover/Jean-regarde-telescope.dl"
