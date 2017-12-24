open UtilsLib.IdGenerator
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
  LOG "Parsing \"%s\"..." filename LEVEL INFO;
  let prog=DatalogLib.Db_parser.program DatalogLib.Db_lexer.lexer lexbuf AbstractSyntax.Proto_Program.empty in 
  LOG "Done." LEVEL INFO;
  LOG "Current symbol tables:" LEVEL DEBUG ;
  UtilsLib.Utils.log_iteration
    (fun s -> LOG s LEVEL DEBUG)
    (AbstractSyntax.Predicate.PredIdTable.to_string prog.AbstractSyntax.Proto_Program.pred_table);
  let sep=String.make 15 '*' in
  let () = Printf.printf "%s\n%!" sep in
  let () = Printf.printf "Create the abstract program and print it...\n" in
  let abs_program = AbstractSyntax.Program.make_program prog in
  let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer abs_program) in
  let () = Printf.printf "Done.\n" in
  let () = Printf.printf "%s\n" sep in
  let () = Printf.printf "Create the internal program and print it...\n" in
  let program=Datalog.Program.make_program abs_program in 
  let () = Buffer.output_buffer stdout (AbstractSyntax.Program.to_buffer (Datalog.Program.to_abstract program)) in 
  let () = Printf.printf "Done.\n" in
  let () = Printf.printf "%s\n" sep in
  let program =
    match edb with
    | None -> 
	LOG "I didn't find an edb file to parse." LEVEL DEBUG ;
      program
    | Some edb_filename ->
      LOG "I found an edb file to parse." LEVEL DEBUG ;
      let edb_in_ch = 
	let edb_fullname = UtilsLib.Utils.find_file edb_filename [""]  in
	open_in edb_fullname in
      let edb_lexbuf = Lexing.from_channel edb_in_ch in
	LOG "Parsing \"%s\"..." edb_filename LEVEL INFO;
      let to_be_added=DatalogLib.Db_parser.extensional_facts DatalogLib.Db_lexer.lexer edb_lexbuf Datalog.Program.(program.pred_table,program.const_table,program.rule_id_gen) in 
      LOG "Done." LEVEL INFO;
      Datalog.Program.add_e_facts program to_be_added in
  let derived_facts,derivations = Datalog.Program.seminaive program in
  let () = Printf.printf "I could derive the following facts:\n%s\n" (Datalog.Predicate.facts_to_string derived_facts program.Datalog.Program.pred_table program.Datalog.Program.const_table) in
  let buff = Buffer.create 80 in
  let () = Datalog.Predicate.add_map_to_premises_to_buffer buff program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations in
  let () = Printf.printf "With the following derivations:\n%s\n" (Buffer.contents buff) in
  let query,new_pred_table,new_cons_table =
    match query with
    | None -> 
      None,program.Datalog.Program.pred_table,program.Datalog.Program.const_table
    | Some s -> 
      let q,t1,t2=DatalogLib.Db_parser.query DatalogLib.Db_lexer.lexer (Lexing.from_string s) Datalog.Program.(program.pred_table,program.const_table) in 
      Some q,t1,t2 in
  let () = Datalog.Predicate.format_derivations2 ?query:query program.Datalog.Program.pred_table program.Datalog.Program.const_table derivations in
  let () = Printf.printf "%s\n" (Buffer.contents (Format.stdbuf)) in
  
  ()
      

let usage_msg="Usage: db_test [-edb edb_file] file"
let edb_file=ref None
let query=ref None

let options =
  [
    ("-edb",Arg.String (fun s -> edb_file:=Some s),"Add the specified file as an edb (it should include only extensional facts).");
    ("-q",Arg.String (fun s -> query:=Some s),"Only outputs the derivations satisfyin the specified query")
  ]

let () =
  Arg.parse options (fun s -> parse_file !query !edb_file s) usage_msg 
