(*#use "signature.ml";;
#use "string_map.ml";;
#use "int_map.ml";;
#use "int_set.ml";;
#use "program.ml";;
#use "pmcfg.ml";;
#use "pmcfg_to_datalog.ml";;
#use "oriented_pmcfg.ml";;
#use "lexicalizer.ml";;
#use "pmcfg_syn.ml";;
#use "program_printer.ml";;
#use "prefix_correct_program.ml";;
#use "adornment2.ml";;
#use "magic_set_rewritting2.ml";;
#use "kanazawa_transform.ml";;
#use "datalog_solver.ml";;*)

(*#load  "signature.cmo";;
#load  "string_map.cmo";;
#load  "int_map.cmo";;
#load  "int_set.cmo";;
#load  "program.cmo";;
#load  "pmcfg.cmo";;
#load  "pmcfg_to_datalog.cmo";;
#load  "oriented_pmcfg.cmo";;
#load  "lexicalizer.cmo";;
#load  "pmcfg_syn.cmo";;
#load  "program_printer.cmo";;
#load  "prefix_correct_program.cmo";;
#load  "adornment2.cmo";;
#load  "magic_set_rewritting2.cmo";;
#load  "kanazawa_transform.cmo";;
#load  "datalog_solver.cmo";;
*)

open Pmcfg_syn
open Kanazawa_transform
open Program_printer
open Datalog_solver

(*opening the file containing the grammar, here gram.pmcfg*)
let ch_in = open_in "gram.pmcfg";;
(*parsing the grammar*)
let grammar = PMCFG_syn.parse (Stream.of_channel ch_in);;
(*transforming the grammar *)
(*let ((start,magic),program) = Kanazawa_transform.transform_pmcfg_1 is for simple equality*)
(*let ((start,magic),program)Kanazawa_transform.transform_pmcfg_2 is for structural equality*)
(*let ((start,magic),program) = Kanazawa_transform.transform_pmcfg_0 grammar;;*)
(*let ((start,magic),program) = Kanazawa_transform.transform_pmcfg_1 grammar;;*)
let ((start,magic),program) = Kanazawa_transform.transform_pmcfg_2 grammar;;
(*printing the resulting datalog program*)
print_string (Program_printer.print_program program);;
(*sentence to analyse as a list of extensional predicate*)
(*the list [a1;...;an] is translated in the extensional db a1(0,1),...,an(n-1,n)*)
(*NB: ["ab"] is different from ["a";"b"]*)
let sentence = ["a";"a";"a";"a";"b";"b";"b"];;
(*let sentence = ["a";"a";"a";"a";"a";"b";"b";"b"];;*)
(*analysing sentence*)
let mem = Datalog_solver.solve2 program sentence magic;;
(*presenting the chart in a stepwise fashion*)
let res = Datalog_solver.print_chart mem;;
