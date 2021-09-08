open Adornment2
open Program
open Datalog_signature
open Prefix_correct_program
open Program_printer
open Magic_set_rewritting2
open Pmcfg
open Oriented_pmcfg
open Pmcfg_to_datalog

module Kanazawa_transform = 
  struct
    let is_impermissible n= 
      let rec check adornment =
        match adornment with 
            [] -> false
          | false::true::_ -> true
          | _::_::adornment -> check adornment
          | _::[] -> false
      in
      function Adornment.AdP(k,adornment) ->
        k<n && (check adornment)

    let is_impermissible2 n= 
      let rec check has_bound_var adornment =
        match adornment with 
            [] -> has_bound_var
          | false::true::_ -> true
          | true::_::adornment -> check true adornment
          | false::false::adornment -> check has_bound_var adornment
          | _::[] -> true
      in
      function Adornment.AdP(k,adornment) ->
        k<n && (check false adornment)
        
    (*let is_impermissible3 = 
      let rec check adornment =
        match adornment with 
            [] -> false
          | false::true::_ -> true
          | false::false::_ -> true
          | _::_::adornment -> check adornment
          | _::[] -> false
      in
      function Adornment.AdP(k,adornment) -> (check adornment)*)

    let exceptions name =
      try
        String.sub name 0 2 = "eq"
      with Invalid_argument _ -> false

    let transform_program program name= 
      let Program.Prog(Datalog_signature.S(l,_,neq) as sign,_) = program in
      let n = List.length l in
      let program = Prefix_correct_program.prefix_correct_transform exceptions program in
      let (_,id) = Datalog_signature.find_pred_of_name name sign in
      let goal = Adornment.AdP(id,[true;false]) in
      let ad_program = 
        Adornment.adorn_program 
          program
          goal 
          (is_impermissible n)
          (Adornment.compare_adornments3 neq)
      in
      let _ =
        print_string
          (Program_printer.print_program 
              (Adornment.program_of_adorned_program ad_program)
          ) 
      in
      let (map,prog) = Magic_set_rewritting.magic ad_program in
        (Magic_set_rewritting.Adorned_predicate_map.find goal map,prog)


    let transform_pmcfg_0 grammar =
      let program = 
        PMCFG.naive_program_of_pmcfg (Oriented_pmcfg.orient_grammar grammar)
      in
      let Program.Prog(Datalog_signature.S (l,_,neq),_) = program in
      let n = List.length l in
      let program = Prefix_correct_program.prefix_correct_transform exceptions program in
      let PMCFG.Grammar(_,_,init_id) = grammar in
      let goal = Adornment.AdP(init_id,[true;false]) in
      let ad_program = 
        Adornment.adorn_program 
          program
          goal 
          (is_impermissible n)
          (Adornment.compare_adornments3 neq)
      in
      let _ =
        print_string
          (Program_printer.print_program 
              (Adornment.program_of_adorned_program ad_program)
          ) 
      in
      let (map,prog) = Magic_set_rewritting.magic ad_program in
      (Magic_set_rewritting.Adorned_predicate_map.find goal map,prog)

    let transform_pmcfg_1 grammar =
      let program = 
        PMCFG.program_of_pmcfg (Oriented_pmcfg.orient_grammar grammar)
      in
      let Program.Prog(Datalog_signature.S (l,_,neq),_) = program in
      let n = List.length l in
      let program = Prefix_correct_program.prefix_correct_transform exceptions program in
      let PMCFG.Grammar(_,_,init_id) = grammar in
      let goal = Adornment.AdP(init_id,[true;false]) in
      let ad_program = 
        Adornment.adorn_program 
          program
          goal 
          (is_impermissible n)
          (Adornment.compare_adornments3 neq)
      in
      let _ =
        print_string
          (Program_printer.print_program 
              (Adornment.program_of_adorned_program ad_program)
          ) 
      in
      let (map,prog) = Magic_set_rewritting.magic ad_program in
      (Magic_set_rewritting.Adorned_predicate_map.find goal map,prog)

    let transform_pmcfg_2 grammar =
      let program = 
        PMCFG_to_datalog.get_program (Oriented_pmcfg.orient_grammar grammar)
      in
      let Program.Prog(Datalog_signature.S (l,_,neq),_) = program in
      let n = List.length l in
      let program = Prefix_correct_program.prefix_correct_transform exceptions program in
      let PMCFG.Grammar(_,_,init_id) = grammar in
      let goal = Adornment.AdP(init_id,[true;false]) in
      let ad_program = 
        Adornment.adorn_program 
          program
          goal 
          (is_impermissible n)
          (Adornment.compare_adornments3 neq)
      in
      let _ =
        print_string
          (Program_printer.print_program 
              (Adornment.program_of_adorned_program ad_program)
          ) 
      in
      let (map,prog) = Magic_set_rewritting.magic ad_program in
      (Magic_set_rewritting.Adorned_predicate_map.find goal map,prog)       
        
  end
