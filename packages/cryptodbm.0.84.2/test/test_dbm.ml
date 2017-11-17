(* Read the preamble in test_dbm_helper.ml *)

open Test_dbm_helper
open Exenum

(* Enumerations for the different elements of a config. *)
let e_fake_random = from_list [ 999 ]

let passwd_chars = ['a' ; '\000']

let e_passwd = e_rstring passwd_chars
let e_signwd = e_passwd
let e_max_extra_key = from_list [ 0 ; 10 ]
(* let e_max_extra_data = from_list [ 0 ; 1 ; 2 ; 15 ; 200 ] *)
let e_max_extra_bindings = from_list [ 0 ; 400 ]

let e_flush = from_list [ 0 ; 3]
let e_append = from_list [ 0 ; 3]

let e_crypted = e_option e_passwd
let e_signed = e_passwd
let e_max_key_pad = e_option e_max_extra_key
(* let e_max_data_pad = e_option e_max_extra_key *)

let e_sub_append = from_list [ 0 ; 2]
let e_parasite = from_list [ 0 ; 10 ]

let e_keys = e_rstring passwd_chars
let e_data = e_keys

let e_content = e_list (pair e_keys e_data)

let e_iterations = from_list [ None ; Some 0 ; Some 1 ; Some 100 ; Some 10000 ]

let e_subtable_tuple =
  pair (pair (pair e_crypted e_iterations) (pair e_signed e_max_key_pad)) (triple e_content e_sub_append e_parasite)

let e_subtable_config =
  map e_subtable_tuple
    begin fun ((((crypted, st_iterations), (signed, max_key_pad)), (content, sub_append_nb, parasite)) as tuple) ->
      let name = Printf.sprintf "subt#%d-" (Hashtbl.hash tuple) in
      { name ;
	crypted ;
	signed ;
        st_iterations ;
	max_key_pad ;
	max_data_pad = max_key_pad ;
	content ;
	sub_append_nb ;
	parasite }
    end

let e_table_tuple =
  triple 
    (triple e_fake_random e_passwd e_signwd) 
    (triple e_max_extra_key e_max_extra_bindings e_iterations) 
    (triple (e_list e_subtable_config) e_flush e_append)
  
let e_table_config =
  map e_table_tuple
    begin fun ((random, passwd, signwd), 
	       (max_extra_key, max_extra_bindings, iterations), 
	       (subtables, flush_nb, append_nb)) ->
      { filename = "" ;
	random ;
	passwd ;
	signwd ;
        iterations ;
	max_extra_key ;
	max_extra_data = max_extra_key ;
	max_extra_bindings ;
	subtables = Common.ifold subtables [] (fun i acu sub -> ({ sub with name = sub.name ^ (string_of_int i) }) :: acu) ;
	flush_nb ;
	append_nb }
    end

open Exenum_internals.Convenience

let () =

  if Array.length Sys.argv <> 2 then
    begin
      Printf.printf "Usage: %s  start-index\n" Sys.argv.(0) ;
      exit 1
    end ;
  
  let count = ref 0 in

  (* On ne veut que les nombres impairs, sauf 0. *)
  let start = int_of_string Sys.argv.(1) in
  let start = if start = 0 then 0 else start * 2 - 1 in

  let namesalt = string_of_int (Unix.getpid ()) in
  
  Exenum.tester e_table_config ~from:(boi (start*1000)) ~verbose_period:1 ~len:1000 
  begin fun tconf ->
    Printexc.print
      begin fun () ->
	let tconf = { tconf with filename = Printf.sprintf "/tmp/table%s-%d" namesalt !count } in
	incr count ;
	show_conf tconf ;
	run tconf ;
	Printf.printf "ok\n%!" ;
	
	if !count mod 10 = 0 then ignore (Sys.command "rm -rf /tmp/table-*") ;
	
      end ()
  end
    
