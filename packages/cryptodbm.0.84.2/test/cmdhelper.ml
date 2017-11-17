open Cryptodbm.Error

type data = 
  | Rtable of Cryptodbm.read Cryptodbm.table
  | Ftable of Cryptodbm.full Cryptodbm.table
  | RSub of Cryptodbm.read Cryptodbm.subtable
  | FSub of Cryptodbm.full Cryptodbm.subtable

let new_env () = Hashtbl.create 10

let assign env var data = 
  Hashtbl.replace env var data ; 
  Printf.printf "%s is defined.\n" var ;
  env

let get env var =
  try Hashtbl.find env var
  with Not_found -> failwith (var ^ " is unbound.")

let optional s = if s = "" then None else Some s

let builtin word f =
  fun env l -> if l = word then (f () ; env) else raise (Scanf.Scan_failure "")

let rec exec env l = function
  | [] ->
      Printf.printf "Sorry, this command is not recognized. Try again.\n%!" ;
      env

  | com :: coms ->
      begin
	try
	  let env' = com env l in
	  Printf.printf "Done.\n%!" ;
	  env'
	with
	| Scanf.Scan_failure _ | End_of_file -> exec env l coms
	| Error e ->
	    Printf.printf "*Error*: %s\n\n%!" (error2s e) ;
	    env
	| Failure msg ->
	    Printf.printf "%s\n%!" msg ;
	    env
      end

let to_bool = function
  | "true" -> true
  | "false" -> false
  | s -> failwith ("Not a boolean: " ^ s ^ ". Try 'true' or 'false'")

let to_int x = try int_of_string x with _ -> failwith ("Not an integer : " ^ x)

let to_obool s = if s = "" then None else Some (to_bool s)
let to_oint s = if s = "" then None else Some (to_int s)
