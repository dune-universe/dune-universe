open RdbgArg;;

let _ = 
  Printf.printf "Set lurette mode\n"; flush stdout;
  args.rdbg <-false;;


#use "test.ml";;
