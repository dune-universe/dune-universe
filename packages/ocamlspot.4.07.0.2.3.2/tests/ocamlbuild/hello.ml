let _ =
  Printf.printf "Hello, %s ! My name is %s! %s!\n"
    (if Array.length Sys.argv > 1 then Sys.argv.(1) else "stranger")
    Sys.argv.(0)
    Bye.bye
;;
