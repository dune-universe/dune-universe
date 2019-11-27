let _ =
  Format.printf "succes code is %d@." Exit.success_code ;
  Format.printf "failure code is %d@." Exit.failure_code ;
  if Exit.success_code <> 0 || Exit.failure_code <> 1 then (
    Format.printf "not POSIX !@." ;
    Exit.failure () ) ;
  Exit.success ()
