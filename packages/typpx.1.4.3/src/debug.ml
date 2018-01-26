let debug = match Sys.getenv "TYPPX_DEBUG" with
  | _ -> true
  | exception Not_found -> false
      
