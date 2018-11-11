open Earley_core
open Earley
let gamma3 = Earley.declare_grammar "gamma3" 
let _ =
  Earley.set_grammar gamma3
    (Earley.alternatives
       [Earley.fsequence gamma3
          (Earley.fsequence gamma3
             (Earley.fsequence gamma3
                (Earley.empty
                   (fun _default_0  ->
                      fun _default_1  -> fun _default_2  -> ()))));
       Earley.fsequence_ignore (Earley.char 'a' 'a') (Earley.empty ());
       Earley.fsequence gamma3
         (Earley.fsequence gamma3
            (Earley.empty (fun _default_0  -> fun _default_1  -> ())))])
  
let n = int_of_string (Sys.argv.(1)) 
let input = String.make n 'a' 
let _ = Earley.debug_lvl := 0; Earley.warn_merge := false 
let _ = parse_string gamma3 no_blank input 
