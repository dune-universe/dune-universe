open Earley_core
open Earley
let g = Earley.declare_grammar "g" 
let _ =
  Earley.set_grammar g
    (Earley.fsequence
       (Earley.apply (fun f  -> f [])
          (Earley.fixpoint' (fun l  -> l)
             (Earley.alternatives
                [Earley.fsequence_ignore (Earley.char '\r' '\r')
                   (Earley.empty ());
                Earley.fsequence_ignore (Earley.char ' ' ' ')
                  (Earley.empty ());
                Earley.fsequence_ignore (Earley.char '\n' '\n')
                  (Earley.empty ());
                Earley.fsequence_ignore (Earley.char '\t' '\t')
                  (Earley.empty ())])
             (fun x  -> fun f  -> fun l  -> f (x :: l))))
       (Earley.empty (fun _default_0  -> ())))
  
let blank = blank_grammar g no_blank 
let r = Earley.declare_grammar "r" 
let _ =
  Earley.set_grammar r
    (Earley.apply (fun f  -> f [])
       (Earley.fixpoint' (fun l  -> l)
          (Earley.alternatives
             [Earley.fsequence_ignore (Earley.char 'b' 'b') (Earley.empty ());
             Earley.fsequence_ignore (Earley.char 'a' 'a') (Earley.empty ())])
          (fun x  -> fun f  -> fun l  -> f (x :: l))))
  
let patocomment = Earley.declare_grammar "patocomment" 
let _ =
  Earley.set_grammar patocomment
    (change_layout
       (Earley.fsequence_ignore (Earley.string "(*" "(*")
          (Earley.fsequence_ignore
             (Earley.apply (fun f  -> f [])
                (Earley.fixpoint' (fun l  -> l)
                   (Earley.alternatives
                      [Earley.fsequence_ignore (Earley.char '\n' '\n')
                         (Earley.empty ());
                      Earley.fsequence_ignore
                        (Earley_str.regexp ~name:"[^*]\\\\|\\\\([*][^)]\\\\)"
                           "[^*]\\|\\([*][^)]\\)" (fun groupe  -> groupe 0))
                        (Earley.empty ())])
                   (fun x  -> fun f  -> fun l  -> f (x :: l))))
             (Earley.fsequence_ignore (Earley.string "*)" "*)")
                (Earley.empty ())))) no_blank)
  
let patocomments = Earley.declare_grammar "patocomments" 
let _ =
  Earley.set_grammar patocomments
    (Earley.fsequence_ignore
       (Earley.apply (fun f  -> f [])
          (Earley.fixpoint' (fun l  -> l) patocomment
             (fun x  -> fun f  -> fun l  -> f (x :: l)))) (Earley.empty ()))
  
let spaces = Earley.declare_grammar "spaces" 
let _ =
  Earley.set_grammar spaces
    (Earley.apply (fun f  -> f [])
       (Earley.fixpoint' (fun l  -> l)
          (Earley_str.regexp ~name:"[ \\t\\r]" "[ \t\r]"
             (fun groupe  -> groupe 0))
          (fun x  -> fun f  -> fun l  -> f (x :: l))))
  
let blank_grammar_sline = Earley.declare_grammar "blank_grammar_sline" 
let _ =
  Earley.set_grammar blank_grammar_sline
    (Earley.fsequence_ignore spaces
       (Earley.fsequence_ignore
          (Earley.option None
             (Earley.apply (fun x  -> Some x)
                (Earley.fsequence_ignore (Earley.char '\n' '\n')
                   (Earley.fsequence_ignore spaces (Earley.empty ())))))
          (Earley.empty ())))
  
let blank_grammar_mline = Earley.declare_grammar "blank_grammar_mline" 
let _ =
  Earley.set_grammar blank_grammar_mline
    (Earley.fsequence_ignore spaces
       (Earley.fsequence_ignore
          (Earley.apply (fun f  -> f [])
             (Earley.fixpoint' (fun l  -> l)
                (Earley.fsequence_ignore (Earley.char '\n' '\n')
                   (Earley.fsequence_ignore spaces (Earley.empty ())))
                (fun x  -> fun f  -> fun l  -> f (x :: l))))
          (Earley.empty ())))
  
let blank_sline = blank_grammar blank_grammar_sline no_blank 
let blank_mline = blank_grammar blank_grammar_mline no_blank 
let blank1 = blank_grammar patocomments blank_sline 
let blank2 = blank_grammar patocomments blank_mline 
open Common
let _ = parse_string r blank_sline " a b a b b ab\n   ab bba " 
let _ =
  try
    let _ = parse_string r blank_sline " a b a b b ab\n \n  ab bba "  in
    assert false
  with | Parse_error _ -> () 
let _ = parse_string r blank_mline " a b a b b\n ab\n \n  ab bba " 
let _ = parse_string r blank1 "a  aab aa  a a a a\na b(*to*to*) a ba b " 
let _ =
  try
    let _ = parse_string r blank1 " a b a b b ab\n (*to*to*)\n \n ab bba "
       in
    assert false
  with | Parse_error _ -> () 
let _ = parse_string r blank2 " a b a b b ab\n (*to*to*)\n \n ab bba " 
