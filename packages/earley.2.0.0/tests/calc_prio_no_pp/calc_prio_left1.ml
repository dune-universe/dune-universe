open Earley_core
open Earley
open Generate_calc
open Common
type calc_prio =
  | Sum 
  | Prod 
  | Pow 
  | Atom 
let prio_to_string =
  function
  | Sum  -> "Sum"
  | Prod  -> "Prod"
  | Pow  -> "Pow"
  | Atom  -> "Atom" 
let float_re = "[0-9]+\\([.][0-9]+\\)?\\([eE][-+]?[0-9]+\\)?" 
let float_num =
  Earley.fsequence
    (Earley_str.regexp ~name:"float" float_re (fun groupe  -> groupe 0))
    (Earley.empty (fun f  -> float_of_string f))
  
let prod_sym =
  Earley.alternatives
    [Earley.fsequence_ignore (Earley.char '/' '/') (Earley.empty (/.));
    Earley.fsequence_ignore (Earley.char '*' '*') (Earley.empty ( *. ))]
  
let sum_sym =
  Earley.alternatives
    [Earley.fsequence_ignore (Earley.char '-' '-') (Earley.empty (-.));
    Earley.fsequence_ignore (Earley.char '+' '+') (Earley.empty (+.))]
  
let test = Earley.declare_grammar "test" 
let _ =
  Earley.set_grammar test
    (Earley.alternatives
       [Earley.fsequence test
          (Earley.fsequence_ignore (Earley.char 'a' 'a')
             (Earley.empty (fun _default_0  -> _default_0)));
       Earley.fsequence_ignore (Earley.empty ()) (Earley.empty ())])
  
let (expr,set_expr) = grammar_family ~param_to_string:prio_to_string "expr" 
let _ =
  set_expr
    (fun prio  ->
       Earley.alternatives
         ((if prio <= Sum
           then
             [Earley.fsequence (expr Sum)
                (Earley.fsequence sum_sym
                   (Earley.fsequence (expr Prod)
                      (Earley.empty (fun e'  -> fun fn  -> fun e  -> fn e e'))))]
           else []) @ (float_num ::
            (Earley.fsequence_ignore (Earley.char '(' '(')
               (Earley.fsequence (expr Sum)
                  (Earley.fsequence_ignore (Earley.char ')' ')')
                     (Earley.empty (fun e  -> e)))))
            ::
            ((if prio <= Pow
              then
                [Earley.fsequence_ignore (Earley.char '-' '-')
                   (Earley.fsequence (expr Pow)
                      (Earley.empty (fun e  -> -. e)))]
              else []) @
               ((if prio <= Pow
                 then
                   [Earley.fsequence_ignore (Earley.char '+' '+')
                      (Earley.fsequence (expr Pow)
                         (Earley.empty (fun e  -> e)))]
                 else []) @
                  ((if prio <= Pow
                    then
                      [Earley.fsequence (expr Atom)
                         (Earley.fsequence_ignore (Earley.string "**" "**")
                            (Earley.fsequence (expr Pow)
                               (Earley.empty (fun e'  -> fun e  -> e ** e'))))]
                    else []) @
                     ((if prio <= Prod
                       then
                         [Earley.fsequence (expr Prod)
                            (Earley.fsequence prod_sym
                               (Earley.fsequence (expr Pow)
                                  (Earley.empty
                                     (fun e'  -> fun fn  -> fun e  -> fn e e'))))]
                       else []) @ [])))))))
  
let _ = run (expr Sum) 
