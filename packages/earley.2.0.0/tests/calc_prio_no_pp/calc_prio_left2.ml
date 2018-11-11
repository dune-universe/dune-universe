open Earley_core
open Earley
open Generate_calc
type calc_prio =
  | Sum 
  | Prod 
  | Pow 
  | Atom 
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
  
let (expr_suit,expr_suit__set__grammar) = Earley.grammar_family "expr_suit" 
let expr = Earley.declare_grammar "expr" 
let _ =
  expr_suit__set__grammar
    (fun p  ->
       Earley.alternatives
         ((if p >= Sum
           then
             [Earley.fsequence sum_sym
                (Earley.fsequence expr
                   (Earley.empty
                      (fun ((p',e') as _default_0)  ->
                         fun fn  ->
                           if p' <= Sum then give_up ();
                           (fun e  -> (Sum, (fn e e'))))))]
           else []) @
            ((if p > Pow
              then
                [Earley.fsequence_ignore (Earley.string "**" "**")
                   (Earley.fsequence expr
                      (Earley.empty
                         (fun ((p',e') as _default_0)  ->
                            if p' < Pow then give_up ();
                            (fun e  -> (Pow, (e ** e'))))))]
              else []) @
               ((if p >= Prod
                 then
                   [Earley.fsequence prod_sym
                      (Earley.fsequence expr
                         (Earley.empty
                            (fun ((p',e') as _default_0)  ->
                               fun fn  ->
                                 if p' <= Prod then give_up ();
                                 (fun e  -> (Prod, (fn e e'))))))]
                 else []) @ []))))
  
let _ =
  Earley.set_grammar expr
    (Earley.alternatives
       [Earley.iter
          (Earley.fsequence expr
             (Earley.empty
                (fun ((p,e) as _default_0)  ->
                   Earley.fsequence (expr_suit p)
                     (Earley.empty (fun g  -> g e)))));
       Earley.fsequence float_num (Earley.empty (fun f  -> (Atom, f)));
       Earley.fsequence_ignore (Earley.char '(' '(')
         (Earley.fsequence expr
            (Earley.fsequence_ignore (Earley.char ')' ')')
               (Earley.empty (fun ((_,e) as _default_0)  -> (Atom, e)))));
       Earley.fsequence_ignore (Earley.char '-' '-')
         (Earley.fsequence expr
            (Earley.empty
               (fun ((p,e) as _default_0)  ->
                  if p < Pow then give_up (); (Pow, (-. e)))));
       Earley.fsequence_ignore (Earley.char '+' '+')
         (Earley.fsequence expr
            (Earley.empty
               (fun ((p,e) as _default_0)  ->
                  if p < Pow then give_up (); (Pow, e))))])
  
let _ = run (apply snd expr) 
