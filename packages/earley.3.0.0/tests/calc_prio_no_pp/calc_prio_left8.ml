open Earley_core
open Generate_calc
open Earley
type calc_prio =
  | Sum 
  | Pro 
  | Pow 
  | Atm 
let prio_to_string =
  function | Sum  -> "Sum" | Pro  -> "Prod" | Pow  -> "Pow" | Atm  -> "Atom" 
let float_re = "[0-9]+\\([.][0-9]+\\)?\\([eE][-+]?[0-9]+\\)?" 
let float_num =
  Earley.fsequence
    (Earley_str.regexp ~name:"float" float_re (fun groupe  -> groupe 0))
    (Earley.empty (fun f  -> float_of_string f))
  
let pro_sym =
  Earley.alternatives
    [Earley.fsequence_ignore (Earley.char '/' '/') (Earley.empty (/.));
    Earley.fsequence_ignore (Earley.char '*' '*') (Earley.empty ( *. ))]
  
let sum_sym =
  Earley.alternatives
    [Earley.fsequence_ignore (Earley.char '-' '-') (Earley.empty (-.));
    Earley.fsequence_ignore (Earley.char '+' '+') (Earley.empty (+.))]
  
let extra = ref [] 
let extra_expression p =
  Earley.alternatives (List.map (fun g  -> g p) (!extra)) 
let (expr_aux,expr_aux__set__grammar) = Earley.grammar_prio "expr_aux" 
let (expr,expr__set__grammar) = Earley.grammar_family "expr" 
let _ =
  expr_aux__set__grammar
    ([(((fun p  -> p <= Sum)),
        (Earley.fsequence (expr Sum)
           (Earley.fsequence sum_sym
              (Earley.fsequence (expr Pro)
                 (Earley.empty (fun e'  -> fun fn  -> fun e  -> fn e e'))))));
     (((fun _  -> true)), float_num);
     (((fun _  -> true)),
       (Earley.fsequence_ignore (Earley.char '(' '(')
          (Earley.fsequence (expr Sum)
             (Earley.fsequence_ignore (Earley.char ')' ')')
                (Earley.empty (fun e  -> e))))));
     (((fun p  -> p <= Pow)),
       (Earley.fsequence_ignore (Earley.char '-' '-')
          (Earley.fsequence (expr Pow) (Earley.empty (fun e  -> -. e)))));
     (((fun p  -> p <= Pow)),
       (Earley.fsequence_ignore (Earley.char '+' '+')
          (Earley.fsequence (expr Pow) (Earley.empty (fun e  -> e)))));
     (((fun p  -> p <= Pow)),
       (Earley.fsequence (expr Atm)
          (Earley.fsequence_ignore (Earley.string "**" "**")
             (Earley.fsequence (expr Pow)
                (Earley.empty (fun e'  -> fun e  -> e ** e'))))));
     (((fun p  -> p <= Pro)),
       (Earley.fsequence (expr Pro)
          (Earley.fsequence pro_sym
             (Earley.fsequence (expr Pow)
                (Earley.empty (fun e'  -> fun fn  -> fun e  -> fn e e'))))))],
      (fun p  -> []))
  
let _ =
  expr__set__grammar
    (fun p  -> Earley.alternatives [expr_aux p; extra_expression p])
  
let _ = run (expr Sum) 
