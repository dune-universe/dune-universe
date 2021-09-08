type err =
     | BadArity of (string * int * int)
     | ParseError of string




module E =
  struct
    type t = err
                    
    let to_string = function
      | BadArity (sym,expected_a,written_a) -> Printf.sprintf "Predicate symbole \"%s\" is defined with arity %d while used with arity %d" sym expected_a written_a
      | ParseError s -> s
  end
  
module Error = UtilsLib.ErrorMg.Make(E)
                   
