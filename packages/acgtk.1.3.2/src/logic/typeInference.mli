open Lambda
open UtilsLib
       

module Type :
sig
  
  (** [inference t] returns [(ty,map)] where [ty] is the type of [t]
      and [map] is a map from [int], denoting the index of occurrences
      of constants considered as free variables (as in Kanazawa's ACG
      to Datalog reduction), to the constant they replace and their
      associated infered type.
      
      The indexes of atomic types are negative to denote type
      variables.
      
  *)
  val inference : Lambda.term -> Lambda.stype * ((Lambda.term*Lambda.stype) Utils.IntMap.t)



end
