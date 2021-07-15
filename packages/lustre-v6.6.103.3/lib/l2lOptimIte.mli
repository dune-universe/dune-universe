(* Time-stamp: <modified the 14/01/2015 (at 17:31) by Erwan Jahier> *)


(** Transforms expressions of the form

    if c then n(...) else m(...) 

    into 

     merge c (true -> n(...) when c) (false -> m(...) when not(c))

    Of course, this is done iff neither n nor m uses memories.
    
*)

val doit : LicPrg.t -> LicPrg.t


