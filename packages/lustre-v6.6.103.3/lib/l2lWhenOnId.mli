(** Time-stamp: <modified the 31/05/2016 (at 16:58) by Erwan Jahier> *)

(** Transforms equations of the form

   y = x when Toto(c);

   into 
  
   x when Toto_c ; 
   Toto_c=Toto(c);
    
*)

val doit : LicPrg.t -> LicPrg.t

