(** Time-stamp: <modified the 26/08/2016 (at 10:49) by Erwan Jahier> *)

(** Transforms equations of the form

   y = x when not some_bool_clock;

   into 

  not_some_bool_clock : bool;
  [...]
  
   y = x when not_some_bool_clock;

It is useful for generating ec code, that does not understand "when not"
statements (sigh).
    
*)

val doit : LicPrg.t -> LicPrg.t

