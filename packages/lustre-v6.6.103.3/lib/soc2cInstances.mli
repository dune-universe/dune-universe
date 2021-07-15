(* Time-stamp: <modified the 18/03/2016 (at 10:35) by Erwan Jahier> *)

(** Each soc has a list of soc instances, made of an (unique) ident
    and a Soc.key.  

    In order to be able to iterate on such instances at the C level
    (e.g., with a for loop), instances are stored into arrays.  

    The to_array function helps to do that, by gathering instances of
    the same soc.
 
    More precisely from a list of soc instances l, this function
    returns :

    - a list made of all the soc.key in l + their number of occ in l
    (which will help to compute the array size)

    - a function that maps each instance ident to its position in the array
     where it is stored
 *)

val to_array : (Soc.ident * Soc.key) list -> 
               (Soc.key * int) list * (Soc.ident * Soc.key -> int)

(* XXX the 2nd looks unused. rm ? *)
