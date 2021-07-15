(* Time-stamp: <modified the 30/06/2017 (at 10:13) by Erwan Jahier> *)


(** Split the equations of a node into several ones, in such a way
    that there is only one operator per equation. 

    We also split tuples. For example, the equation:

    (a, b, c) =  (1, 2, 3)  -> titi(x); 

    is splitted into 4 equations:

    a = 1 -> _v1;
    b = 2 -> _v2;
    c = 3 -> _v3;
    (_v1, _v2, _v3) = titi(x);

    The node local_env is provided so that we can update its table, as
    we add some fresh local variables during the code transmation.
*)

val doit : Lv6MainArgs.t -> LicPrg.t -> LicPrg.t
