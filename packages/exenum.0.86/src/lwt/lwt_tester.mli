(** 
    {2 Lwt version of Exenum.tester }
*)
    
(** Note that [tester] does not comply with non-lwt exceptions: some exceptions may get lost.
    @param verbose_period A message is printed every [verbose_period] tests.
    @param tos Function used to print values of the enumeration.
    @param len [len] consecutive values are enumerated, then the index is doubled (and we loop)
    @param upto By default, the tests go on forever.
*)
val tester : 'a Exenum.t -> ?from:Z.t -> ?upto:Z.t -> ?verbose_period:int -> ?tos:('a -> string) -> len:int -> ('a -> unit Lwt.t) -> unit Lwt.t
