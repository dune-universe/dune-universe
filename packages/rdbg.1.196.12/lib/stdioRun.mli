(* Time-stamp: <modified the 11/01/2017 (at 15:27) by Erwan Jahier> *)

(** Build a RdbgPlugin.t out of a sys call that reads/writes RIF on stdin/stdout *)
val make : string -> RdbgPlugin.t
val make_init : string -> RdbgPlugin.t

(* Ditto via sockets instead of stdin/stdout *)
val make_socket : string -> int -> RdbgPlugin.t
val make_socket_init : string -> int -> RdbgPlugin.t

                            
