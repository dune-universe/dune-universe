val ok200 : int * 'a -> ('a, [> `Http of int * 'a ]) result
(** HTTP result to a result monad *)

val get_string 
  : (Curl.handle -> unit) 
  -> (string, [> `Http of int * string 
              |  `Curl of Curl.curlCode * int * string ]) result
(** Get string *)

val download 
  : string (*+ destination *)
  -> (Curl.handle -> unit) 
  -> (string, [> `Http of int * string (* downloaded file path *) ]) result
(** Download file. Return the final downloaded file path.
    At error, the tmp file is not removed since it may contain error message. 
*)
