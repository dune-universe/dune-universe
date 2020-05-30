type output =
  [ `Err of [ `EOF | `Error of exn | `Read of string ]
  | `Out of [ `EOF | `Error of exn | `Read of string ]
  ]

module Ver1 : sig
  val exec :
    ?env:string array  (*+ env strings *)
    -> string list (*+ command string tokens *)
    -> (output SpotStream.t -> 'a) (*+ stream processing *)
    -> (('a, [> `Exn of exn]) result * Unix.process_status -> 'res) (*+ process finalizer *)
    -> 'res
  
  val shell_exec :
    ?env:string array  (*+ env strings *)
    -> string  (*+ command string *)
    -> (output SpotStream.t -> 'a) (*+ stream processing *)
    -> (('a, [> `Exn of exn]) result * Unix.process_status -> 'res) (*+ process finalizer *)
    -> 'res
  
  val must_exit_with : int -> ('a, [< `Exn of exn ]) result * Unix.process_status -> 'a
  (** a process finalizer. If the exit status is different from the argument,
      it raises [Failure mes]
  *)
  
  val force_lines : output SpotStream.t -> string list
  
  val force_stdout : output SpotStream.t -> string list
  
  val force_stderr : output SpotStream.t -> string list
  
  val print : output SpotStream.t -> unit
end

(** This is Ver2 *)

type 'a t

val shell :
  ?env:string array  (*+ env strings *)
  -> string  (*+ command string *)
  -> output SpotStream.t t

val exec :
  ?env:string array  (*+ env strings *)
  -> string list (*+ command string *)
  -> output SpotStream.t t

val command : 'a t -> string

val wait : 'a t -> 'a * Unix.process_status

val map : ('a -> 'b) -> 'a t -> 'b t

val print : output SpotStream.t t -> output SpotStream.t t

val lines : output SpotStream.t t -> string list t 

val stdout : output SpotStream.t t -> string list t 

val stderr : output SpotStream.t t -> string list t 

val iter : (output -> unit) -> output SpotStream.t t -> unit t

val fold : ('a -> 'b -> 'b) -> 'b -> 'a SpotStream.t t -> 'b t
  
val void : 'a t -> unit t
  
val must_exit_with : int -> 'a * Unix.process_status -> 'a
