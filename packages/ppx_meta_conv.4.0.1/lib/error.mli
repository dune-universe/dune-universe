(** {6 Decoding errors } *)

type desc = 
  [ `Exception of exn (*+ exception of the Decode.tuple, variant or record *)

  | `Unknown_fields of 
      string        (*+ type name *) 
      * string list (*+ unknown fields *)
      * Obj.t       (*+ value built from the known fields *)

  | `Unknown_tag of 
      string   (*+ type name *) 
      * string (*+ unknown tag *)

  | `Required_field_not_found of 
      string   (*+ type name *)
      * string (*+ missing field *)

  | `Wrong_arity of 
      int                        (*+ expected tuple/variant arity *) 
      * int                      (*+ actual arity *) 
      * (string * string) option (*+ type name and tag, if tuple, None *) 

  | `Primitive_decoding_failure of  string (*+ message *)

  | `Sub_decoders_failed_for_one_of of string (*+ type name *)
  ]

type 'target trace = [ `Node of 'target | `Pos of int | `Field of string ] list 
(** Position information of decoded data *) 

type 'target t = desc * 'target * 'target trace

open Format

val format : (formatter -> 'target -> unit) -> formatter -> 'target t -> unit
(** Print out [t] with its trace *)

val format_desc : formatter -> desc -> unit
(** Print out [t] without its trace *)

val format_trace_item : 
  (formatter -> 'target -> unit) 
  -> formatter 
  -> [< `Field of string | `Node of 'target | `Pos of int ] 
  -> unit
(** Print out one trace item *)

(** LocalException functor to have exception with target type

  Typical usage: avoid Result monad bind chain, using local an exception.
    
  let f decoder = 
    let module E = LocalException(struct type t = target) in
    E.catch begin
       ... 
       E.exn decoder ...
       ...
    end     
*)
module LocalException(A : sig type t end) : sig
  exception Exception of A.t t
  val exn : ('a -> ('b, A.t t) result) -> 'a -> 'b
  val catch : ('a -> 'b) -> 'a -> ('b, A.t t) result
end
