(** You should not open this module, unless you are writing decoders/encoders for a new target data type. *)

open Types

(** {6 Tools used by generated code } *)

val field_assoc_exn : 
  string                            (*+ type name *)
  -> string                         (*+ field name *)
  -> (string * 'target) list        (*+ record *)
  -> ('target Error.t -> 'host)     (*+ thrower *)
  -> ('host, 'target) Decoder.t_exn (*+ converter *)
  -> ('host, 'target) Decoder.t_exn

val field_assoc_optional_exn : 
  string                            (*+ type name *)
  -> string                         (*+ field name *)
  -> (string * 'target) list        (*+ record *)
  -> ('host, 'target) Decoder.t_exn (*+ converter *)
  -> ('host option, 'target) Decoder.t_exn

val filter_fields : 
  string list                  (*+ fields known in the type system *) 
  -> (string * 'target) list   (*+ the actual fields *) 
  -> (string * 'target) list   (*+ known fields *)
     * (string * 'target) list (*+ unknown fields *)

(** helper function for embded record field *)
val embeded_decoding_helper : 
  (string * 'target) list              (*+ secondary fields *)
  -> 'target                           (*+ the source target *) 
  -> ('host, 'target Error.t) Result.t (*+ sub decoder result *)
  -> ('host * (string * 'target) list, 'target Error.t) Result.t

(** {6 Error decoders } *)

(** They are decoders but just fail *)

val tuple_arity_error : 
  int    (*+ expected *) 
  -> int (*+ actual *) 
  -> ('host, 'target) Decoder.t

val variant_arity_error : 
  string    (*+ type name *) 
  -> string (*+ constructor name *)
  -> int    (*+ expected *) 
  -> int    (*+ actual *) 
  -> ('host, 'target) Decoder.t

val variant_unknown_tag_error : 
  string    (*+ type name *)
  -> string (*+ tag name *) 
  -> ('host, 'target) Decoder.t

val primitive_decoding_failure : 
  string (*+ message *) 
  -> ('host, 'target) Decoder.t

val sub_decoders_failed_for_one_of : 
  string (*+ type name *)
  -> ('host, 'target) Decoder.t

(** {6 Misc functions } *)

val list_filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** List.map + List.filter *)

val list_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

val (~?) : 'a list option -> 'a list
