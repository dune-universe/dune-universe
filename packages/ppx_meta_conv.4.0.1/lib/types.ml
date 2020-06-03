open Open

(** {6 Encoder } *)

module Encoder = struct
  type ('host, 'target) t = 'host -> 'target
end

(** {6 Decoder } *)

module Decoder = struct
  type ('host, 'target) t = 
      ?trace:'target Error.trace -> 'target -> ('host, 'target Error.t) Result.t
  (** Error is reported via Result.t *)

  type ('host, 'target) t_exn = 
      ?trace:'target Error.trace -> 'target -> 'host
  (** Error is reported via a target specific exception *)
end

(** {6 Conv module type } *)

module type Min = sig
  type target

  val format : Format.formatter -> target -> unit
  (** The target must be printable. *)  
  (* CR jfuruse: Is it called [print] instead? *)

  module Constr : sig
    val tuple        : target list -> target
    val variant      : string -> string -> target list -> target
    val poly_variant : string -> string -> target list -> target
    val record       : string -> (string * target) list -> target
    val object_      : string -> (string * target) list -> target
  end
  
  module Deconstr : sig

    (** Primitive ADT decoders. They may raise exceptions. *)

    val tuple        : target -> target list
    val variant      : string -> target -> string * target list
    val poly_variant : string -> target -> string * target list
    val record       : string -> target -> (string * target) list
    val object_      : string -> target -> (string * target) list
  end
end

(** [Internal.Make(A : Min)] returns a module with this signature *) 
module type S = sig

  include Min

  (** Exception for decoding error. We must declare here and not 
      in a more general place, since the error contains [target], 
      and we cannot have polymorphic exceptions in OCaml. *)
  exception Exception of target Error.t

  type 'a encoder     = ('a, target) Encoder.t
  type 'a decoder     = ('a, target) Decoder.t
  type 'a decoder_exn = ('a, target) Decoder.t_exn

  (** Auto generated decoders from Deconstr *)
  module DeconstrDecoder : sig
    val tuple        : target list              decoder
    val variant      : string -> (string * target list)   decoder
    val poly_variant : string -> (string * target list)   decoder
    val record       : string -> ((string * target) list) decoder
    val object_      : string -> ((string * target) list) decoder

    val tuple_exn        : target list              decoder_exn
    val variant_exn      : string -> (string * target list)   decoder_exn
    val poly_variant_exn : string -> (string * target list)   decoder_exn
    val record_exn       : string -> ((string * target) list) decoder_exn
    val object_exn       : string -> ((string * target) list) decoder_exn
  end

  val exn : 'a decoder -> 'a decoder_exn
  (** Result monad decoder to decoder with runtime exception [Exception]. *)

  val result : 'a decoder_exn -> 'a decoder
  (** Any exception (including [Exception]) reported from [decoder_exn] 
      is reported as [Error] *)

  val throw : target Error.t -> 'exn 
  (** raises [Exception] *)

  val catch : ('a -> 'b) -> 'a -> ('b, target Error.t) Result.t

  val from_Ok : ('a, target Error.t) Result.t -> 'a
  (** If the argument is [Error e], raises [Exception e]. *)

  open Format

  val format_error      : formatter -> target Error.t -> unit
  (** Format the error, without its trace *)

  val format_full_error : formatter -> target Error.t -> unit
  (** Format the error, with its full trace *)

  val format_with : ('host -> target) -> formatter -> 'host -> unit
  (** Format host data using its encoder *)    

  module Helper : sig
    (** {6 Useful tool functions for writing encoders+decoders of primitive types } *)
    
    val integer_of_float : 
      float              (*+ min value for 'int in float *)
      -> float           (*+ max value for 'int in float *)
      -> (float -> 'int) (*+ conversion *)
      -> float           (*+ to convert *)
      -> ('int, string) Result.t
    (** float to integer conversion with error checks.

        This is not target type dependent, but it is here for easier access.
    *)
    
    val list_of : 
      (target -> target list option) 
      -> 'a decoder -> 'a list decoder
    (** typical list_of_<targe_type>. It takes a function to get a list from a target value *)
    
    val array_of : 
      (target -> target list option) 
      -> 'a decoder -> 'a array decoder
    (** typical array_of_<targe_type>. It takes a function to get a list from a target value  *)
    
    val option_of : 
      (target -> target option option) 
      -> 'a decoder -> 'a option decoder
    (** typical option_of_<targe_type>. 
        It takes predicate to check the arg is a Some, None or something else.
    
        Some (Some v) : Some for a target value v
        Some None     : None
        None :        : Error. Target value cannot have the option type
    *)
    
    val ref_of : 
      (target -> target option) 
      -> 'a decoder -> 'a ref decoder
    (** typical ref_of_<targe_type>. 
        It takes predicate to check the arg is a reference or something else.
    
        Some v : Found a record { contents = <v> }
        None   : Error. Target value cannot have the option type
    *)
    
    val lazy_t_of : 
      (target Error.t -> 'a) (*+ error handler *)
      -> 'a decoder -> 'a lazy_t decoder
    (** typical lazy_t_of_<targe_type>. 
        The decoding is done lazily. 
        Error at the deferred decoding is handled by the error handler, 
        normally it should raises <Target_conv>.Exception exception.
    *)
    
    val of_mc_lazy_t : 'a encoder -> ('a, target) mc_lazy_t encoder
    (** typical <targe_type>_of_mc_lazy_t *)
    
    val mc_lazy_t_of : 'a decoder -> ('a, target) mc_lazy_t decoder
    (** typical mc_lazy_t_of_<targe_type> 
        The decoding is done lazily. 
        Error at the lazy decoding is reported by the result monad.
    *)
     
    val mc_fields_of : 
      (target -> (string * target) list option)
      -> 'a decoder -> (string * 'a) list decoder
    (** typical mc_fields_of_<targe_type> *)
    
    val of_deconstr : (target -> 'a) -> 'a decoder
    (** Convert a simple raw deconstr of type [target -> 'a] to ['a decoder].
        Error must be reported as [Failure s]. The other exceptions simply escape.
    *)

    val of_hashtbl : 
      (target encoder -> target list encoder) (*+ target_of_list *)
      -> 'a encoder (*+ key encoder *)
      -> 'b encoder (*+ value encoder *)
      -> ('a,'b) Hashtbl.t encoder

    val hashtbl_of : 
      (('a * 'b) decoder -> ('a * 'b) list decoder) (*+ list_of_target *)
      -> 'a decoder (*+ key encoder *)
      -> 'b decoder (*+ value encoder *)
      -> ('a, 'b) Hashtbl.t decoder

    val of_result : 
      (target -> target) (* ok embedder *)
      -> (target -> target) (* error embedder *)
      -> 'a encoder  (* ok encoder *)
      -> 'b encoder  (* error encoder *)
      -> ('a, 'b) result encoder

    val result_of : 
      (target, target) result decoder (* result extractor *)
      -> 'a decoder  (* ok deoder *)
      -> 'b decoder  (* error decoder *)
      -> ('a, 'b) result decoder
  end

end
