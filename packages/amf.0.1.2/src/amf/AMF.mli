open Core

module V0 :
sig
  exception UnsupportedType
  exception UpgradeToAMFV3
  type typed_object = {
    class_name : string;
    properties : (string * t) list;
  }
  and t =
    | Number of float
    | Boolean of bool
    | String of string
    | Object of (string * t) list
    | MovieClip
    | Null
    | Undefined
    | Reference of int
    | ECMAArray of (string * t) list
    | ObjectEnd
    | StrictArray of t array
    | Date of Time.t
    | LongString of string
    | Unsupported
    | RecordSet
    | XMLDocument of string
    | TypedObject of typed_object
    | AVMPlusObject
  val to_buffer : t -> string
  val of_buffer : string -> (t, Core_kernel.Error.t) result
  val list_of_buffer : string -> (t list, Core_kernel.Error.t) result
  val buffer_of_list : t list -> string
  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t

  (** [peel_off_buffer buffer] attempts to peel off the first valid AMF value in [buffer]. If successful, it returns [Result.Ok (value, remainder)]. *)
  val peel_off_buffer : string -> (t * string, Core_kernel.Error.t) result

  (** [peel_off_list buffer] repeatedly calls [peel_off_buffer] until it either returns an error or it successfully parses all the items in the buffer.

      On failure, it returns an error result containing a list of items parsed successfully, the unparsed remainder, and the accompanying error.

      On success, it returns an ok result list of the parsed items.*)
  val peel_off_list :
    string -> (t list, t list * string * Core_kernel.Error.t) result

end
