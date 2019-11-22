module Domain : sig
  include
    Mimestring.S
    with type t = Email_address.Domain.t
     and type comparator_witness = Email_address.Domain.comparator_witness

  val to_string : t -> string
end

include module type of struct
  include Email_address
end
with module Domain := Email_address.Domain

val local_address : unit -> t
