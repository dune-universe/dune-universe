module type Intf = sig
  val pretty_print : Format.formatter -> 'a Json_internal.constrained -> unit
  val pretty_print_to_string : 'a Json_internal.constrained -> string
  val pretty_print_to_channel : out_channel -> 'a Json_internal.constrained -> unit
end

module Make (Compliance : Compliance.S) : Intf
