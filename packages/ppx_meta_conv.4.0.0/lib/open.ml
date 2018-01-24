(** You should open this module *)

(** Special types 

    This module defines types specially handled by meta_conv.
    To be handled correctly, these type names must be used WITHOUT module names.
    
    For example, Meta_conv.Types.mc_option or Types.mc_option are just treated
    as normal types and no special meta_conv decoding/encoding are not generated 
    for them.
*)

type 'a mc_option = 'a option
type 'target mc_fields = (string * 'target) list
type 'target mc_leftovers = (string * 'target) list
type ('host, 'target) mc_lazy_t = ('host, 'target Error.t) Result.t lazy_t
type ('host, 'target) mc_result = ('host, 'target Error.t) Result.t
type 'a mc_embeded = 'a
type 'a mc_option_embeded = 'a option

type ('a, 'b) hashtbl = ('a, 'b) Hashtbl.t
