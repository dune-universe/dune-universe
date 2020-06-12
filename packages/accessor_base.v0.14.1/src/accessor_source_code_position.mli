open! Base
open! Import

include sig
  type t =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving accessors]
end
with type t := Source_code_position.t
