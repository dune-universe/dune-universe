open! Base
open! Import

type t = Source_code_position.t =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving accessors]
