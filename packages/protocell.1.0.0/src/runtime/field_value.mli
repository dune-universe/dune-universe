open Base

type 'v t

type _ typ =
  | String_t : string typ
  | Bytes_t : string typ
  | Int32_t : int typ
  | Int64_t : int typ
  | Sint32_t : int typ
  | Sint64_t : int typ
  | Uint32_t : int typ
  | Uint64_t : int typ
  | Fixed32_t : int typ
  | Fixed64_t : int typ
  | Sfixed32_t : int typ
  | Sfixed64_t : int typ
  | Float_t : float typ
  | Double_t : float typ
  | Bool_t : bool typ

type validation_error = [`Integer_outside_field_type_range of int typ * int]

val typ_to_string : 'v typ -> string

val default : 'v typ -> 'v

val create : 'v typ -> 'v -> ('v t, [> validation_error]) Result.t

val typ : 'v t -> 'v typ

val unpack : 'v t -> 'v
