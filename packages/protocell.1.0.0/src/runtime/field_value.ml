open Base

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

type 'v t = 'v typ * 'v

type validation_error = [`Integer_outside_field_type_range of int typ * int]

let typ_to_string : type v. v typ -> string = function
  | String_t -> "string"
  | Bytes_t -> "bytes"
  | Int32_t -> "int32"
  | Int64_t -> "int64"
  | Sint32_t -> "sint32"
  | Sint64_t -> "sint64"
  | Uint32_t -> "uint32"
  | Uint64_t -> "uint64"
  | Fixed32_t -> "fixed32"
  | Fixed64_t -> "fixed64"
  | Sfixed32_t -> "sfixed32"
  | Sfixed64_t -> "sfixed64"
  | Float_t -> "float"
  | Double_t -> "double"
  | Bool_t -> "bool"

let default : type v. v typ -> v = function
  | String_t -> ""
  | Bytes_t -> ""
  | Int32_t -> 0
  | Int64_t -> 0
  | Sint32_t -> 0
  | Sint64_t -> 0
  | Uint32_t -> 0
  | Uint64_t -> 0
  | Fixed32_t -> 0
  | Fixed64_t -> 0
  | Sfixed32_t -> 0
  | Sfixed64_t -> 0
  | Float_t -> 0.0
  | Double_t -> 0.0
  | Bool_t -> false

let max_uint_32_value =
  match Int32.(to_int max_value) with
  | None -> Int.max_value
  | Some n -> (2 * n) + 1

let create : type v. v typ -> v -> (v t, [> validation_error]) Result.t =
 fun typ value ->
  let validate_i32 : int typ -> int -> (int t, [> validation_error]) Result.t =
   fun typ value ->
    match Int.to_int32 value with
    | None -> Error (`Integer_outside_field_type_range (typ, value))
    | Some _ -> Ok (typ, value)
  in
  let validate_u32 : int typ -> int -> (int t, [> validation_error]) Result.t =
   fun typ value ->
    match value < 0 || value > max_uint_32_value with
    | true -> Error (`Integer_outside_field_type_range (typ, value))
    | false -> Ok (typ, value)
  in
  let validate_u64 : int typ -> int -> (int t, [> validation_error]) Result.t =
   fun typ value ->
    match value < 0 with
    | true -> Error (`Integer_outside_field_type_range (typ, value))
    | false -> Ok (typ, value)
  in
  match typ with
  | String_t -> Ok (typ, value)
  | Bytes_t -> Ok (typ, value)
  | Int32_t -> validate_i32 Int32_t value
  | Int64_t -> Ok (typ, value)
  | Sint32_t -> validate_i32 Sint32_t value
  | Sint64_t -> Ok (typ, value)
  | Uint32_t -> validate_u32 Uint32_t value
  | Uint64_t -> validate_u64 Uint64_t value
  | Fixed32_t -> validate_u32 Fixed32_t value
  | Fixed64_t -> validate_u64 Fixed64_t value
  | Sfixed32_t -> validate_i32 Fixed32_t value
  | Sfixed64_t -> Ok (typ, value)
  | Float_t -> Ok (typ, value)
  | Double_t -> Ok (typ, value)
  | Bool_t -> Ok (typ, value)

let typ (typ, _) = typ

let unpack (_, value) = value
