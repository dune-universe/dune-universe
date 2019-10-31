open Core

type binary_type =
    | Generic
    | Function
    | Binary_old
    | UUID_old
    | UUID
    | MD5
    | Encrypted
    | User_defined [@@deriving sexp]

type document_type = Array | Document | Js_code_w_scope of int [@@deriving sexp]

let document_type_to_char = function
    | Document -> '\x03'
    | Array -> '\x04'
    | Js_code_w_scope _ -> '\x0F'

module type BsonWriter = sig

    type t

    exception Invalid_state of string

    val create : int -> t

    val write_float : t -> string -> float -> unit

    val write_string : t -> string -> string -> unit
    
    val write_document_start : t -> string -> unit

    val write_document_close : t -> unit
    
    (* Write the start of an array. Must be closed with write_array_close. *)
    val write_array_start : t -> string -> unit

    val write_array_close : t -> unit

    val write_binary : t -> string -> binary_type -> bytes -> unit

    (* Bytes must be of length 12 *)
    val write_objectid : t -> string -> bytes -> unit

    val write_bool : t -> string -> bool -> unit
    
    val write_utc_datetime : t -> string -> int64 -> unit

    val write_null : t -> string -> unit

    val write_regex : t -> string -> pattern:string -> options:string -> unit
    
    val write_js : t -> string -> string -> unit

    (* This writes javascript to the key, then opens a new document as its scope. The document must then be closed with write_js_with_scope_close. *)
    val write_js_with_scope : t -> string -> string -> unit

    val write_js_with_scope_close : t -> unit

    val write_int32 : t -> string -> int32 -> unit
    
    val write_timestamp : t -> string -> int64 -> unit
    
    val write_int64 : t -> string -> int64 -> unit
    
    (* Bytes value must by 16 bytes long, representing a decimal128 in little-endian format. *)
    val write_decimal128 : t -> string -> bytes -> unit
    
    val write_minkey : t -> string -> unit

    val write_maxkey : t -> string -> unit

    val to_bytes : t -> (bytes, string) Result.t

    val to_string : t -> (string, string) Result.t

    val to_out_channel : t -> Out_channel.t -> (unit, string) Result.t

end

module type BsonReader = sig

    exception No_data of string

    type bson_type =
        | Double of float
        | String of string
        | Document_start
        | Array_start
        | Binary of binary_type * bytes
        | ObjectId of bytes
        | Boolean of bool
        | DateTime of int64
        | Null
        | Regex of { pattern:string;  options:string } (* Options must be stored in alphabetical order *)
        | JSCode of string
        | JSCode_with_scope of string
        | Int32 of int32
        | Timestamp of int64 
        | Int64 of int64
        | Decimal128 of bytes 
        | Min_key
        | Max_key [@@deriving sexp]

    type read_result =
        | Field of string * bson_type
        | End_of_document [@@deriving sexp]

    type t

    val of_bytes : bytes -> t

    val of_string : string -> t

    val of_in_channel : In_channel.t -> t

    val read_next : t -> read_result

end
