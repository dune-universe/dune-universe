open! Import

type t =
  | Clock of string
  | Binary of string * Data.t
  | Data of string * Data.t * Wave_format.t
[@@deriving sexp_of]

let get_name = function
  | Clock n -> n
  | Binary (n, _) -> n
  | Data (n, _, _) -> n
;;

let get_data = function
  | Clock _ -> failwith "no clock data"
  | Binary (_, d) -> d
  | Data (_, d, _) -> d
;;

let get_to_str = function
  | Clock _ -> failwith "no clock to_str"
  | Binary (_, _) -> failwith "no binary to_str"
  | Data (_, _, f) ->
    let rec to_f : Wave_format.t -> _ = function
      | Binary -> Bits.to_bstr
      | Bit -> Bits.to_bstr
      | Bit_or t -> to_f t
      | Hex -> Bits.to_hstr
      | Unsigned_int -> Bits.to_ustr
      | Int -> Bits.to_sstr
      | Custom f -> f
      | Index s ->
        fun elt ->
          (try List.nth_exn s (Bits.to_int elt) with
           | _ -> "-")
    in
    to_f f
;;

let get_format : t -> Wave_format.t = function
  | Clock _ -> Binary
  | Binary _ -> Binary
  | Data (_, _, f) -> f
;;
