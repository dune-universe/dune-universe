open! Import

type t =
  | Empty of string
  | Clock of string
  | Binary of string * Data.t
  | Data of string * Data.t * Wave_format.t * Wave_format.alignment
[@@deriving sexp_of]

let set_name t n =
  match t with
  | Empty _ -> Empty n
  | Clock _ -> Clock n
  | Binary (_, a) -> Binary (n, a)
  | Data (_, a, b, c) -> Data (n, a, b, c)
;;

let get_name = function
  | Empty n -> n
  | Clock n -> n
  | Binary (n, _) -> n
  | Data (n, _, _, _) -> n
;;

let get_data = function
  | Empty _ -> failwith "no empty data"
  | Clock _ -> failwith "no clock data"
  | Binary (_, d) -> d
  | Data (_, d, _, _) -> d
;;

let get_to_str = function
  | Empty _ -> failwith "no empty to_str"
  | Clock _ -> failwith "no clock to_str"
  | Binary (_, _) -> failwith "no binary to_str"
  | Data (_, _, f, _) ->
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

let get_alignment = function
  | Empty _ -> failwith "no empty get_alignment"
  | Clock _ -> failwith "no clock get_alignment"
  | Binary _ -> failwith "no binary get_alignment"
  | Data (_, _, _, alignment) -> alignment
;;

let get_format : t -> Wave_format.t = function
  | Empty _ -> Binary
  | Clock _ -> Binary
  | Binary _ -> Binary
  | Data (_, _, f, _) -> f
;;
