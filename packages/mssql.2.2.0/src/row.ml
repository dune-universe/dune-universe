open Core

type t = Db_field.t option String.Map.t [@@deriving compare, sexp_of]

let create_exn ~month_offset ~colnames row =
  List.map row ~f:(Db_field.of_data ~month_offset)
  |> List.zip_exn colnames
  |> String.Map.of_alist_exn
;;

let to_alist (t : t) = List.map (Map.to_alist t) ~f:(Tuple2.map_snd ~f:Db_field.to_string)

let find_map ~(f : ?column:string -> Db_field.t -> 'a) (t : t) column =
  match Map.find t column with
  | None ->
    let columns = Map.keys t |> String.concat ~sep:", " in
    failwithf
      "Column %s was not returned in the query, columns returned are: %s"
      column
      columns
      ()
  | Some data -> Option.map data ~f:(f ~column)
;;

let required colname = function
  | None -> failwithf "Expected data for %s but got NULL" colname ()
  | Some v -> v
;;

let bignum = find_map ~f:Db_field.bignum
let bignum_exn t colname = required colname (bignum t colname)
let float = find_map ~f:Db_field.float
let float_exn t colname = required colname (float t colname)
let int = find_map ~f:Db_field.int
let int_exn t colname = required colname (int t colname)
let int32 = find_map ~f:Db_field.int32
let int32_exn t colname = required colname (int32 t colname)
let int64 = find_map ~f:Db_field.int64
let int64_exn t colname = required colname (int64 t colname)
let bool = find_map ~f:Db_field.bool
let bool_exn t colname = required colname (bool t colname)
let str = find_map ~f:Db_field.str
let str_exn t colname = required colname (str t colname)
let date = find_map ~f:Db_field.date
let date_exn t colname = required colname (date t colname)
let datetime = find_map ~f:Db_field.datetime
let datetime_exn t colname = required colname (datetime t colname)
