open! Core

type t =
  | Words
  | Seconds
  | Nanoseconds
  | Int
[@@deriving sexp, compare]

let to_string t = t |> [%sexp_of: t] |> Sexp.to_string
let of_string str = str |> Sexp.of_string |> [%of_sexp: t]

let format_int t x =
  let float_to_string = Float.to_string_hum ~strip_zero:true ~decimals:2 in
  let rec loop x_div suffixes =
    match suffixes with
    | [] -> assert false
    | [s] -> float_to_string x_div ^ s
    | s::ss ->
      if Float.abs x_div < 1000.
      then float_to_string x_div ^ s
      else loop (x_div /. 1000.) ss
  in
  let x = float x in
  match t with
  | Seconds     -> float_to_string x ^ "s"
  | Nanoseconds -> loop x ["ns"; "us"; "ms"; "s"]
  | Words       -> loop x ["w"; "kw"; "Mw"; "Gw"]
  | Int       -> loop x [""; "e3"; "e6"; "e9"; "e12"]


let%test_unit "format_int" =
  let long = 5_000_001_000_000_001L |> Int64.to_int_exn in
  List.iter
    [ (long,      Nanoseconds, "5_000_001s")
    ; (-100_100,  Nanoseconds, "-100.1us")
    ; (-99_010,   Nanoseconds, "-99.01us")
    ; (-99_001,   Nanoseconds, "-99us")
    ; (201,       Nanoseconds, "201ns")
    ; (4_500_000, Nanoseconds, "4.5ms")

    ; (1_000,     Seconds,     "1_000s")
    ; (100,       Words,       "100w")

    ; (-235,      Int,       "-235")
    ]
    ~f:(fun (num, units, str) ->
      [%test_eq: string] (format_int units num) str
    )
