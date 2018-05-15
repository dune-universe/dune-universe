open Core

module Date = struct
  include Date
  include (Csvfields.Csv.Atom (Date) : Csvfields.Csv.Csvable with type t := t)
end

type t = {
  a : float;
  b : string;
  c : int;
  d : Date.t;
} [@@deriving fields, csv, compare, sexp]

let%test_unit _ =
  let actual = csv_load "test.csv" in
  let expect =
    [
      {a = 3.14; b = "first";  c = 1; d = Date.of_string "2012-12-01"};
      {a = 6.28; b = "second"; c = 2; d = Date.of_string "2012-12-02"};
      {a = 9.42; b = "third";  c = 3; d = Date.of_string "2012-12-03"};
    ]
  in
  [%test_result: t list] actual ~expect
;;
