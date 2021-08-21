type b = { b1 : int } [@@deriving make]

type o = { o1 : int option } [@@deriving make]

type l = { l1 : int list } [@@deriving make]

type s = { s1 : string } [@@deriving make]

(*type a = { a1 : int array }[@@deriving make]*)

(*type q = { q1 : int seq }[@@deriving make]*)

type d = { answer : int [@default 42] } [@@deriving make]

type m = { m1 : int; [@main] m2 : int [@main] } [@@deriving make]

type r = {
  r1 : int option;
  r2 : string; [@required]
  r3 : int option; [@required]
}
[@@deriving make]

type complex = {
  c1 : int;
  c2 : int option;
  c3 : int list;
  c4 : string;
  c5 : int; [@default 1024]
  c6 : string; [@required]
}
[@@deriving make]

type complex_with_main = {
  cm1 : int; [@main]
  cm2 : int option;
  cm3 : int option; [@main]
}
[@@deriving make]
