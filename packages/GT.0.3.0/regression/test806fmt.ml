type t = {a : GT.int; b: GT.string} [@@deriving gt ~options:{fmt}]
type t2 =
  | QQQ of GT.string
  | LLL of GT.int GT.list
 [@@deriving gt ~options:{fmt}]


let () =
  let open Format in
  pp_set_margin std_formatter 12;
  fprintf std_formatter "%a\n"  t.GT.plugins#fmt {a=1; b="x"};
  (* fprintf std_formatter "%a\n" t2.GT.plugins#fmt (RRR {asdf=20}); *)
  fprintf std_formatter "%a\n" t2.GT.plugins#fmt (QQQ "azerty");
  fprintf std_formatter "%a\n" t2.GT.plugins#fmt (LLL [1;2;3]);
  fprintf std_formatter "%a\n"
    (GT.list.GT.plugins#fmt GT.float.GT.plugins#fmt)
    [1.; 2.; 3.; 4.];
  ()

type t3 = Foo of {xxx: GT.int; yyy: GT.int}
        | Boo of GT.int * GT.int
[@@deriving gt ~options:{fmt; compare}]
