(* generating HTML5 with TyXML is not repaired *)
(*
open Html_tyxml_api

type t = { a : int; b : string }
[@@deriving gt ~options:{html}]

type 'a t2 = A of int | C of 'a * int
[@@deriving gt ~options:{html}]

type t3 = D of t | E of int t2
[@@deriving gt ~options:{html}]

type t4 = int t2
[@@deriving gt ~options:{html}]

let () =
  let ch = open_out "/tmp/out.html" in
  let fmt = Format.formatter_of_out_channel ch in
  let t1 = {a=5; b="beeeee"} in
  Tyxml.Html.pp_elt () fmt (html_t t1);
  Tyxml.Html.pp_elt () fmt (Tyxml_html.hr ());

  let t2 = A 5655 in
  Tyxml.Html.pp_elt () fmt (html_t2 float.GT.plugins#html t2);
  Tyxml.Html.pp_elt () fmt (Tyxml_html.hr ());

  let t3 = C (3.1415, 888) in
  Tyxml.Html.pp_elt () fmt (html_t2 float.GT.plugins#html t3);
  Tyxml.Html.pp_elt () fmt (Tyxml_html.hr ());

  let t4 = D t1 in
  Tyxml.Html.pp_elt () fmt (html_t3 t4);
  Tyxml.Html.pp_elt () fmt (Tyxml_html.hr ());

  let t5 = E (A 18) in
  Tyxml.Html.pp_elt () fmt (html_t3 t5);
  Tyxml.Html.pp_elt () fmt (Tyxml_html.hr ());

  close_out ch
*)
