include MiniHtml

let print_header _ =
  print_string
    "@[<v 0><!DOCTYPE html>@;\
     @[<v 2><html>@;\
     @[<v 2><head>@;\
     <title>Grammar</title>@;\
     @[<v 2><style>@;\
     @[<v 4>.specification td, th{@;\
     vertical-align: baseline;@;\
     padding: 0;@;\
     margin: 0;@;\
     font-weight: normal;\
     @]@;}@;\
     @[<v 4>.specification td {@;\
     text-align: left;\
     @]@;}@;\
     @[<v 4>.specification th {@;\
     text-align: right;@;\
     white-space: nowrap;\
     @]@;}@;\
     @[<v 4>.specification th.bar {@;\
     text-align: right;\
     @]@;}@;\
     @[<v 4>.rule th, td {@;\
     padding-top: .5em;\
     @]@;}@;\
     @[<v 4>.list_after {@;\
     vertical-align: super;@;\
     font-size: smaller;\
     @]@;}@;\
     @[<v 4>.ne_list_after {@;\
     vertical-align: super;@;\
     font-size: smaller;\
     @]@;}@;\
     @]@;</style>\
     @]@;</head>@;@;\
     @[<v 2><body>@;@;\
     @[<v 2><table class=\"specification\">@;@;"

let nonterminal_before = "&lt;"
let nonterminal_after = "&gt;"
let option_before = "["
let option_after = "]"
let list_after = "*"
let ne_list_after = "+"
let rule_def = "::="

let def () = Format.sprintf "&nbsp;%s&nbsp;</th>@;<td>" rule_def
let prod_bar () = "@[<v 2><tr>@;<th class=\"bar\">|&nbsp;</th>@;<td>"

let print_rule_name =
  print_rule_name_with
    (Format.sprintf "<th>%s<span class=\"nonterminal\">" nonterminal_before)
    (Format.sprintf "</span>%s" nonterminal_after)

let print_symbol symbols =
  print_symbol_aux
    (Format.sprintf "%s<span class=\"nonterminal\">" nonterminal_before)
    (Format.sprintf "</span>%s" nonterminal_after) symbols

let opt _ print =
  print_string option_before;
  print_string "<span class=\"option\">";
  print ();
  print_string "</span>";
  print_string option_after

let plus e print =
  print_string "<span class=\"ne_list\">";
  par e print;
  print_string "</span>";
  print_string "<span class=\"ne_list_after\">";
  print_string ne_list_after;
  print_string "</span>"
let star e print =
  print_string "<span class=\"list\">";
  par e print;
  print_string "</span>";
  print_string "<span class=\"list_after\">";
  print_string list_after;
  print_string "</span>"
