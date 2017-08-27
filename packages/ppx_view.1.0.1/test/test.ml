open Expect_test_helpers
open Migrate_parsetree.Ast_404
open Viewast.Parseview


let int   x = Ast_helper.(Exp.constant (Const.int   x))
let int32 x = Ast_helper.(Exp.constant (Const.int32 x))
let float x = Ast_helper.(Exp.constant (Const.float x))
let ident x = Ast_helper.(Exp.ident { txt = Lident x; loc = Location.none; })

let%expect_test "match failure" =
  begin try match%view int 3 with
    | Pexp_constant (Pconst_integer ("2", _)) ->
      print_string "matched"
  with e ->
    print_string (Printexc.to_string e)
  end;[%expect {|"Match_failure .*test.ml:12:18" (regexp)|}]

let%expect_test "match simple" =
  let match_3 = function%view
    | Pexp_constant (Pconst_integer ("3", _)) ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match with guard" =
  let match_3 = function%view
    | Pexp_constant (Pconst_integer (s, _)) when s = "3" ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match or-pattern" =
  let match_3 = function%view
    | Pexp_constant (Pconst_integer ("3",  _))
    | Pexp_constant (Pconst_float   ("3.", _)) ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int      3);
    match_3 (int32   3l);
    match_3 (float "3.")
  end;[%expect {|333|}]

let%expect_test "match or-pattern with variable" =
  let match_3xyz = function%view
    | Pexp_constant (Pconst_integer (s, _))
    | Pexp_constant (Pconst_float   (s, _)) when s.[0] = '3' ->
      print_string "3"
    | _ ->
      print_string "KO"
  in
  begin
    match_3xyz (int        3);
    match_3xyz (int32   321l);
    match_3xyz (float "3.14")
  end;[%expect {|333|}]

let%expect_test "match deep or-pattern" =
  let match_3_4 = function%view
    | Pexp_constant (Pconst_integer (("3" | "4"),  _)) ->
      print_string "34"
    | _ ->
      print_string "KO"
  in
  begin
    match_3_4 (int 3);
    match_3_4 (int 4);
  end;[%expect {|3434|}]

let%expect_test "match with alias" =
  let match_3 = function%view
    | Pexp_constant (Pconst_integer ("3" as s,  _)) ->
      print_string s
    | _ ->
      print_string "KO"
  in
  begin
    match_3 (int 3);
  end;[%expect {|3|}]

let%expect_test "match with record" =
  let match_ident = function%view
    | Pexp_ident { txt = Lident id; } ->
      print_string id
    | _ ->
      print_string "KO"
  in
  begin
    match_ident (ident "x");
  end;[%expect {|x|}]
