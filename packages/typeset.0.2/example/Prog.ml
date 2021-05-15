(* AST definitions *)
module AST = struct

type shape =
  | SUnit
  | SArrow of shape * shape

type stmt =
  | SDefn of string * shape * stmt
  | SDecl of string * value * stmt
  | SExpr of expr
and expr =
  | EApp of expr * expr
  | EValue of value
and value =
  | VUnit
  | VParam of string
  | VAbs of string * stmt

end (* AST *)

(* Layout functions *)
module Layout = struct
open Typeset
open AST

let _skip layout = layout
let _group layout = grp (~$"(" <!&> layout <!&> ~$")")
let _indent layout = nest (grp layout)

let rec _visit_shape wrap shape return =
  match shape with
  | SUnit -> return ~$"unit"
  | SArrow (dom, codom) ->
    _visit_shape _group dom @@ fun dom1 ->
    _visit_shape _skip codom @@ fun codom1 ->
    return (wrap (seq (dom1 <+> ~$"->" <!+> codom1)))

let rec _visit_stmt stmt return =
  match stmt with
  | SDefn (name, shape, stmt1) ->
    _visit_shape _skip shape @@ fun shape1 ->
    _visit_stmt stmt1 @@ fun stmt2 ->
    return (~$name <!+> ~$":" <+> (_indent shape1) </> stmt2)
  | SDecl (name, value, stmt1) ->
    _visit_value _skip value @@ fun value1 ->
    _visit_stmt stmt1 @@ fun stmt2 ->
    return (~$name <!+> ~$"=" <+> (_indent value1) </> stmt2)
  | SExpr expr1 ->
    _visit_expr _skip expr1 return
and _visit_expr wrap expr return =
  match expr with
  | EApp (func, arg) ->
    _visit_expr _skip func @@ fun func1 ->
    _visit_expr _group arg @@ fun arg1 ->
    return (wrap (func1 <+> (_indent arg1)))
  | EValue value ->
    _visit_value wrap value return
and _visit_value wrap value return =
  match value with
  | VUnit -> return ~$"()"
  | VParam name -> return ~$name
  | VAbs (param, body) ->
    _visit_stmt body @@ fun body1 ->
    return (wrap (~$param <!+> ~$"=>" <+> (_indent body1)))

let shape x r = _visit_shape _skip x r
let stmt x r = _visit_stmt x r
let expr x r = _visit_expr _skip x r
let value x r = _visit_value _skip x r

end (* Layout *)

(* Print functions *)
module Print = struct

let shape shape1 return =
  Layout.shape shape1 @@ fun layout ->
  Typeset.compile layout @@ fun document ->
  Typeset.render document 2 80 return

let stmt stmt1 return =
  Layout.stmt stmt1 @@ fun layout ->
  Typeset.compile layout @@ fun document ->
  Typeset.render document 2 80 return

let expr expr1 return =
  Layout.expr expr1 @@ fun layout ->
  Typeset.compile layout @@ fun document ->
  Typeset.render document 2 80 return

let value value1 return =
  Layout.value value1 @@ fun layout ->
  Typeset.compile layout @@ fun document ->
  Typeset.render document 2 80 return

end (* Print *)
