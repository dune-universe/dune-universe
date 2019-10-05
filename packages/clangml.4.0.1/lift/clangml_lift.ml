[@@@ocaml.warning "-30"]

module%import Clang = struct
  module%override Bindings = struct
    [%%recursive [%%types]]
      [@@deriving traverse_lift]
  end

  open Bindings

  module%override Types = struct
    [%%recursive [%%types]]
      [@@deriving traverse_lift]
  end

  open Types

  module%override Ast = struct
    [%%recursive [%%types]]
      [@@deriving traverse_lift]
  end
end

class virtual ['a] lift = object
  inherit ['a] Bindings.lift
  inherit ['a] Types.lift
  inherit ['a] Ast.lift
end

class lift_expr loc = object(self)
  inherit [Ppxlib.expression] lift
  inherit Ppxlib_metaquot_lifters.expression_lifters loc

  method! open_node :
    'a 'b . ('a -> Ppxlib.expression) -> ('b -> Ppxlib.expression) ->
      ('a, 'b) Clang.Ast.open_node -> Ppxlib.expression =
    fun _a _qual_type node ->
      [%expr {
        decoration = Custom { location = None; qual_type = None };
        desc = [%e _a node.desc] }]

  method! qual_type (qual_type : Clang.Ast.qual_type) =
    let lift = Ppxlib_metaquot.Expr.lift loc in
    let const = lift#bool qual_type.const in
    let volatile = lift#bool qual_type.volatile in
    let restrict = lift#bool qual_type.restrict in
    [%expr {
      cxtype = Clang.Bindings.get_cursor_type
        (Clang.Bindings.get_null_cursor ());
      const = [%e const]; volatile = [%e volatile]; restrict = [%e restrict];
      desc = [%e self#type_desc qual_type.desc ] }]
end

class lift_pattern loc = object(self)
  inherit [Ppxlib.pattern] lift
  inherit Ppxlib_metaquot_lifters.pattern_lifters loc

  method! open_node :
    'a 'b . ('a -> Ppxlib.pattern) -> ('b -> Ppxlib.pattern) ->
      ('a, 'b) Clang.Ast.open_node -> Ppxlib.pattern =
    fun _a _qual_type node ->
      [%pat? {
        decoration = _;
        desc = [%p _a node.desc] }]

  method! qual_type (qual_type : Clang.Ast.qual_type) =
    let lift = Ppxlib_metaquot.Patt.lift loc in
    let const = lift#bool qual_type.const in
    let volatile = lift#bool qual_type.volatile in
    let restrict = lift#bool qual_type.restrict in
    [%pat? {
      cxtype = _; const = [%p const];
      volatile = [%p volatile]; restrict = [%p restrict];
      desc = [%p self#type_desc qual_type.desc ] }]
end
