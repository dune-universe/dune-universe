[%%metapackage metapp]
[%%metadir "traverse_meta/.traverse_meta.objs/byte/"]
[%%metaflag "-open", "Stdcompat"]

type ('a, 'b) eq = Eq : ('a, 'a) eq
(** Equality witness between types. *)

module type MonomorphicS = sig
  module Applicative : Modules.Applicative.S

  [%%meta Metapp.Sigi.of_list (List.init Traverse_meta.variable_count
    (fun i ->
      let ti = Traverse_meta.ti i in
      let ti_t = Traverse_meta.ti_t i in
      Metapp.Sigi.of_list [
        Ppxlib.Ast_helper.Sig.type_ Recursive
          [Ppxlib.Ast_helper.Type.mk (Metapp.mkloc ti);
            Ppxlib.Ast_helper.Type.mk (Metapp.mkloc ti_t)];
        Ppxlib.Ast_helper.Sig.value
          (Ppxlib.Ast_helper.Val.mk (Metapp.mkloc (Printf.sprintf "eq%d" i))
            [%t: ([%meta Traverse_meta.type_of_string ti] Applicative.t,
              [%meta Traverse_meta.type_of_string ti_t]) eq])]))]
end

type 'a desc =
  | A :
      [%meta Ppxlib.Ast_helper.Typ.package
        (Metapp.mkloc (Longident.Lident "MonomorphicS"))
        (List.flatten (List.init Traverse_meta.variable_count (fun i ->
          let ti = Traverse_meta.ti i in
          let ti_t = Traverse_meta.ti_t i in
          [(Traverse_meta.mklid ti, Ppxlib.Ast_helper.Typ.var ti);
            (Traverse_meta.mklid ti_t, Ppxlib.Ast_helper.Typ.var ti_t)])))] ->
      [%meta Traverse_meta.compose (fun i acc ->
        [%type: [%meta Ppxlib.Ast_helper.Typ.var (Traverse_meta.ti i)] *
          [%meta Ppxlib.Ast_helper.Typ.var (Traverse_meta.ti_t i)] *
          [%meta acc]]) [%t: unit]] desc

(** ['a t] {!type:Applicative.t} is a first-class representation for applicative
    functors.*)
type 'a t = unit -> 'a desc

module type InstanceS = sig
  module Applicative : Modules.Applicative.S

  val instance :
    [%meta Traverse_meta.mk_t (fun i ->
      Ppxlib.Ast_helper.Typ.var (Traverse_meta.ti i),
      [%type: [%meta Ppxlib.Ast_helper.Typ.var (Traverse_meta.ti i)] Applicative.t])]
end
