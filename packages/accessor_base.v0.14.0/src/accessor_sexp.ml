open! Base
open! Import

type t = Sexp.t =
  | Atom of string
  | List of t list
[@@deriving accessors]

let children = [%accessor list @> Accessor_list.each]

let tuple2 =
  [%accessor
    Accessor.variant
      ~match_:(function
        | Sexp.List [ a; b ] -> First (a, b)
        | (List _ | Atom _) as sexp -> Second sexp)
      ~construct:(fun (a, b) -> Sexp.List [ a; b ])]
;;

let field name =
  children
  @> tuple2
  @> Accessor_tuple2.sndi
  @> Accessor.filter_map_index (fun (n :: i) ->
    if Sexp.equal (Atom name) n then Some i else None)
;;

let variant name = list @> Accessor_list.prefixed [ Atom name ] ~equal:Sexp.equal

let atoms =
  let rec traverse =
    let open Accessor.Many.Let_syntax in
    function
    | Atom atom ->
      let%map atom = Accessor.Many.access atom in
      Atom atom
    | List sexps ->
      let%map sexps = Accessor.Many.all (List.map sexps ~f:traverse) in
      List sexps
  in
  [%accessor Accessor.many traverse]
;;

let conv (type a) (module A : Sexpable.S with type t = a) =
  Accessor.variant
    ~match_:(fun sexp ->
      match A.t_of_sexp sexp with
      | a -> First a
      | exception _ -> Second sexp)
    ~construct:A.sexp_of_t
;;

let conv_strict (type a) (module A : Sexpable.S with type t = a) =
  Accessor.isomorphism ~get:A.t_of_sexp ~construct:A.sexp_of_t
;;
