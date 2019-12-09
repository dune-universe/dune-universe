(* open Remu_ts.Comm *)
open Comm

type rowt =
  | RowCons of string * t * rowt
  | RowPoly of t
  | RowMono
  [@@deriving show  { with_path = false }]

and t =
  | App      of t * t
  | Arrow    of t * t
  | Var      of int
  | Nom      of int
  | Fresh    of string
  | Tuple    of t list
  | Forall   of string list * t
  | Record   of rowt
  | Implicit of t * t
  [@@deriving show  { with_path = false }]

let (|->) a b = Arrow(a, b)
let (<||) a b = App(a, b)
let record xs = List.fold_right (fun (k, v) b -> RowCons(k, v, b)) xs
let record_of_map map = Map.foldi (fun k v b -> RowCons(k, v, b)) map


type tctx = {
  store : (int, t) map;
  qualns  : (int, string) map; (* qualified names for nominal types *)
} [@@deriving show {with_path = false}]

let empty_tctx = {store=Map.empty; qualns=Map.empty}

let previsit (f : 'ctx -> t -> ('ctx * t)) : 'ctx -> t -> t =
  let rec visit_t ctx' root =
    let (ctx, root) = f ctx' root in
    let rec eval_t node = visit_t ctx node
    and eval_row root =
      match root with
      | RowCons(k, t, row) -> RowCons(k, eval_t t, eval_row row)
      | RowMono            -> RowMono
      | RowPoly t          -> RowPoly(eval_t t)
    in
    match root with
    | Var _ | Nom _ | Fresh _ -> root
    | App(a, b)               -> App(eval_t a, eval_t b)
    | Arrow(a, b)             -> Arrow(eval_t a, eval_t b)
    | Tuple xs                -> Tuple(List.map eval_t xs)
    | Implicit(wit, t)        -> Implicit(eval_t wit, eval_t t)
    | Forall(ns, t)           -> Forall(ns, eval_t t)
    | Record rowt             -> Record(eval_row rowt)
  in visit_t


let visit_check (f : t -> bool) : t -> bool =
  let rec eval_t root =
    f root &&
    let rec eval_row root =
      match root with
      | RowCons(_, t, row) -> eval_t t && eval_row row
      | RowMono            -> true
      | RowPoly t          -> eval_t t
    in
    match root with
    | Var _ | Nom _ | Fresh _ -> true
    | App(a, b)               -> eval_t a && eval_t b
    | Arrow(a, b)             -> eval_t a && eval_t b
    | Tuple xs                -> List.for_all eval_t xs
    | Implicit(wit, t)        -> eval_t wit && eval_t t
    | Forall(_, t)            -> eval_t t
    | Record rowt             -> eval_row rowt
  in eval_t

exception IllFormedType of string
exception UnboundTypeVar of string
exception RowFieldMismatch of string
exception RowFieldDuplicatedInfer of string

module type TState = sig

  val global : tctx ref
  val load_tvar : int -> t
  val mut_tvar : int -> t -> unit
  val mut_tnom : int -> string -> unit

  val fresh : (string, t) map -> t -> t

  val new_tvar : unit -> t
  val new_type : string -> t

  val tvar_of_int : int -> t
  val int_of_tvar : t -> int option

  val tnom_of_int : int -> t
  val typeid      : t -> int option

  val occur_in : int -> t -> bool
  val prune : t -> t
  val unify : t -> t -> bool
  val extract_row: rowt -> (string, t) map * t option

end

let crate_tc : tctx -> (module TState) =
  fun global ->
  (module struct
      let global = ref global

      let load_tvar i = Map.find i (!global).store

      let mut_tvar i a =
        let up = fun _ -> Some a in
        let tctx = !global in
        global := {tctx with store=Map.modify_opt i up tctx.store}

      let mut_tnom i s =
        let up = fun _ -> Some s in
        let tctx = !global in
        global := {tctx with qualns=Map.modify_opt i up tctx.qualns}

      let fresh =
        let visit_func freshmap = function
            | Fresh s as a -> freshmap <.> Map.find_default a s freshmap
            | Forall(ns, _) as a -> List.fold_right Map.remove ns freshmap <.> a
            | a -> freshmap <.> a
        in fun freshmap ty -> previsit visit_func freshmap ty

      let new_tvar () =
        let tctx = !global in
        let vid = Map.cardinal tctx.store in
        let tvar = Var vid in
        global := {tctx with store = Map.add vid tvar tctx.store}; tvar

      let new_type typename =
        let tctx = !global in
        let nid = Map.cardinal tctx.qualns in
        global := {tctx with qualns = Map.add nid typename tctx.qualns}; Nom nid

      let tvar_of_int i = Var i

      let tnom_of_int i = Nom i

      let int_of_tvar = function
          | Var i -> Some i
          | _     -> None

      let typeid = function
          | Nom i -> Some i
          | _     -> None

      let occur_in i ty =
        if int_of_tvar ty = Some i then
          false
        else
        let visit_func = function
          | Var i' when i = i' -> false
          | _ -> true
        in not @@ visit_check visit_func ty

      let rec prune x =
        let vfunc () a = (), match a with
          | Var i ->
            begin
              match load_tvar i with
              | Var i' when i' = i -> a
              | a ->
                let t = prune a in
                mut_tvar i t; t
            end
          | _ -> a
        in previsit vfunc () x

      let extract_row =
        let rec extract_row fields =
          function
          | RowCons(k, _, _) when Map.mem k fields ->
            raise @@ RowFieldDuplicatedInfer k
          | RowCons(k, v, rowt) ->
            let fields = Map.add k v fields in
            extract_row fields rowt
          | RowMono -> (fields, None)
          | RowPoly (Record rowt) ->
            extract_row fields rowt
          | RowPoly t -> (fields, Some t)
        in extract_row Map.empty

      let rec unify lhs rhs = match prune lhs, prune rhs with
        | Nom a, Nom b -> a = b
        | Var a, Var b when a = b -> true
        (* This rule produces value restriction *)
        | (Forall _ as a), b -> unify b a
        | a, Forall(ns, poly) ->
          let freemap =
            let fn a =
              let tvar = new_tvar() in
              a, tvar
            in Map.of_enum @@ List.enum @@ List.map fn ns
          in
          unify a @@ fresh freemap poly
        | Var a, b ->
          if occur_in a b
          then raise @@ IllFormedType "a = a -> b"
          else mut_tvar a b; true
        | a, (Var _ as b) -> unify b a
        | (_, Fresh s) | (Fresh s, _) -> raise @@ UnboundTypeVar s
        | Implicit(a1, b1), Implicit(a2, b2) ->
          unify a1 a2 && unify b1 b2
        | Implicit(_, a), b
        | a, Implicit(_, b) -> unify a b
        | Arrow(a1, r1), Arrow(a2, r2) ->
          unify a1 a2 && unify r1 r2
        | App(f1, arg1), App(f2, arg2) ->
          unify f1 f2 && unify arg1 arg2
        | Tuple xs1, Tuple xs2 ->
          List.for_all2 unify xs1 xs2
        | Record a, Record b ->
          (* let rec unify_has_field record_t fn fty =
            (* fn: field name; fty: field type *)
            (* may produce a new record_t *)
            match record_t with
            | Var _ ->
              let ex = new_tvar() in
              let ex = Record(fn, fty, ExtRef ex)
              in unify record_t ex
            | Record(k, v, ex) when k = fn -> unify v fty
            | Record(_, _, ExtRef ex)      -> unify_has_fiel ex fn fty
            (* not row-polymorphic and given field not found *)
            | Record (_, _, Mono)          -> false
          in *)
          let (m1, ex1) = extract_row a in
          let (m2, ex2) = extract_row b in
          let common_keys =
            Map.intersect (fun _ _ -> ()) m1 m2
            |> Map.keys
            |> List.of_enum
          in
          let only_by1 = Map.diffkeys m1 common_keys in
          let only_by2 = Map.diffkeys m2 common_keys in
          let check_align key = unify (Map.find key m1) (Map.find key m2)
          in
          List.for_all check_align common_keys  &&
          let rec row_check row1 row2 only_by1 only_by2 =
            match (row1, row2) with
            | None, None -> Map.is_empty only_by1 && Map.is_empty only_by2
            | Some _, None -> row_check row2 row1 only_by2 only_by1
            | None, Some row2 ->
              (* only_by1 == {only_by2 | row2}
                where
                  only_by1 \cap \only_by2 = \emptyset,
                therefore,
                  only_by2 = \emptyset,
                  row2 = only_by1
              *)
              Map.is_empty only_by2 &&
              unify row2 @@ Record (record_of_map only_by1 RowMono)
            | Some row1, Some row2 ->
              (*
                {only_by1|row1} == {only_by2|row2},
                where
                  only_by1 \cap \only_by2 = \emptyset,
                therefore,
                  forall type in only_by2. type in row1, and
                  forall type in only_by1. type in row2,
                therefore,
                  t1 = {only_by1 \cup only_by2|row} = t2,
                  {only_by1|row} = row2
                  {only_by2|row} = row1
              *)
              let polyrow = RowPoly (new_tvar()) in
              let ex2 = Record(record_of_map only_by1 polyrow) in
              let ex1 = Record(record_of_map only_by2 polyrow) in
              unify row1 ex1 && unify row2 ex2
          in row_check ex1 ex2 only_by1 only_by2
      | _ -> false
  end: TState)

let copy_tc : (module TState) -> (module TState) =
  fun (module M1) -> crate_tc !M1.global
