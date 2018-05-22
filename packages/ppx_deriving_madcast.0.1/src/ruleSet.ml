
(* more than a set, rules are in fact stored in a semilattice *)

open Parsetree

type cell =
  { rule : Rule.t ;
    mutable higher : cell list ;
    mutable level : int }

let make_cell ?(level=(-1)) ?(higher=[]) rule =
  { rule ; higher ; level }

let identity =
  let name = "'a -> 'a" in
  let matcher (itype, otype) =
    if Parsetree_utils.equal_core_type itype otype
    then Some []
    else None
  in
  let builder casts =
    assert (casts = []);
    [%expr fun x -> x]
  in
  make_cell (Rule.make ~name ~matcher ~builder ())

module SMap = Map.Make(String)

let cells : cell SMap.t ref = ref (SMap.singleton (Rule.name_ identity.rule) identity)

let lookup_cell rule =
  SMap.find (Rule.name_ rule) !cells

let lookup name =
  (SMap.find name !cells).rule

let register ?(applies_before=[]) ?(applies_after=[]) rule =
  let cell = make_cell ~higher:(List.map lookup_cell applies_before) rule in
  cells := SMap.add (Rule.name_ rule) cell !cells;
  identity.higher <- cell :: identity.higher;
  List.iter
    (fun rule' ->
      let cell' = lookup_cell rule' in
      cell'.higher <- cell :: cell'.higher)
    applies_after

let fill_levels () =
  let rec fill_level i lower cell =
    assert (not (List.mem cell lower));
    if cell.level < i then
      cell.level <- i;
    List.iter (fill_level (i+1) (cell :: lower)) cell.higher
  in
  SMap.iter (fun _ cell -> cell.level <- -1) !cells;
  fill_level 0 [] identity

let fold_by_priority f x =
  let rec fold x level = function
    | [] -> x
    | cells_at_that_level ->
       let x' = f (List.map (fun cell -> cell.rule) cells_at_that_level) x in
       let next_level = level + 1 in
       let cells_at_next_level =
         List.map (fun cell -> cell.higher) cells_at_that_level
         |> List.flatten
         |> List.filter (fun cell -> cell.level = next_level)
       in
       fold x' next_level cells_at_next_level
  in
  fill_levels ();
  fold x identity.level [identity]
