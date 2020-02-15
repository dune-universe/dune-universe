(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol
open Owl_symbolic_graph
open Owl_symbolic_operator
open Owl_symbolic_cas_add
open Owl_symbolic_cas_assumption

(* Temporay helper function *)

let int_sym v =
  let sym = Owl_symbolic_ops_generator.Int.create v in
  Owl_symbolic_symbol.Int sym


(* Main entry *)

let rec _to_canonical node =
  let sym = Owl_graph.attr node in
  match sym with
  | Int _      -> canonical_int node
  | Float _    -> canonical_float node
  | Variable _ -> canonical_var node
  | Add _      -> canonical_add node
  | Mul _      -> canonical_mul node
  | Div _      -> canonical_div node
  | _          -> failwith "error: _to_canonical"


and canonical_int node = node

and canonical_float node = node

and canonical_div node =
  if not (is_rational node)
  then failwith "Non-rational div not implemented."
  else (
    let parents = Owl_graph.parents node in
    let p = parents.(0) |> _to_canonical in
    let q = parents.(1) |> _to_canonical in
    let get_rat p =
      match Owl_graph.attr p with
      | Int _   ->
        let v = Owl_symbolic_symbol.int_value (Owl_graph.attr p) in
        v, 1
      | Float _ ->
        let v = Owl_symbolic_symbol.float_value (Owl_graph.attr p) in
        Owl_symbolic_utils.float_as_ratio v
      | _       -> failwith "canonical_rat: unsupported input type"
    in
    let pn, pd = get_rat p in
    let qn, qd = get_rat q in
    let p = pn * qd in
    let q = qn * pd in
    (* now both p and q are integers *)
    let p, q =
      if q = 0
      then failwith "canonical_rat: Division_by_zero"
      else if q < 0
      then -p, -q
      else (
        let gcd =
          Owl_symbolic_utils.gcd (Owl_base_maths.abs (float_of_int p) |> int_of_float) q
        in
        p / gcd, q / gcd)
    in
    if q == 1
    then (
      set_sym node (int_sym p);
      Owl_graph.remove_edge parents.(0) node;
      Owl_graph.remove_edge parents.(1) node;
      Owl_graph.remove_node parents.(0);
      Owl_graph.remove_node parents.(1);
      node)
    else (
      set_sym parents.(0) (int_sym p);
      set_sym parents.(1) (int_sym q);
      node)
    (* !!!!! TODO: but you have to remove the parents ?!!!!! *)
    (* let x = Owl_graph.attr node in 
    x.p <- p;
    x.q <- q *))


and canonical_var node = node

(** Extract common factor  *)
and canonical_add node =
  let parents = Owl_graph.parents node in
  let parents = Array.map _to_canonical parents in
  (* TODO: expand dict dynamically *)
  let terms = Hashtbl.create 20 in
  (* express each parent term in the form of number*term *)
  Array.iter
    (fun p ->
      let ap = Owl_graph.attr p in
      match ap with
      | Zero _ -> ()
      | _      ->
        let num, term =
          match ap with
          | Mul _ -> Owl_symbolic_cas_tree.extract_mul_coeff p
          | Add _ -> int 1, p
          (* TODO: doesn't consider multiple add/mul arguments *)
          | _ -> int 1, p
        in
        (try
           let nums = Hashtbl.find terms term in
           let new_num = cas_add num nums |> _to_canonical in
           Hashtbl.replace terms term new_num
         with
        | Not_found -> Hashtbl.add terms term num))
    parents;
  let new_parents = ref [] in
  Hashtbl.iter
    (fun term num ->
      let p =
        match Owl_graph.attr num with
        | Zero _ -> int 0
        (* | One _  -> new_parents := List.append !new_parents [ term ] *)
        | _ ->
          (match Owl_graph.attr term with
          | Add _ -> mul num term
          | _     ->
            let new_op = mul num term in
            (* _to_canonical new_op; *)
            new_op)
      in
      new_parents := List.append !new_parents [ p ])
    terms;
  (* Sort parents by symbolic order *)
  let new_parents =
    List.sort
      (fun x y ->
        let sx = Owl_graph.attr x in
        let sy = Owl_graph.attr y in
        Owl_symbolic_symbol.compare sx sy)
      !new_parents
  in
  (* Update parents of node *)

  (* Owl_graph.iter_ancestors ~order:DFS ~traversal:PostOrder (fun sym_node ->
      let a = Owl_graph.attr sym_node in
      Printf.fprintf stderr "%s\n" (Owl_symbolic_symbol.name a)
  ) [|node|]; *)

  (* let children = children node in 
  Array.iter
    (fun c ->
      remove_edge node c;)
    children;
  remove_node node;
  let new_parents = Array.of_list new_parents in
  connect_ancestors new_parents children;
  Array.iter (fun c -> Array.iter (fun p -> connect_descendants [| p |] [| c |]) new_parents ) children; *)

  (* Array.iter
    (fun p ->
      remove_edge p node;
      remove_node p)
    parents;

  Owl_log.info "Parents length: %d" (List.length new_parents);
  let new_parents = Array.of_list new_parents in
  connect_ancestors new_parents [| node |];
  Array.iter (fun p -> connect_descendants [| p |] [| node |]) parents; *)
  List.nth new_parents 0


and canonical_mul node = node

let canonical_form sym_graph =
  let output = Owl_symbolic_graph.get_output_nodes sym_graph in
  let nodes = Array.map _to_canonical output in
  make_graph nodes sym_graph.name
