open Cgraph

let rec partition list pivot k =
  match list with
  | [] -> k ([], [])
  | tt :: qq ->
      partition qq pivot @@ fun (l1, l2) ->
      if_ (map2 tt pivot ( < )) (k (tt :: l1, l2)) (k (l1, tt :: l2))

let partition list pivot = partition list pivot

let rec qsort list k =
  match list with
  | [] -> k []
  | a :: ll ->
      partition ll a @@ fun (l1, l2) ->
      qsort l1 @@ fun l1 ->
      qsort l2 @@ fun l2 -> k (l1 @ a :: l2)

let qsort l = qsort l return

let construct len =
  let vars = List.init len (fun _ -> Var.create 0) in
  let inps = List.map var vars in
  (vars, qsort inps)

let (vars, compute, graph) =
  let list = [15; 12; 8; 5] in
  let (vars, graph) = construct (List.length list) in
  List.iter2 (fun var x -> Var.set var x) vars list ;
  let compute () = List.map get (get graph) in
  (vars, compute, graph)

(* let () =
 *   let oc = open_out "/tmp/graph0.dot" in
 *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

let sorted = compute ()

(* let () =
 *   let oc = open_out "/tmp/graph1.dot" in
 *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

(* let pp_sep fmtr () = Format.pp_print_char fmtr ',' *)

(* let () = Format.printf "%a@." Format.(pp_print_list ~pp_sep pp_print_int) sorted *)

let [v1; v2; v3; v4] = vars [@@ocaml.warning "-8"]

let () = Var.set v1 1

let () = Var.set v2 7

(* let () =
 *   let oc = open_out "/tmp/graph2.dot" in
 *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

let sorted = compute ()

(* let () =
 *   let oc = open_out "/tmp/graph3.dot" in
 *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

(* let () = Format.printf "%a@." Format.(pp_print_list ~pp_sep pp_print_int) sorted *)

let () = Var.set v1 54

let () = Var.set v2 0

(* let () =
 *   let oc = open_out "/tmp/graph4.dot" in
 *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

let sorted = compute ()

(* let () =
 *   let oc = open_out "/tmp/graph5.dot" in
 *   Internal.(to_dot ~mode:Full (ex graph) oc) *)

(* let () = Format.printf "%a@." Format.(pp_print_list ~pp_sep pp_print_int) sorted *)

let () = Var.set v1 0

let () = Var.set v2 1

let () = Var.set v3 2

let () = Var.set v4 3

let sorted = compute ()

(* let () = Format.printf "%a@." Format.(pp_print_list ~pp_sep pp_print_int) sorted *)

(* let () =
 *   let oc = open_out "/tmp/graph5.dot" in
 *   Internal.(to_dot ~mode:Full (ex graph) oc) *)
