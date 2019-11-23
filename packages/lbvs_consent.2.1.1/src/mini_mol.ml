(* mini molecule module *)

module A = BatArray
module IntSet = BatSet.Int
module L = MyList
module StringMap = BatMap.String

type t = { name: string;
           graph: Node.t array }

let get_name m = m.name

let get_graph m = m.graph

let create name graph =
  { name; graph }

let to_string (m: t): string =
  let buff = Buffer.create 80 in
  Buffer.add_string buff m.name;
  Buffer.add_char buff '\n';
  A.iter (fun n ->
      Buffer.add_string buff (Node.to_string n);
      Buffer.add_char buff '\n';
    ) m.graph;
  Buffer.contents buff

let get_typ (m: t) (i: int) =
  Node.get_typ m.graph.(i)

let get_succs (m: t) (i: int) =
  Node.get_succs m.graph.(i)

let mop2d_encode (m: t): Mop2d_env.t list =
  (* canonicalized count of atom types *)
  let count_typs (typs: string list): (string * int) list =
    let res =
      L.fold_left (fun acc typ ->
          try
            StringMap.modify typ ((+) 1) acc
          with Not_found ->
            StringMap.add typ 1 acc
        ) StringMap.empty typs
    in
    L.of_enum (StringMap.enum res)
  in
  let node_encode (n_i: int): Mop2d_env.t =
    let n = m.graph.(n_i) in
    let l0 = Node.get_typ n in
    let succs = Node.get_succs n in
    let succs_typs =
      IntSet.fold (fun i acc ->
          get_typ m i :: acc
        ) succs []
    in
    let l1 = count_typs succs_typs in
    let succs_succs =
      let res =
        IntSet.fold (fun i acc ->
            let succs = get_succs m i in
            IntSet.union succs acc
          ) succs IntSet.empty
      in
      IntSet.diff res (IntSet.union (IntSet.singleton n_i) succs)
    in
    let succs_succs_typs =
      IntSet.fold (fun i acc ->
          get_typ m i :: acc
        ) succs_succs []
    in
    let l2 = count_typs succs_succs_typs in
    (l0, l1, l2)
  in
  A.fold_lefti (fun acc i _ ->
      node_encode i :: acc
    ) [] m.graph
