module P =Plebeia.Internal
open P.Result (* for >>= *)
  
(* unoptimized tree *)
type t = 
  | Null
  | Leaf of P.Value.t
  | Tree of t
  | Node of t * t
    
type trail = Root | Treed of trail | Left of t * trail | Right of t * trail

type segment = P.Segment.t
type error = string
type value = P.Value.t
type context = unit

type cursor = t * trail

let get_node (t, _) = t
  
let rec of_plebeia_node : P.Context.t -> P.Node.node -> t = fun context -> function
  | Disk (i, wit) -> of_plebeia_node context (View (P.Node.load_node context i wit))
  | View n  -> 
      match n with
      | Bud (None, _, _) -> Tree Null
      | Bud (Some n, _, _) -> Tree (of_plebeia_node context n)
      | Internal (l, r, _, _) -> Node (of_plebeia_node context l,
                                          of_plebeia_node context r)
      | Leaf (v, _, _) -> Leaf v
      | Extender (seg, n, _, _) ->
          let rec aux n seg =
            match P.Segment.cut seg with
            | None -> n
            | Some (P.Segment.Left, seg) -> Node (aux n seg, Null)
            | Some (P.Segment.Right, seg) -> Node (Null, aux n seg)
          in 
          aux (of_plebeia_node context n) seg
  
let empty () = (Tree Null, Root) (* not just Null *)
  
let check_node (n, trail) =
  match n with
  | Tree n -> Ok (n, Treed trail)
  | _ -> Error "Start node is not Tree"

let subtree ntrail seg =
  let rec aux ((n, trail) as cur) = function
    | [] -> 
        begin match n with
        | Tree _ -> Ok cur
        | _ -> Error "Reached to non Tree"
        end
    | P.Segment.Left :: seg' ->
        begin match n with
        | Null -> Error "Null"
        | Leaf _ -> Error "Leaf"
        | Tree _ -> Error "Tree in middle"
        | Node (l, r) -> aux (l, Left (r, trail)) seg'
        end
    | P.Segment.Right :: seg' ->
        begin match n with
        | Null -> Error "Null"
        | Leaf _ -> Error "Leaf"
        | Tree _ -> Error "Tree in middle"
        | Node (l, r) -> aux (r, Right (l, trail)) seg'
        end
  in
  check_node ntrail >>= fun ntrail -> aux ntrail (P.Segment.to_sides seg)

(* Find the Tree above, cleaning Node (Null, Null) *)
let rec go_up_tree (n, trail) =
  let trim_null = function
    | Node (Null, Null) -> Null
    | t -> t
  in
  match trail with
  | Treed trail -> Ok (Tree n, trail)
  | Root -> Error "Root"
  | Left (r, trail) -> go_up_tree (trim_null (Node (n,r)), trail)
  | Right (l, trail) -> go_up_tree (trim_null (Node (l, n)), trail)
  
let parent ((n, _) as ntrail) =
  match n with
  | Tree _ -> go_up_tree ntrail
  | _ -> Error "not Tree"
  
let get_node_seg ntrail seg =
  let rec aux ((n, trail) as ntrail) = function
    | [] -> Ok ntrail
    | P.Segment.Left :: seg' ->
        begin match n with
        | Null -> Error "Null"
        | Leaf _ -> Error "Leaf"
        | Tree _ -> Error "Tree in middle"
        | Node (l, r) -> aux (l, Left (r, trail)) seg'
        end
    | P.Segment.Right :: seg' ->
        begin match n with
        | Null -> Error "Null"
        | Leaf _ -> Error "Leaf"
        | Tree _ -> Error "Tree in middle"
        | Node (l, r) -> aux (r, Right (l, trail)) seg'
        end
  in
  check_node ntrail >>= fun ntrail -> 
  aux ntrail seg

let get_value ntrail seg =
  let seg = P.Segment.to_sides seg in
  get_node_seg ntrail seg >>= function
  | (Leaf v, _) -> Ok v
  | _ -> Error "Not Leaf"

let alter ntrail seg f =
  let seg = P.Segment.to_sides seg in
  let rec aux (n, trail) = function
    | [] -> f n >>= fun v -> Ok (v, trail)
    | P.Segment.Left :: seg' ->
        begin match n with
        | Null -> aux (Null, Left (Null, trail)) seg'
        | Leaf _ -> Error "Leaf"
        | Tree _ -> Error "Tree in middle"
        | Node (l, r) -> aux (l, Left (r, trail)) seg'
        end
    | P.Segment.Right :: seg' ->
        begin match n with
        | Null -> aux (Null, Right (Null, trail)) seg'
        | Leaf _ -> Error "Leaf"
        | Tree _ -> Error "Tree in middle"
        | Node (l, r) -> aux (r, Right (l, trail)) seg'
        end
  in
  check_node ntrail >>= fun ntrail -> 
  aux ntrail seg >>= go_up_tree

let insert ntrail seg v = 
  let f = function
    | Null -> Ok (Leaf v)
    | _ -> Error "not Null"
  in
  alter ntrail seg f
    
let upsert ntrail seg v = 
  let f = function
    | Null | Leaf _ -> Ok (Leaf v)
    | _ -> Error "not Null nor Leaf"
  in
  alter ntrail seg f
    
let create_subtree ntrail seg = 
  let f = function
    | Null -> Ok (Tree Null)
    | _ -> Error "not Null"
  in
  alter ntrail seg f 

let delete ntrail seg = 
  let seg = P.Segment.to_sides seg in
  get_node_seg ntrail seg >>= function
  | ((Leaf _ | Tree _), trail) -> go_up_tree (Null, trail)
  | _ -> Error "Not Leaf nor Tree"

(* Graphviz's dot file format *)

let link ?label n1 n2 =
  match label with
  | None -> Printf.sprintf "%s -> %s;" n1 n2
  | Some l -> Printf.sprintf "%s -> %s [label=\"%s\"];" n1 n2 l

let null n = Printf.sprintf "%s [shape=point];" n
let leaf n value = Printf.sprintf "%s [label=%S];" n (P.Value.to_string value)
let tree n = Printf.sprintf "%s [shape=diamond, label=\"\"];" n
let node n = Printf.sprintf "%s [shape=circle, label=\"\"];" n
    
let of_node_aux cntr root =
  let rec aux : int -> t -> (string * string list * int) = fun cntr -> function
    | Null ->
        let n = Printf.sprintf "Null%d\n" cntr in
        (n, [null n], cntr+1)
    | Leaf value ->
        let n = Printf.sprintf "Leaf%d\n" cntr in
        (n, [leaf n value], cntr+1)
    | Tree node ->
        let n', s, cntr = aux cntr node in
        let n = Printf.sprintf "Tree%d" cntr in
        (n, 
         [tree n;
          link n n'
         ] @ s,
         cntr + 1)
    | Node (left, right) ->
        let ln, ls, cntr = aux cntr left in 
        let rn, rs, cntr = aux cntr right in 
        let n = Printf.sprintf "Node%d" cntr in
        (n,
         [ node n;
           link n ln ~label:"L";
           link n rn ~label:"R" ]
         @ ls @ rs,
         cntr + 1)
  in
  aux cntr root

let rec of_trail dst cntr = function
  | Root -> ([], cntr)
  | Left (r, trail) ->
      let n = Printf.sprintf "Node%d" cntr in
      let cntr = cntr + 1 in
      let r, ss, cntr = of_node_aux cntr r in
      let (ss', cntr) = of_trail n cntr trail in
      ([ node n;
         link n dst ~label:"L";
         link n r ~label:"R" ]
       @ ss @ ss',
       cntr)
  | Right (l, trail) ->
      let n = Printf.sprintf "Node%d" cntr in
      let cntr = cntr + 1 in
      let l, ss, cntr = of_node_aux cntr l in
      let (ss', cntr) = of_trail n cntr trail in
      ([ node n;
         link n l ~label:"L";
         link n dst ~label:"R" ]
       @ ss @ ss',
       cntr)
  | Treed trail ->
      let n = Printf.sprintf "Tree%d" cntr in
      let cntr = cntr + 1 in
      let (ss, cntr) = of_trail n cntr trail in
      ([ tree n;
         link n dst ]
       @ ss,
       cntr)

let make_digraph ss = "digraph G {\n" ^ String.concat "\n" ss ^ "\n}\n"

let dot_of_node root =
  let (_name, ss, _cntr) = of_node_aux 0 root in
  make_digraph ss

let dot_of_cursor (node, trail) =
  let (n, ss, cntr) = of_node_aux 0 node in
  let ss', _ = of_trail n cntr trail in
  let s = Printf.sprintf "cursor [shape=point, label=\"\"]; cursor -> %s [style=bold];" n in
  make_digraph (s :: ss @ ss')
