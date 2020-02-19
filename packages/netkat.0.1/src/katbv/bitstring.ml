open Base


(* AF: the table [H] represents a bitvector [v] where [v_i]=1 iff [(i, ())] is
   in [H] *)
(* type t = (int, unit) Hashtbl.t *)

(* type t = unit Hashtbl.M(Int) [@@deriving compare] *)

module T = struct
  type t = unit Hashtbl.M(Int).t [@@deriving sexp]

  let max (t:t) = 
    Hash_set.(max_elt ~compare:compare_int (of_hashtbl_keys t))
  
  let max_exn (t:t) =
    match max t with
    | None -> 0
    | Some i -> i
  
  let nth = Hashtbl.mem

  let xor (t1:t) (t2:t) = 
    let smaller, larger = 
      if Hashtbl.(max_exn t1 > max_exn t2) then t2, t1 else t1, t2 in
    Hashtbl.filteri larger ~f:(fun ~key ~data:() -> not (Hashtbl.mem smaller key))

  let compare v1 v2 = 
    match max (xor v1 v2) with 
    | None -> 0
    | Some i -> if nth v1 i then 1 else ~-1
end
include T
include Comparator.Make(T)

let zero = Hashtbl.create (module Int)

let of_bool_list lst = 
  let len = List.length lst in
  let lst' = List.foldi lst ~init:[] ~f:(fun i acc x -> 
    if x then (len-i-1, ())::acc else acc
  ) in
  Hashtbl.of_alist_exn (module Int) lst'

let of_int_list lst = Hashtbl.group (module Int) ~get_key:(fun i -> i)
  ~get_data:(fun _ -> ()) ~combine:(fun _ _ -> ()) lst


let of_binary s =
  String.fold s ~init:[] ~f:(fun acc c ->
    match Char.get_digit c with
    | Some i -> i::acc
    | None -> acc
  )
  |> List.foldi ~init:[] ~f:(fun i acc b ->
    if b = 1 then i::acc else acc
  )
  |> of_int_list

let of_ternary s =
  let len = String.length s in
  let z,n = String.foldi s ~init:([],[]) ~f:(fun i (z',n') c ->
    match Char.get_digit c with
    | Some 0 -> ((len-i-1)::z',n')
    | Some 1 -> (z',(len-i-1)::n')
    | _ -> (z',n')
  )
  in
  (of_int_list z),(of_int_list n)

let test v i bl = 
  Katbb_lib.Ast.{var = v ^ Int.to_string i; value = bl}

let ( ** ) (t1:t) (t2:t) =
  let smaller, larger = 
    if Hashtbl.(length t1 > length t2) then t2, t1 else t1, t2 in
  Hashtbl.filteri smaller ~f:(fun ~key ~data:() -> Hashtbl.mem larger key)

let ( ++ ) = Hashtbl.merge ~f:(fun ~key:_ _ -> Some ())

let ( -- ) (t1:t) (t2:t) = 
  Hashtbl.filteri t1 ~f:(fun ~key ~data:() -> not (Hashtbl.mem t2 key))

let is_zero = Hashtbl.is_empty

let equal (t1:t) (t2:t) = Hashtbl.equal t1 t2 (fun () () -> true)

let min (t:t) = 
  Hash_set.(min_elt ~compare:compare_int (of_hashtbl_keys t))

(* [build_list ~f lst i n] is [e_jm; e_jm-1;...; e_j1] if 
   [(f jm); (f jm-1); ...; (f j1)] is [Some e_jm; Some e_jm-1; ...; Some e_j1]
   where jm, jm-1,..., j1 satisfies n >= jm > ... > j1 >= i and f k = None 
   for all k such that k is not in the list jm, jm-1,..., j1 *)
let rec build_list (lst:'a list) (i:int) (n:int) ~(f:int -> 'a option) =
  if i < n then
    match (f i) with
    | None -> build_list ~f lst (i+1) n
    | Some e -> build_list ~f (e::lst) (i+1) n
  else lst


(* [build_clause v a m lst init_bool init_val] is the Boolean KAT+B!
   expression which is the disjunction of the clauses: 
   x_m=a_m ∧ x_{m-1} = a_{m-1} ∧ ... ∧ x_i=a_i ∧ x_{i-1}=init_bool
   for all i-1 in [lst] and the clause
   x_{m}=a_{m} ∧ ... ∧ x_0=a_0 *)
let build_clause v a m lst init_bool =
  let rec build_clause a lst formula =
    match lst with
    | h::t -> 
      let term = Kat.Ast.Test (test v h init_bool) in
      let clause = build_list [term] (h+1) (m+1) ~f:(fun i ->
        Some (Kat.Ast.Test (test v i (nth a i)))
      ) in
      let formula' = Kat.Optimize.disj formula (Kat.Optimize.big_conj clause) in
      build_clause a t formula'
    | [] -> formula
  in
  let init_clause = build_list [] 0 (m+1) ~f:(fun i ->
    Some (Kat.Ast.Test (test v i (nth a i)))
  ) in
  build_clause a lst (Kat.Optimize.big_conj init_clause)

(* let build_clause v a max lst init_bool init_val =
  let rec build_clause a max lst formula =
    match lst with
    | h::t ->
      (* build clause 
        x_0=a_0 ∧ ... ∧ x_{max-1} = a_{max-1} ∧ a_max = 0 ∧ x_max = 1 for v>=a
      *)
      (* then recursively call with max as hd of lst and lst as list tl *)
      let term = Kat.Ast.Test (test v max init_bool) in
      let clause = build_list [term] 0 (max-1) ~f:(fun i -> 
        Some (Kat.Ast.Test (test v i (Hashtbl.mem a i)))
      ) in
      let formula' = Kat.Optimize.disj formula (Kat.Optimize.big_conj clause) in
      Hashtbl.remove a max;
      build_clause a h t formula'
    | [] -> formula
  in
  let init_clause = build_list [] 0 init_val ~f:(fun i ->
    Some (Kat.Ast.Test (test v i (Hashtbl.mem a i)))
  ) in
  build_clause a max lst (Kat.Optimize.big_conj init_clause) *)

let lower_bound (v:string) (a:t) : Katbb_lib.Ast.bexp =
  (* lst is a sorted list of values not in a *)
  match max a with
  | None -> Kat.Ast.True
  | Some m -> 
    let lst = build_list [] 0 m ~f:(fun i -> 
      if Hashtbl.mem a i then None else Some i
    ) in
    build_clause v a m lst true

let upper_bound (v:string) (b:t) : Katbb_lib.Ast.bexp =
  let lst = Hashtbl.keys b in
  match max b with 
  | None -> build_clause v b 0 [] false
  | Some m -> build_clause v b m lst false

let build_term_list (v:string) (z:t) (n:t) h =
  let m = Int.max (max_exn z) (max_exn n)
  in
  build_list [] 0 (m+1) ~f:(fun i -> 
    if Hashtbl.mem z i then Some (h false i)
    else if Hashtbl.mem n i then Some (h true i)
    else None
  )

let to_string bv =
  let m = max_exn bv in
  build_list [] 0 (m+1) ~f:(fun i -> 
    if Hashtbl.mem bv i then Some "1"
    else Some "0"
  )
  |> String.concat