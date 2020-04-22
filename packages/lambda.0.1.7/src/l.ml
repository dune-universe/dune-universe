type term = 
	| Var of string 
	| Abs of (string * term) 
	| App of term * term
;;

(* type token = 
  | TVAR of string
  | TPAREN_LEFT
  | TPAREN_RIGHT
  | TLAMBDA
  | TDOT
  | TEOF
;; *)

let rec to_string t = match t with
    Var x -> x
  | Abs (x,t') -> "(λ" ^ x ^ "." ^ to_string t' ^ ")" (* λ *)
  | App (t0,t1) -> "(" ^ (to_string t0) ^ " " ^ (to_string t1) ^ ")"
;;

  
let var_alphabet = "xyzfnwtuvpqrsabcdeghilmo";;
let var_alphabet_len = String.length var_alphabet;;


let get_var_bound n =  String.get var_alphabet n |> String.make 1;;

let get_var v = 
  let rec in_v r =
    match r with
    | n when n < String.length var_alphabet -> String.get var_alphabet r |> String.make 1
    | _ -> 
      (String.get var_alphabet @@ r mod var_alphabet_len |> String.make 1) ^
      (in_v @@ r / var_alphabet_len)
  in in_v v
;;


(** Set definition *)
type 'a set = Set of 'a list;;
 
let emptyset = Set [];;
 
let rec member x s = match s with
    Set [] -> false
  | Set (y::s') -> x=y || (member x (Set s'));;
 
let rec union s t = match s with
    Set [] -> t
  | Set (x::s') -> (match union (Set s') t with Set t' ->
      if member x t then Set t' else Set (x::t'));;
 
let rec diff s x = match s with
    Set [] -> s
  | Set (y::s') -> (match diff (Set s') x with Set t' ->
      if x=y then Set s' else Set (y::t'));;


(** Return a set of free variables *)
let rec fv t = match t with
    Var x -> Set [x]
  | App(t0,t1) -> union (fv t0) (fv t1)
  | Abs(x,t0) -> diff (fv t0) x;;

let fv_l t = match fv t with Set l -> l;;

(** Substitution *)
let vcount = ref(-1);;
let gensym = fun () -> vcount := !vcount +1; get_var !vcount;; (* "x" ^ string_of_int (!vcount);; *)
 
let rec subst x t' t = match t with
    Var y -> if x=y then t' else Var y
  | App(t0,t1) -> App(subst x t' t0, subst x t' t1)
  | Abs(y,t0) when y=x -> Abs(x,t0)
  | Abs(y,t0) when y!=x && not (member y (fv t')) -> Abs(y, subst x t' t0)
  | Abs(y,t0) when y!=x && member y (fv t') -> 
      let z = gensym() in Abs(z,subst x t' (subst z (Var y) t0));;

(** α-conversion *)
let rec alfa_conversion a b t = match t with 
	  Var x -> if x=a then Var b else Var x
  | Abs(x, t') -> Abs((if x=a then b else a), alfa_conversion a b t')
  | App(t', t'') -> App(alfa_conversion a b t', alfa_conversion a b t'')
;;

(** η-conversion *)
let eta_conversion t = match t with
| Abs(x, App(m, Var y)) when x=y && not (member x @@ fv m) -> m
| _ -> t
;;
      
(** β-reduction *)
let is_redex t = match t with 
    App(Abs(x,t0),t1) -> true
  | _ -> false;;
 
let rec has_redex t = match t with 
    Var x -> false
  | Abs(x,t') -> has_redex t'
  | App(t0,t1) -> is_redex t || has_redex t0 || has_redex t1;;
 
 
let rec reduce1 t = if not (has_redex t) then t else match t with
    Abs(x,t') -> Abs(x,reduce1 t')
  | App(Abs(x,t0),t1) -> subst x t1 t0 
  | App(t0,t1) -> if has_redex t0 then App(reduce1 t0,t1) else App(t0,reduce1 t1);;
 
let reduce k t =
  let rec reduce' k t = if k=0 then t else let t' = reduce1 t in reduce' (k-1) t' in
  vcount := 0;
  reduce' k t
;;

let reduce_fix t =
  let rec reduce_fix' t = let t' = reduce1 t in if t'=t then t' else reduce_fix' t' in
  vcount := 0;
  reduce_fix' t
;;

let reduce_fix_timeout ?(n=128) t = 
  let rec subst' x t' t n' = if n' = 0 then t else match t with
    Var y -> if x=y then t' else Var y
  | App(t0,t1) -> App(subst' x t' t0 (n'-1), subst' x t' t1 (n'-1))
  | Abs(y,t0) when y=x -> Abs(x,t0)
  | Abs(y,t0) when y!=x && not (member y (fv t')) -> Abs(y, subst' x t' t0 (n'-1))
  | Abs(y,t0) when y!=x && member y (fv t') -> 
    let z = gensym() in Abs(z,subst' x t' (subst' z (Var y) t0 (n'-1)) (n'-1))
  in
  let rec reduce1' t n' = if not (has_redex t) || n' = 0 then t else match t with
    Abs(x,t') -> Abs(x,reduce1' t' (n'-1))
  | App(Abs(x,t0),t1) -> subst' x t1 t0 (n'-1)
  | App(t0,t1) -> if has_redex t0 then App(reduce1' t0 (n'-1),t1) else App(t0,reduce1' t1 (n'-1))
  in  
  let rec reduce_fix' t n' = 
  	let t' = reduce1' t (n'-1) in if t'=t || n' = 0 then t' else reduce_fix' t' (n'-1) 
  in
  vcount := 0;
  reduce_fix' t n
;;


let rec len t = match t with
  Var _ -> 1
| Abs (_, t') -> 1 + len t'
| App (t', t'') -> 1 + len t' + len t''
;;