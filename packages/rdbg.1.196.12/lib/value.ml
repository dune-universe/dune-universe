(* Time-stamp: <modified the 27/04/2018 (at 11:04) by Erwan Jahier> *)
(*-----------------------------------------------------------------------
**
** File: value.ml
** Main author: Erwan Jahier
*)

(* exported *)
type num = I of Num.num | F of float
type t = B of bool | N of num


(****************************************************************************)
(* Pretty printing *)
(* necessary for OfIdent *)

let precision = 6 (* XXX *)

(* exported *)
let to_string e =
  match e with
    | N(I(i)) ->((Num.string_of_num i) ^ " ")
    | N(F(f)) -> ((Mypervasives.my_string_of_float f precision) ^ " ")

    | B(true)  -> "T "
    | B(false) -> "F "
let to_string2 e =
  match e with
    | N(I(i)) ->((Num.string_of_num i) ^ " ")
    | N(F(f)) -> ((string_of_float f) ^ " ")
    | B(true)  -> "T "
    | B(false) -> "F "

let list_to_string l sep = 
	String.concat sep (List.map to_string l)

(* exported *)
let print oc e = output_string oc (to_string e)


let (to_data_val : t -> Data.v) =
  function
      B b -> Data.B b
    | N(I i)-> Data.I (Mypervasives.int_of_num i)
    | N(F f)-> Data.F f

let (from_data_val : Data.v -> t) =
  function
      Data.B b -> B b 
    | Data.I i -> N(I (Num.num_of_int i))
    | Data.F f -> N(F f) 
    | Data.U -> failwith "undefined variable"
	
(* exported *)
let (num_value_to_string : num -> string) =
  fun n ->
    match n with
      | I(i) -> Num.string_of_num i
      | F(f) -> Mypervasives.my_string_of_float f precision

(* MAP string -> Value.t *)

type name = string

module IdentMap  = struct
  include Map.Make(
   struct
      type t = name
      let compare = compare
   end
   )
end

(* Value.OfIdent *)

module OfIdent = struct
	type value = t
	let value_to_string = to_string
   type t = value IdentMap.t
   let empty:t = IdentMap.empty
   let get (n2v:t) (n:name) = IdentMap.find n n2v
   let add (n2v:t) ((n,v):name * value) = IdentMap.add n v n2v
   let add2 (n2v:t) (n : name) (v:value) = IdentMap.add n v n2v
   let add_list (n2v:t) (l:(name * value) list) = List.fold_left add n2v l
   let add_list2 (n2v:t) (nl: name list) (vl: value list) =
		try List.fold_left2  add2 n2v nl vl
      with _ -> assert false
   let from_list (l:(name * value) list) = List.fold_left add empty l
   let union (x1:t) (x2:t) = IdentMap.fold (fun n v x -> add x (n,v)) x1 x2
   let support (x:t) = IdentMap.fold (fun n v acc -> n::acc) x []
   let partition f (x:t) = IdentMap.fold
      (fun n v (yes, no) -> if f (n,v) then (add yes (n,v), no) else (yes, add no (n,v))) x (empty,empty)
   let content (x:t) = (
      List.fast_sort (fun (vn1, _) (vn2, _) -> compare vn1 vn2)
         (IdentMap.fold (fun n v acc -> (n,v)::acc) x [])
   )
   let to_string (pfx:string) (x:t) = (
      if x = empty then pfx^"empty\n"
      else (
         (* let nv2s (n,v) = pfx ^ "\t" ^ (Prevar.format n) ^ " = " ^ (value_to_string v) ^ "\n" in *)
         let nv2s (n,v) = pfx ^ "\t" ^ (n) ^ " = " ^ (value_to_string v) ^ "\n" in
         let str_l = List.map nv2s (content x) in
         String.concat "" str_l
      )
   )
   let to_short_string (x:t) = (
      let nv2s (n,v) = (n) ^ "=" ^ (value_to_string v) in
      let str_l = List.map nv2s (content x) in
      "{"^(String.concat ";" str_l)^"}"
   )
   let print (x:t) (oc:out_channel) = output_string oc (to_string "" x)
   let mapi = IdentMap.mapi
   let iter = IdentMap.iter
   let fold = IdentMap.fold
end


(* exported *)
let (num_is_int: num -> bool) =
  function I _ -> true | F _  -> false

(* exported *)
let (add_num : num -> num -> num) =
  fun n1 n2 ->
    match (n1, n2) with
	   | (I(i1), I(i2)) -> I(Num.add_num i1 i2)
      | (F(f1), F(f2)) -> F(f1 +. f2)
      | _ -> assert false

(* exported *)
let (diff_num : num -> num -> num) =
  fun n1 n2 ->
    match (n1, n2) with
	     (I(i1), I(i2)) -> I(Num.sub_num i1 i2)
      | (F(f1), F(f2)) -> F(f1 -. f2)
      | _ -> assert false

(* exported *)
let (mult_num : num -> num -> num) =
  fun n1 n2 ->
    match (n1, n2) with
	     (I(i1), I(i2)) -> I(Num.mult_num i1 i2)
      | (F(f1), F(f2)) -> F(f1 *. f2)
      | _ -> assert false

(* exported *)
let (quot_num : num -> num -> num) =
  fun n1 n2 ->
    match (n1, n2) with
	     (I(i1), I(i2)) -> I(Num.quo_num i1  i2)
      | (F(f1), F(f2)) -> F(f1 /. f2)
      | _ -> assert false

(* exported *)
let (div_num : num -> num -> num) = 
  fun n1 n2 ->
    match (n1, n2) with
	     (I(i1), I(i2)) -> I(Num.quo_num i1 i2)
      | (F(f1), F(f2)) -> F(f1 /. f2)
      | _ -> assert false

(* exported *)
let (modulo_num : num -> num -> num) =
  fun n1 n2 ->
    match (n1, n2) with
	     (I(i1), I(i2)) -> I(Num.mod_num i1 i2)
      | (F(f1), F(f2)) -> F(mod_float f1 f2)
      | _ -> assert false


let zero = Num.num_of_int 0

(* exported *)
let (num_eq_zero : num -> bool) =
  fun n ->
    match n with
	     I(i) -> i = zero
      | F(f) -> f = 0.

(* exported *)
let (num_sup_zero : num -> bool) =
  fun n ->
    match n with
	     I(i) -> Num.gt_num i zero
      | F(f) -> f > 0.

(* exported *)
let (num_supeq_zero : num -> bool) =
  fun n ->
    match n with
	     I(i) -> Num.ge_num i zero
      | F(f) -> f >= 0.

(* exported *)
let (neg : num -> num) =
  fun n ->
    match n with
	     I(i) -> I(Num.minus_num i)
      | F(f) -> F(-.f)

(* exported *)
let (eq_num: num -> num -> bool) =
  fun n1 n2 -> 
    match n1, n2 with
      | I n1, I n2 -> Num.eq_num n1 n2
      | I _ , F _  | F _, I _ -> false 
      | F f1, F f2 -> f1 = f2

