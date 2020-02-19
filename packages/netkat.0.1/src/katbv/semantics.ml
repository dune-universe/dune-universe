open Base
open Ast


module Env = struct
  module T = struct
    type t = Bitstring.t Hashtbl.M(String).t [@@deriving sexp]
    let compare t1 t2 =
      if Hashtbl.equal t1 t2 Bitstring.equal then 0
      else 
        let cmp = compare_int (Hashtbl.length t1) (Hashtbl.length t2) in
        if cmp > 0 then cmp else ~-1
  end
  include T
  include Comparator.Make(T)
end

type env_set = Set.M(Env).t


let of_str_lst lst =
  List.map lst ~f:(fun (a,b) -> (a,Bitstring.of_binary b))
  |> Hashtbl.of_alist_exn (module String)

let to_str_lst env_st = 
  Set.fold env_st ~init:[] ~f:(fun lst elm ->
    Hashtbl.to_alist elm
    |> List.map ~f:(fun (a,b) -> (a, Bitstring.to_string b))
    |> (fun h -> h::lst)
  )

let get_env env v =
  match Hashtbl.find env v with
  | Some v -> v
  | None -> Bitstring.zero

let eval_test (env:Env.t) = function
  | Interval (v, a, b) -> 
    let test_bound x1 x2 = 
      match Bitstring.(max (xor x1 x2)) with
      | None -> true
      | Some i -> Bitstring.nth x1 i
    in
    let env' = get_env env v in
    (test_bound env' a) && (test_bound b env')
  | Test (v, z, n) -> 
    let env' = get_env env v in
    Bitstring.(is_zero (env' ** z) && equal (env' ++ n) env')    
    
let rec eval_pred (env:Env.t) (b:bexp) : bool =
  match b with
  | True -> true
  | False -> false
  | Test tst -> eval_test env tst
  | Disj (b1, b2) -> (eval_pred env b1) || (eval_pred env b2)
  | Conj (b1, b2) -> (eval_pred env b1) && (eval_pred env b2)
  | Neg b' -> not (eval_pred env b')

let eval_act (env:Env.t) ((v, z, n):act) = 
  let v' = get_env env v in
  let updated_v' = Bitstring.((v' -- z) ++ n) in
  let modified_env = Hashtbl.copy env in
  Hashtbl.update modified_env v ~f:(fun _ -> updated_v');
  Set.singleton (module Env) modified_env

let rec eval ~env (exp:exp) =
  match exp with
  | Assert b -> 
    if eval_pred env b then 
      Set.singleton (module Env) env
    else 
      Set.empty (module Env)
  | Action act -> eval_act env act
  | Union (e1, e2) -> Set.union (eval ~env e1) (eval ~env e2)
  | Seq (e1, e2) -> Set.fold (eval ~env e1) ~init:(Set.empty (module Env))
    ~f:(fun set env' -> Set.union set (eval env' e2))
  | Star e -> 
    let rec loop acc =
      let acc' = Set.fold acc ~init:acc ~f:(fun set env' ->
        Set.union set (eval env' e)
      ) in
      if Set.equal acc acc' then acc else loop acc'
    in
    loop (Set.singleton (module Env) env)