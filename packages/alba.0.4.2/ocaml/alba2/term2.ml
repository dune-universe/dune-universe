open Container
open Alba2_common




type fix_index = int
type decr_index = int
type oo_application = bool

type t =
  | Sort of Sorts.t
  | Variable of int
  | Application of t * t * Application_type.t
  | Lambda of abstraction
  | All of abstraction
  | Inspect of t * t * (t*t) array
  | Fix of fix_index * fixpoint
and typ = t
and abstraction =  string option * typ * t
and case = t * t
and fixpoint =
  (Feature_name.t option * typ * decr_index * t) array (* typ in outer context,
                                                          t in inner context *)


type fname = Feature_name.t option
type name_type = string option * typ
type fname_type = Feature_name.t option * typ
type gamma = fname_type array
type arguments = name_type array
type argument_list = name_type list


let proposition: t = Sort Sorts.Proposition
let any: t = Sort Sorts.Any
let box: t = Sort Sorts.Box


let product (a:t) (b:t): t option =
  match a, b with
  | Sort sa, Sort sb ->
     Some (Sort (Sorts.product sa sb))
  | _ ->
     None


let product1 (a:t) (b:t): t =
  match product a b with
  | None ->
     assert false (* illegal call *)
  | Some p ->
     p


let get_sort (a:t): Sorts.t option =
  match a with
  | Sort s ->
     Some s
  | _ ->
     None


let variable0: t = Variable 0
let variable1: t = Variable 1
let variable2: t = Variable 2
let variable3: t = Variable 3
let variable4: t = Variable 4
let variable5: t = Variable 5


let apply_any (f:t) (a:t): t =
  Application (f, a, Application_type.Any)

let apply_operator (op:t) (a:t): t =
  Application (op, a, Application_type.Operator)

let apply_target (f:t) (tgt:t): t =
  Application (f, tgt, Application_type.Target)

let apply1 (f:t) (a:t): t =
  Application (f, a, Application_type.First)

let apply2 (f:t) (a:t) (b:t): t =
  apply_any (apply1 f a) b

let apply3 (f:t) (a:t) (b:t) (c:t): t =
  apply_any (apply2 f a b) c

let apply4 (f:t) (a:t) (b:t) (c:t) (d:t): t =
  apply_any (apply3 f a b c) d

let binary (op:t) (a:t) (b:t): t =
  apply_any (apply_operator op a) b

let rec equal (a:t) (b:t): bool =
  match a,b with
  | Sort sa, Sort sb ->
     Sorts.equal sa sb
  | Variable i, Variable j when i = j ->
     true
  | Application (fa,a,_), Application (fb,b,_) ->
     equal fa fb && equal a b
  | Lambda (_,tp1,t1), Lambda (_,tp2,t2)
    | All (_,tp1,t1), All (_,tp2,t2) ->
     equal tp1 tp2 && equal t1 t2
  | Inspect(e1,r1,f1), Inspect(e2,r2,f2) ->
     let n = Array.length f1 in
     n = Array.length f2
     && equal e1 e2
     && equal r1 r2
     && assert false (* nyi *)
  | Fix(i1,f1), Fix(i2,f2) when i1 = i2 ->
     assert false (* nyi *)
  | _, _ ->
     false


let equal1 (a:t option) (b:t): bool =
  match a with
  | None ->
     false
  | Some a ->
     equal a b


let equal_arguments (a:arguments) (b:arguments): bool =
  let n = Array.length a in
  n = Array.length b
  && interval_for_all
       (fun i -> equal (snd a.(i)) (snd b.(i)))
       0 n


let fold_from (start:int) (f:'a->int->'a) (a:'a) (t:t): 'a =
  let rec fold (s:int) (a:'a) (t:t): 'a =
    match t with
    | Sort s -> a
    | Variable i when i < s ->
       a
    | Variable i ->
       assert (s <= i);
       f a (i-s)
    | Application (g,x,_) ->
       fold s (fold s a g) x
    | Lambda (_,tp,t) ->
       fold (s+1) (fold s a tp) t
    | All (_,tp,t) ->
       fold (s+1) (fold s a tp) t
    | Inspect (t,mp,arr) ->
       let fld = fold s in
       Array.fold_left
         (fun a (c,f) -> fld (fld a c) f)
         (fld (fld a t) mp)
         arr
    | Fix (idx,arr) ->
       Array.fold_left
         (fun a (_,tp,_,t) ->
           fold (s + Array.length arr) (fold s a tp) t)
         a
         arr
  in
  fold start a t


let fold (f:'a->int->'a) (a:'a) (t:t): 'a =
  fold_from 0 f a t


let has_variables (f:int->bool) (t:t): bool =
  fold
    (fun has v -> has || f v)
    false
    t


let map_from (start:int) (f:int->int) (t:t): t =
  let rec map s t =
    match t with
    | Sort s -> t
    | Variable i when i < s ->
       t
    | Variable i ->
       assert (s <= i);
       let idx = f (i-s) + s in
       assert (s <= idx); (* no capturing allowed *)
       Variable idx
    | Application (a,b,app) ->
       Application (map s a, map s b, app)
    | Lambda (nm,tp,t) ->
       Lambda (nm, map s tp, map (s+1) t)
    | All (nm,tp,t) ->
       All (nm, map s tp, map (s+1) t)
    | Inspect (t,mp,arr) ->
       Inspect (map s t,
                map s mp,
                Array.map
                  (fun (c,f) -> map s c, map s f)
                  arr)
    | Fix (idx,arr) ->
       let s1 = s + Array.length arr in
       Fix (idx,
            Array.map
              (fun (nm,tp,didx,t) ->
                nm, map s tp, didx, map s1 t)
              arr)
  in
  map start t

let map (f:int -> int) (t:t): t = map_from 0 f t

let shift_from (start:int) (n:int) (t:t): t =
  map_from start (fun i -> i + n) t

let shift (n:int) (t:t): t =
  shift_from 0 n t

let up_from (start:int) (n:int) (t:t): t =
  assert (0 <= n);
  shift_from start n t


let up (n:int) (t:t): t =
  assert (0 <= n);
  up_from 0 n t


let arrow (a:t) (b:t): t =
  All (None, a, up 1 b)






let rec split_application (f:t) (args:t list): t * t list =
  (* Analyze the term [f(args)] and split [f] as long as it is an
         application. Push all remaining arguments in the term [f] in front of
         the arguments [args].  *)
  match f with
  | Application (f,a,_) ->
     split_application f (a::args)
  | _ ->
     f, args

let application_type (i:int): Application_type.t =
  if i = 0 then
    Application_type.First
  else
    Application_type.Any

let apply_args (f:t) (args:t list): t =
  let t,_ =
    List.fold_left
      (fun (f,i) a ->
        Application (f, a, application_type i),
        i+1)
      (f,0) args
  in
  t

let rec apply_n_args (f:t) (n:int) (args: t list): t =
  if n = 0 then
    f
  else
    match args with
    | [] ->
       assert false (* Illegal call: Not enough arguments. *)
    | hd :: tl ->
       apply_n_args (apply1 f hd) (n-1) tl


let apply_arg_array (f:t) (args: t array): t =
  let nargs = Array.length args in
  let rec apply i t =
    if i = nargs then
      t
    else
      apply (i+1) (Application (t, args.(i), application_type i))
  in
  apply 0 f



let apply_standard (n:int) (start:int) (f:t): t =
  (* Compute the application

         f (Variable (start+n-1)) (Variable (start+n-2)) ... (Variable start)
   *)
  let res = ref f in
  for i = 0 to n - 1 do
    res := Application (!res,
                        Variable (start + n - 1 - i),
                        application_type i)
  done;
  !res



let rec split_lambda0
          (n:int) (a:t) (i:int) (args: argument_list)
        : t * argument_list =
  (* Analyze [(a:A,b:B, ...) := t], return (t, [...,b:B,a:A,args])

     [i]: number of arguments in [args]

     [n]: number of arguments to split from [t], if -1 then all possible
     splits.

   *)
  if i = n then
    a,args
  else
    match a with
    | Lambda(nme,tp,t) ->
       split_lambda0 n t (i+1) ((nme,tp) :: args)
    | _ ->
     a, args


let split_lambda_list (a:t): t * argument_list =
  split_lambda0 (-1) a 0 []


let split_lambda (a:t): arguments * t =
  (* Analyze [(a:A,b:B, ...) := e], return ([a:A,b:B,...], e) *)
  let e,args = split_lambda_list a in
  Array.of_list (List.rev args),
  e



let rec lambda (args:argument_list) (e:t): t =
  match args with
  | [] ->
     e
  | (nme,tp) :: args ->
     Lambda (nme, tp, lambda args e)



let push_lambda (args:arguments) (t:t): typ =
  let t = ref t
  and n = Array.length args in
  for i = 0 to Array.length args - 1 do
    let nme,tp = args.(n - 1 - i) in
    t := Lambda(nme,tp,!t)
  done;
  !t


let make_case (args:arguments) (co:t) (def:t): case =
  push_lambda args co, push_lambda args def


let case_definition ((co,def):case): t =
  def

let case_constructor ((co,def):case): t =
  co


let case_pair (c:case): t * t =
  c

let split_case ((co,def):case): argument_list * t * t =
  let co,args  = split_lambda_list co in
  let n = List.length args in
  let def,_ = split_lambda0 n def 0 [] in
  args, co, def





let rec split_product0
          (n:int) (a:typ) (i:int) (args: argument_list)
        : typ * argument_list =
  (* Analyze [all(a:A,b:B, ...) T], return (T, [...,b:B,a:A,args])

     [i]: number of arguments in [args]

     [n]: number of arguments to split from [t], if -1 then all possible
     splits.

   *)
  if i = n then
    a, args
  else
    match a with
    | All(nme,tp,t) ->
       split_product0 n t (i+1) ((nme,tp) :: args)
    | _ ->
       a, args



let split_product(a:typ): arguments * typ =
  (* Analyze [all(a:A,b:B, ...) T], return ([A,B,...], T) *)
  let tp, args = split_product0 (-1) a 0 [] in
  let n = List.length args in
  let arr = Array.make n (None,tp) in
  let rec mkarr i args =
    match args with
    | [] ->
       assert (i = 0)
    | a :: tl ->
       assert (i > 0);
       arr.(i-1) <- a;
       mkarr (i-1) tl
  in
  mkarr n args;
  arr, tp




let push_product (args:arguments) (tp:typ): typ =
  let tp = ref tp
  and n = Array.length args in
  for i = 0 to Array.length args - 1 do
    let nme,t = args.(n - 1 - i) in
    tp := All(nme,t,!tp)
  done;
  !tp




let substitute_vars (f:int->t) (t:t): t =
  (* Substitute the free variable [i] by the term [f i] in the term [t]. *)
  let rec subst bnd t =
    match t with
    | Sort _ -> t

    | Variable i when i < bnd ->
       t

    | Variable i ->
       f (i - bnd) |> up bnd

    | Application (a, b, oo) ->
       let sub = subst bnd in
       Application (sub a, sub b, oo)

    | Lambda (nm, tp, t0) ->
       Lambda (nm, subst bnd tp, subst (bnd+1) t0)

    | All (nm, tp, t0) ->
       All (nm, subst bnd tp, subst (bnd+1) t0)

    | Inspect (exp, map, cases) ->
       let sub = subst bnd in
       Inspect (sub exp,
                sub map,
                Array.map
                  (fun (c,f) -> sub c, sub f)
                  cases)

    | Fix (idx, arr) ->
       let sub = subst (bnd + Array.length arr) in
       Fix (idx,
            Array.map
              (fun (nm,tp,decr,t) -> nm, sub tp, decr, sub t)
              arr)
  in
  subst 0 t





let substitute_args (n:int) (f:int->t) (t:t): t =
  (* Substitute the first [n] variables by the terms returned by the function
     [f] in the term [t]. Shift all variables above [n] down by [n]. *)
  substitute_vars
    (fun i ->
      if i < n then
        f i
      else
        Variable (i - n)
    )
    t



let substitute (a:t) (b:t): t =
  (* Substitute the variable 0 in the term [a] by the term [b]. *)
  substitute_args 1 (fun _ -> b) a




let reduce_fixpoint (i:int) (fp:fixpoint): t =
  let n = Array.length fp in
  assert (i < n);
  let _,_,_,t = fp.(i) in
  substitute_args n (fun j -> Fix (n - j - 1, fp)) t



let beta_reduce (f:t) (args:t list): t * t list =
  (* Beta reduce the term [f(args)] where [f] is not an
     application. Analyze the function term [f] and extract lambda terms
     possible as many as there are arguments and do beta reduction. Return
     the beta reduced term and the remaining arguments. *)
  let rec beta f args args_rest =
    match f,args_rest with
    | Lambda (_,_,t0), a :: args_rest ->
       beta t0 (a :: args) args_rest
    | Application _, _ ->
       assert false (* [f] must not be an application. *)
    | _, _ ->
       let args = Array.of_list args in
       let nargs = Array.length args in
       (substitute_args
          nargs
          (fun i ->
            (* argument 0 is the last argument (list is reversed). Last
               argument (innermost) has to replace the variable 0. *)
            assert (i < nargs);
            args.(i)
          )
          f),
       args_rest
  in
  beta f [] args


let rec to_level (n:int) (t:t): t =
  (* Transform the variables of [t] which are De Bruijn indices into De Bruijn
     levels. The term [t] has [n] free variables. *)
  match t with
  | Sort _ ->
     t
  | Variable i ->
     Variable (n - i - 1)
  | Application (f, a, oo) ->
     Application (to_level n f, to_level n a, oo)
  | Lambda (nme, tp, t) ->
     Lambda (nme, to_level n tp, to_level (n+1) t)
  | All (nme, tp, t) ->
     All (nme, to_level n tp, to_level (n+1) t)
  | Inspect (e, r, cases) ->
     Inspect (to_level n e,
              to_level n r,
              Array.map
                (fun (c,f) ->
                  to_level n c,
                  to_level n f)
                cases)
  | Fix (i, fp) ->
     Fix (i,
          let len = Array.length fp in
          Array.map
            (fun (nme,tp,k,t) ->
              nme, to_level n tp, k, to_level (n+len) t)
          fp)


let to_index (n:int) (t:t): t =
  (* Transform the variables of [t] which are De Bruijn levels into De Bruijn
     indices. The term [t] has [n] free variables. *)
  let res = to_level n t in
  assert (to_level n res = t);
  res
