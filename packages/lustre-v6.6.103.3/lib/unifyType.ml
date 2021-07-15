(* Time-stamp: <modified the 29/08/2019 (at 16:48) by Erwan Jahier> *)

(*
12/07. Premier pas vers une méthode un peu plus standard :
   + renvoie un Lic.type_matches, i.e. une liste
     d'assoc. (TypeVar * type_)
   - Evidemment, comme on a en dur uniquement 2 TypeVar possible
     ça reste très limité ... 
*)

open Lic

let dbg = (Lv6Verbose.get_flag "typing")

(* exported *)
type t = 
  | Equal
  | Unif of Lic.type_
  | Ko of string (* a msg explaining why the unification failed *)

let teff2str = Lic.string_of_type
let string_of_t = function
   | Equal -> "Equal"
   | Unif e -> "Unif with any(num) = "^(teff2str e)
   | Ko _ -> "Ko" 


let (is_overloadable : Lic.type_ -> bool) = function
  | Int_type_eff -> true
  | Real_type_eff -> true
  | _ -> false
  

(* [contains t1 t2] is true iff t2 appears in t1 *)
let rec (contains : Lic.type_ -> Lic.type_ -> bool) =
  fun t1 t2 -> 
    if t1 = t2 then true else
    match t1 with
    | (TypeVar Any) 
    | TypeVar AnyNum
    | Bool_type_eff 
    | Int_type_eff 
    | Real_type_eff 
    | Enum_type_eff(_,_) 
    | External_type_eff _ -> false
    | Abstract_type_eff (_, _teff) -> false
    | Array_type_eff(teff,_i) -> contains teff t2
    | Struct_type_eff(_l, fl) -> 
   List.exists (fun (_,(teff,_)) -> contains teff t2) fl
    

(* exported
What for ?
Inutile d'essayer de ressembler à un vrai algo d'unification de type : 
- c'est PAS de l'unification, c'est du "matching" avec des jockers
- on a exactement 2 jockers
- c'est disymétrique : on a un expected avec des jockers
  et un given, et on doit arriver à s'en sortir ...
*)
let f (l1: Lic.type_ list) (l2: Lic.type_ list): t =
  let rec unify_type_eff (t1:Lic.type_) (t2: Lic.type_) : t =
    if t1 = t2 then Equal else  
      match (t1,t2) with
        | Array_type_eff(teff_ext1,i1), Array_type_eff(teff_ext2,i2) -> 
          if i1 <> i2 then Ko "\n***    incompatible array size"
          else unify_type_eff teff_ext1 teff_ext2
        | Struct_type_eff(l1, fl1), Struct_type_eff(l2, fl2) -> 
          if l1 <> l2 then Ko "\n***    incompatible structure" else
            (* USELESS ??? *)
            let fl1 = List.map (fun (_,(te,_)) -> te) fl1
            and fl2 = List.map (fun (_,(te,_)) -> te) fl2 in
            assert(List.length fl1 = List.length fl2);
            List.fold_left2 unify_do_acc Equal fl1 fl2
        | TypeVar AnyNum, TypeVar Any
        | TypeVar Any, TypeVar AnyNum -> Unif (TypeVar AnyNum)
        | t, TypeVar Any | (TypeVar Any), t -> 
          if contains t (TypeVar Any) || contains t (TypeVar AnyNum) then 
            Ko(("\n***    " ^ teff2str t1) ^ " and " ^ (teff2str t2) ^ 
                  " are not unifiable (there is a cycle)") 
          else Unif t
        | t, TypeVar AnyNum 
        | TypeVar AnyNum, t -> 
          if contains t (TypeVar Any) || contains t (TypeVar AnyNum) then 
            Ko("\n***    " ^ (teff2str t1) ^ " and " ^ (teff2str t2) ^ 
                  " are not unifiable (there is a cycle)") 
          else if is_overloadable t then Unif t 
          else 
            Ko("\n***    get '" ^ (teff2str t) ^ "' where 'int' or 'real' was expected")
        | _ -> 
          Ko("\n***    " ^ (teff2str t1) ^ " and " ^ (teff2str t2) ^
                " are not unifiable")

  and (unify_do_acc: t -> Lic.type_ -> Lic.type_ -> t) =
    fun acc te1 te2 -> 
      match acc, unify_type_eff te1 te2 with
        | Equal, Equal -> Equal
        | Ko msg, _ 
        | _, Ko msg -> Ko msg
        | Unif t, Equal
        | Equal, Unif t -> Unif t
        | Unif t1, Unif t2 -> if t1 = t2 then acc else 
            Ko("\n***    " ^ (teff2str t1) ^ " and " ^ (teff2str t2) ^
                  " are not unifiable")
  in
  let l1_str = Lic.string_of_type_list l1 in
  let l2_str = Lic.string_of_type_list l2 in
  
  Lv6Verbose.printf ~flag:dbg "#DBG: UnifyType.f (%s) with (%s) \n" l1_str l2_str;
  if (List.length l1 <> List.length l2) then
    Ko("\n** "^ l1_str ^ " and " ^ l2_str ^ " are not unifiable (bad arity)")
  else
  let res =
    assert (List.length l1 = List.length l2);
    List.fold_left2 unify_do_acc Equal l1 l2 in
  Lv6Verbose.printf ~flag:dbg
    "#DBG: UnifyType.f (%s) with (%s) gives %s\n"
    (Lic.string_of_type_list l1)
    (Lic.string_of_type_list l2)
    (string_of_t res);
  res

(****** MATCH ASSYMETRIQUE
On le fait avec un fold_left2
N.B. :
Lic.type_matches = (type_var * type_) list
(c'est un peu du luxe vu qu'on n'a que 2 jockers possibles) 
*)
exception Match_failed of string

(* UTILE : try_assoc curmatches tvar t
   - si existe (tvar, t') dans curmatches,
     * renvoie curmatches si t=t'
     * raise Match_failed sinon
   - sinon ajoute (tvar, t) à curmatches
*)
let try_assoc curmatches tvar t =
   try (
      let t' = List.assoc tvar curmatches in
      if (t = t') then curmatches
      else
         raise (Match_failed(
            Printf.sprintf "\n***    %s can't be matched both by %s and %s"
               (teff2str (TypeVar tvar)) (teff2str t) (teff2str t')
         ))
   ) with Not_found -> (tvar,t)::curmatches


let is_matched (expect_l: Lic.type_ list) (given_l: Lic.type_ list) : Lic.type_matches =
   (* Traite 1 type, accumule dans curmatches *)
  let rec do_type (curmatches:Lic.type_matches) (expect:Lic.type_) (given:Lic.type_) 
      : Lic.type_matches =
    if (given = expect) then curmatches else 
      match (expect, given) with
        | (TypeVar Any, t) -> try_assoc curmatches Any t
        | (TypeVar AnyNum, Int_type_eff) -> try_assoc curmatches AnyNum Int_type_eff
        | (TypeVar AnyNum, Real_type_eff) -> try_assoc curmatches AnyNum Real_type_eff
        | Array_type_eff(teff_ext1,i1), Array_type_eff(teff_ext2,i2) -> 
          if i1 <> i2 then raise (Match_failed("\n***    incompatible array sizes"))
          else do_type curmatches teff_ext1 teff_ext2
      (* Dans tous les autres cas échoue *)
        | _ -> raise(Match_failed(
          Printf.sprintf "\n***    %s can't be matched by %s"
            (teff2str expect) (teff2str given) 
        ))
  in
  assert(List.length expect_l = List.length given_l);
  List.fold_left2 do_type [] expect_l given_l


(************************************************************************************)
(* Some unit tests *)
let i = Int_type_eff
let r = Real_type_eff
let b = Bool_type_eff
let e = External_type_eff (Lv6Id.long_of_string "Toto::t")
let o = TypeVar AnyNum
let a = (TypeVar Any)
let array t c = Array_type_eff(t,c)
let struc t = Struct_type_eff ((Lv6Id.long_of_string "T::t"), 
               [(Lv6Id.of_string "x"),(t,None)])

let unify_failed = function Ko(_) -> true | _  -> false
let _to_string = function 
  | Ko(msg) -> msg
  | Equal -> "types are equals\n"
  | Unif t -> "types are unifiable via " ^ (teff2str t) ^ "\n"

  

let proposition1 t1 t2 =
  (* two lists of type are unifiable iff there exists a substitution
     that makes them equal. Hence, if [f] and [subst_type] are
     correct, the following function should always return true *)
  match f t1 t2 with
    | Unif t -> List.map (subst_type t) t1 = List.map (subst_type t) t2
    | _ -> true

(* Since i don't know how to prove proposition1, I generate some random tests *)
let rec gen_random_type_eff () = 
  match Random.int 8 with
    | 0 -> b
    | 1 -> i
    | 2 -> r
    | 3 -> e
    | 4 -> array (gen_random_type_eff ()) 42
    | 5 -> struc (gen_random_type_eff ())
    | 6 -> (TypeVar Any)
    | _ -> TypeVar AnyNum 

let gen_unifiable_typeff_of_size size =
  let rec aux tl1 tl2 =
    let ntl1 = (gen_random_type_eff ())::tl1 
    and ntl2 = (gen_random_type_eff ())::tl2 in
      if unify_failed (f ntl1 ntl2) then
   aux tl1 tl2
      else
   if List.length ntl1 = size then (ntl1,ntl2) else aux ntl1 ntl2
  in
    aux [] []


let (type_eff_list_to_string : Lic.type_ list -> string) =
  fun tel -> 
    let str_l = List.map teff2str tel in
      String.concat "*" str_l    


let unit_test () = 
  assert( f [i;r;b] [i;r;b] = Equal  );
  assert( f [i;r;b] [i;r;a] = Unif b );
  assert( f [i;r;a] [i;r;a] = Equal  );
  assert( f [i;a;b] [i;b;a] = Unif b );
  assert( f [a] [o]         = Unif o );
  assert( unify_failed(f [a] [ array o 3]) );
  assert( unify_failed(f [a] [ array a 3]) );
  assert( unify_failed(f [o] [ array o 3]) );
  assert( unify_failed(f [array i 4] [array o 3]));
  assert( unify_failed(f [a;a] [r;b]));

  Random.self_init ();
  for _i = 1 to 1000 do
    let (tl1, tl2) = gen_unifiable_typeff_of_size (1+ Random.int 10) in
      Lv6Verbose.print_string ~level:3 (
   " ==> try UnifyType.proposition1 with lists " ^ 
     (type_eff_list_to_string tl1) ^ " and " ^ 
     (type_eff_list_to_string tl2) ^ "\n");
      assert (proposition1 tl1 tl2)
  done

(*****************************************)
(* Check that the interface and the implementation types profile
   declarations are compatible.

   The idea is to consider abstract types as alpha types. The problem
   is that my unification algo works for one type variable only.
   However, it is a very simple [*] case of unification with several
   alpha var, that does not require the general unification
   algorithm. By considering variables one by one, I can use [UnifyType.f].

   [*] it is simpler than the general case because we can have alpha
   var in the provide part.

*)
let (profile_is_compatible: node_key -> Lxm.t -> Lic.type_ list * Lic.type_ list -> 
     Lic.type_ list * Lic.type_ list -> Lic.type_ option) =
  fun nk lxm (ins1, ins2) (outs1, outs2) -> 
    let to_any t = match t with (* consider abstract types as alpha types *)
      | Abstract_type_eff(_name, _) -> (TypeVar Any)
      | t -> t
    in
    let msg_prefix = ("provided node for " ^ (Lv6Id.string_of_long false (fst nk)) ^ 
                      " is not compatible with its implementation: ")
    in
    let apply_subst s t = try List.assoc t s with Not_found -> t in
    let unif_types (subst,topt) t_prov t_body = 
      if t_body = TypeVar Any || t_body = TypeVar AnyNum then
        (* Migth occur when a model is instanciated with a
           polymorphic operator, such as Lustre::eq. In such a case,
           we obtain a (TypeVar Any) or a TypeVar AnyNum from the
           implementation part ; the solution then is to replace that
           (TypeVar Any) type by the type of the provided part.  *)
        ( 
          assert (t_prov <> (TypeVar Any)); 
          assert (t_prov <> TypeVar AnyNum); 
          assert (topt = None || topt = Some t_prov); (* at most one type var per op *)
          (subst, Some t_prov) 
        )
      else
        let t_prov = apply_subst subst t_prov and t_body = apply_subst subst t_body in
        let nt_prov = to_any t_prov in
        match f [nt_prov] [t_body] with
        | Equal -> subst, topt
        | Unif t -> 
          (match t_prov with
           | Abstract_type_eff(_, tbody) -> 
             if tbody <> t && t_prov <> t_body then
               let msg = msg_prefix ^ "\n*** "^(teff2str tbody)^" <> "^(teff2str t) in
               raise(Lv6errors.Compile_error (lxm, msg))
           | _ -> ()
          );
          (t_prov,t)::subst, topt
        | Ko(msg) -> raise(Lv6errors.Compile_error (lxm, msg_prefix ^ msg))
    in

    let acc = [], None in
    let _ = 
      if List.length ins1 <> List.length ins2 then 
        raise(Lv6errors.Compile_error (lxm, msg_prefix ^ " wrong number of inputs"));
      if List.length outs1 <> List.length outs2 then 
        raise(Lv6errors.Compile_error (lxm, msg_prefix ^ " wrong number of outputs"))
    in
    let acc = List.fold_left2 unif_types acc ins1 ins2 in
    let acc = List.fold_left2 unif_types acc outs1 outs2 in
    snd acc
