(* Name binding and transformation workout: lambda calculus + let + let rec, 
   named, nameless and nominal representations
*)

(* Opening the library *)
open GT

(* Some supplementary definitions *)       
module M = Map.Make (String)
                    
class enumerator =
  object
    val i = 0
    val m = M.empty
              
    method add    name = {< i = i + 1; m = M.add name i m >}, i
    method lookup name = M.find name m                                
  end
                            
let rec fix f x  = f (fix f) x

(* Three shortcuts for type parameter-transforming functions *)                     
let unused _ _ = failwith "*** Using the unused ***"
let ith m n = fix (fun me i -> function
                   | []                 -> raise Not_found
                   | x :: tl when x = n -> i
                   | _ :: tl            -> me (i+1) tl 
                  ) 0 m
let lookup env name = env#lookup name


(* Basic type for lambda terms: application + bound variable *)
module Lam =
  struct
        
    @type ('name, 'lam) t = [`App of 'lam * 'lam | `Var of 'name] with show, eval

  end

(* Abstraction *)    
module Abs =
  struct
        
    @type ('name, 'term) t = [`Abs of 'name * 'term] with show, eval

    (* Converting to a nominal representation *)
    class ['me, 'me'] import ft =
    object
      inherit [string, int, 'me, 'me', enumerator, 'me'] @t[eval] unused lookup ft
      method c_Abs env name term =
        let env', i = env#add name in `Abs (i, ft env' term)
    end

    (* Converting to a nameless representation *)
    class ['me, 'me'] de_bruijn ft =
    object
      inherit [string, unit, 'me, 'me', string list, 'me'] @t[eval] unused (fun _ _ -> ()) ft
      method c_Abs env name term = `Abs ((), ft (name :: env) term)
    end

    (* Converting to a nameless representation, throwing out the placeholder for 
       unneeded name occurrence
    *)
    class ['me, 'me'] de_bruijn' ft =
    object
      inherit [string, string list, unit, 'me, string list, 'me', string list, 'me', 'me] @t
      method c_Abs env name term = `Abs (ft (name :: env) term) (* note: different type *)
    end
      
  end

(* Let; the components have the same meanings *)    
module Let =
  struct
    
    @type ('name, 'term) t = [`Let of 'name * 'term * 'term] with show, eval
                                                                          
    class ['me, 'me'] de_bruijn ft =
    object 
      inherit [string, unit, 'me, 'me', string list, 'me'] @t[eval] unused (fun _ _ -> ()) ft
      method c_Let env name bnd term = `Let ((), ft env bnd,  ft (name :: env) term) 
    end
      
    class ['me, 'me'] import ft =
    object
      inherit [string, int, 'me, 'me', enumerator, 'me'] @t[eval] unused lookup ft
      method c_Let env name bnd term =
        let env', i = env#add name in `Let (i, ft env bnd, ft env' term)
    end
      
    class ['me, 'me'] de_bruijn' ft =
    object 
      inherit [string, string list, unit, 'me, string list, 'me', string list, 'me', 'me] @t
      method c_Let env name bnd term = `Let (ft env bnd,  ft (name :: env) term) 
    end
      
  end

(* Let rec *)    
module LetRec =
  struct
    
    @type ('name, 'term) t = [`LetRec of 'name * 'term * 'term] with show, eval
                                                                             
    class ['me, 'me'] de_bruijn ft =
    object 
      inherit [string, unit, 'me, 'me', string list, 'me'] @t[eval] unused (fun _ _ -> ()) ft
      method c_LetRec env name bnd term =
        let env' = name :: env in
        `LetRec ((), ft env' bnd,  ft env' term) 
    end
      
    class ['me, 'me'] import ft =
    object
      inherit [string, int, 'me, 'me', enumerator, 'me'] @t[eval] unused lookup ft
      method c_LetRec env name bnd term =
        let env', i = env#add name in `LetRec (i, ft env' bnd, ft env' term)
    end
      
    class ['me, 'me'] de_bruijn' ft =
    object 
      inherit [string, string list, unit, 'me, string list, 'me', string list, 'me', 'me] @t
      method c_LetRec env name bnd term =
        let env' = name :: env in
        `LetRec (ft env' bnd,  ft env' term) 
    end
      
  end

(* Combining the things together *)

(* Basic very general type; 'n --- a representation of names in non-binder occurences, 
   'b --- in binders 
*)
@type ('n, 'b) t = [
| ('n, ('n, 'b) t) Lam.t
| ('b, ('n, 'b) t) Abs.t
| ('b, ('n, 'b) t) Let.t
| ('b, ('n, 'b) t) LetRec.t
] with show, eval

(* Various concrete representations *)               
@type named    = (string, string) t with show
@type nameless = (int   , unit  ) t with show
@type nominal  = (int   , int   ) t with show

(* Combining the convertor to a nameless representation *)
class de_bruijn fself =
object
  inherit [string, int, string, unit, string list, nameless] @t[eval] fself ith unused
  inherit [named, nameless] Abs   .de_bruijn fself
  inherit [named, nameless] Let   .de_bruijn fself
  inherit [named, nameless] LetRec.de_bruijn fself
end

(* Combining the convertor to a nominal representation *)  
class import fself =
object
  inherit [string, int, string, int, enumerator, nominal] @t[eval] fself lookup lookup
  inherit [named, nominal] Abs   .import fself
  inherit [named, nominal] Let   .import fself
  inherit [named, nominal] LetRec.import fself
end

(* Conversion functions *)  
let convert term =
  fix0 (fun f -> transform(t) (new de_bruijn f)) [] term 

(*       
let import term =
  let rec import' =
    let mem = lazy (transform(t) (Printf.printf "New...\n"; new import import')) in
    fun enum term -> (Lazy.force mem) enum term
  in
  import' (new enumerator) term
 *)     


let rec fix1 f x y =
  let f' = f (fix1 f) in
  f' x y

(*let import term =
  fix1 (fun f -> (*lazy*) (transform(t) (new import f) (new enumerator))) term
 *)     


let import term =
  fix1 (fun f -> transform(t) (Printf.printf "New...\n"; new import f)) (new enumerator) term 
 
          
(* Testing *)
let _ =
  let l = `App (`Abs ("x", `Var "x"), `Abs ("y", `Var "y")) in
  Printf.printf "Original: %s\n"  (show(named) l);
  Printf.printf "Converted: %s\n" (show(nameless) @@ convert l);  
  Printf.printf "Converted: %s\n" (show(nameless) @@ convert (`Let    ("z", `Abs ("x", `Var "x"), `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z")))))); 
  Printf.printf "Converted: %s\n" (show(nameless) @@ convert (`LetRec ("z", `App (`Abs ("x", `Var "x"), `Var "z"), `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z"))))));
  Printf.printf "Imported: %s\n" (show(nominal) @@ import l) 
                
module BetterNameless =
  struct

    @type named = [
    | (string, named) Lam.t
    | (string, named) Abs.t
    | (string, named) Let.t
    | (string, named) LetRec.t
    ] with show, eval
                     
    @type nameless = [
    | (int, nameless) Lam.t
    | `Abs of nameless
    | `Let of nameless * nameless
    | `LetRec of nameless * nameless
    ] with show, eval

    class de_bruijn (fself : string list -> named -> nameless) =
    object
      inherit [string, int, named, nameless, string list, nameless] @Lam.t[eval] fself ith fself
      inherit [named, nameless] Abs   .de_bruijn' fself 
      inherit [named, nameless] Let   .de_bruijn' fself
      inherit [named, nameless] LetRec.de_bruijn' fself 
    end
      
    let convert term =
      fix0 (fun f -> transform(named) (new de_bruijn f)) [] term 
           
    let _ =
      let l = `App (`Abs ("x", `Var "x"), `Abs ("y", `Var "y")) in
      Printf.printf "Original: %s\n" (show(named) l);
      Printf.printf "Converted: %s\n" (show(nameless) @@ convert l);
      Printf.printf "Converted: %s\n" (show(nameless) @@ convert (`Let    ("z", `Abs ("x", `Var "x"), `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z")))))); 
      Printf.printf "Converted: %s\n" (show(nameless) @@ convert (`LetRec ("z", `App (`Abs ("x", `Var "x"), `Var "z"), `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z"))))))
                    
  end
     
