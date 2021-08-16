module M = Map.Make (String)

class enumerator =
  object
    val i = 0
    val m = M.empty

    method add    name = {< i = i + 1; m = M.add name i m >}, i
    method lookup name = M.find name m
  end
let lookup env name = (env, env#lookup name)
let unused _ _ = failwith "*** Using the unused ***"

let rec fix f x = f (fix f) x
let ith m n =
  fix (fun me i -> function
      | []                 -> raise Not_found
      | x :: _  when x = n -> i
      | _ :: tl            -> me (i+1) tl
    ) 0 m

let index = ith
module Lam = struct
  @type ('name, 'lam) t = [`App of 'lam * 'lam | `Var of 'name] with show

  class [ 'lam , 'nameless] lam_to_nameless
      (flam : string list -> 'lam -> 'nameless) =
    object
      inherit [ string list, string, int
              , string list , 'lam , 'nameless
              , string list , 'lam , 'nameless
              ] t_t
      method c_App env _ l r = `App (flam env l , flam env r)
      method c_Var env _ x   = `Var (index env x)
  end

end

module Abs = struct
  @type ('name, 'term) t = [`Abs of 'name * 'term ] with show

  class [ 'lam , 'nameless] abs_to_nameless
      (flam : string list -> 'lam -> 'nameless) =
    object
      inherit [string list , string , int
              , string list , 'lam , 'nameless
              , string list , 'lam , 'nameless
              ] t_t
      method c_Abs env _ name term = `Abs (flam (name :: env) term)
    end
end

@type ('name , 'lam) term = [ ( 'name , 'lam) Lam.t | ( 'name , 'lam) Abs.t ] with show

@type named = (GT.string, named) term with show
@type nameless = [ (GT.int , nameless) Lam.t | `Abs of nameless ] with show

class to_nameless (fself : string list -> named ->nameless) =
  object
    inherit [ string list, named, nameless ] named_t
    inherit [named , nameless] Lam.lam_to_nameless fself
    inherit [named , nameless] Abs.abs_to_nameless fself
  end

let to_nameless lam =
  GT.transform(named) (fun fself -> new to_nameless fself) [ ] lam

let _ =
  let l = `App (`Abs ("x", `Var "x"), `Abs ("y", `Var "y")) in
  Printf.printf "Original: %s\n" (GT.show(named) l);
  Printf.printf "Converted: %s\n" (GT.show(nameless) @@ to_nameless l)

(*
module Let = struct
  @type ('name, 'term) t = [`Let of 'name * 'term * 'term] with show,eval,stateful
  class ['me, 'term2, 'extra, 'syn] de_bruijn ft = object
    inherit [string, unit, 'me, 'term2, 'extra, 'syn, 'env] @t[eval]
        (fun _ _ -> ())
        ft
        unused
    constraint 'env = string list
    constraint 'syn = [> (unit, 'term2) t ]
    constraint 'extra = [> (string, 'term) t ]
    method c_Let env _ name bnd term = `Let ((), ft env bnd,  ft (name :: env) term)
  end

  class ['me, 'me', 'extra, 'syn] import ft =
    object
      inherit [string, int, 'me, 'me', 'extra, 'syn, enumerator] @t[stateful]
                lookup ft unused
      constraint 'extra = [> (string, 'me) t ]
      constraint 'syn = [> (int, 'me2) t ]
      method c_Let env0 _ name bnd term =
        let env1, i = env0#add name in
        let env2, l = ft env1 bnd in
        let env3, r = ft env2 term in
        env3, `Let (i, l, r)
    end
end

module LetRec = struct
  @type ('name, 'term) t = [`LetRec of 'name * 'term * 'term] with show,eval,stateful

  class ['me, 'me2, 'extra, 'syn] de_bruijn ft = object
    inherit [string, unit, 'me, 'me2, 'extra, 'syn, 'env] @t[eval]
        (fun _ _ -> ())
        ft
        unused
    constraint 'env = string list
    constraint 'extra = [> (string, 'me) t]
    constraint 'syn = [> (unit, 'me2) t]
    method c_LetRec env _ name bnd term =
      let env' = name :: env in
      `LetRec ((), ft env' bnd,  ft env' term)
  end

  class ['me, 'me', 'extra, 'syn] import ft =
    object
      inherit [string, int, 'me, 'me', 'extra, 'syn, enumerator] @t[stateful]
                lookup ft unused
      method c_LetRec env _ name bnd term =
        let (env0, i) = env#add name in
        let (env1, l) = ft env0 bnd in
        let (env2, r) = ft env1 term in
        env2, `LetRec (i, l, r)
    end
end
;;

@type ('n, 'b) t =
   [ ('n, ('n, 'b) t) Lam.t
   | ('b, ('n, 'b) t) Abs.t
   | ('b, ('n, 'b) t) Let.t
   | ('b, ('n, 'b) t) LetRec.t
   ] with show,eval,stateful
;;
@type named    = (GT.string, GT.string) t with show;;
@type nameless = (GT.int, GT.unit) t with show;;
@type nominal  = (GT.int, GT.int ) t with show;;

class ['extra, 'syn] de_bruijn fself = object
  inherit [string, int, string, unit, 'extra, 'syn, _] eval_t_t
      ith (fun _ _ -> ()) fself

  inherit [named, nameless, 'extra, 'syn ]    Abs.de_bruijn fself
  inherit [named, nameless, 'extra, 'syn ]    Let.de_bruijn fself
  inherit [named, nameless, 'extra, 'syn ] LetRec.de_bruijn fself
end

let convert term = GT.transform(t) (new de_bruijn) [] term


class ['extra, 'syn] import fself =
object
  inherit [string, int, string, int, 'extra, 'syn, _] @t[stateful]
            lookup lookup fself

  inherit [named, nominal, 'extra, 'syn]    Abs.import fself
  inherit [named, nominal, 'extra, 'syn]    Let.import fself
  inherit [named, nominal, 'extra, 'syn] LetRec.import fself
end

let import term =
  snd @@ GT.transform(t) (new import) (new enumerator) term

let _ =
  let l = `App (`Abs ("x", `Var "x"), `Abs ("y", `Var "y")) in
  Printf.printf "Original: %s\n" (GT.show(named) l);
  let m = `App (`Abs ((), `Var 1), `Abs ((), `Var 2)) in
  Printf.printf "Nameless : %s\n" (GT.show(nameless) m);
  Printf.printf "Converted: %s\n" (GT.show(nameless) @@ convert l);
  Printf.printf "Converted: %s\n" @@ GT.show(nameless) @@ convert @@
  `Let ("z", `Abs ("x", `Var "x"),
        `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z"))));
  Printf.printf "Converted: %s\n" @@ GT.show(nameless) @@ convert @@
  `LetRec ("z", `App (`Abs ("x", `Var "x"), `Var "z"),
           `Abs ("x", `Abs ("y", `App (`Var "x", `Var "z"))));
  Printf.printf "Imported: %s\n" (GT.show(nominal) @@ import l)
*)
