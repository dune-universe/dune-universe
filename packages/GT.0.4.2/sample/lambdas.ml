(* Set/map of strings *)
module S = Set.Make (String)
module M = Map.Make (String)

(* Name generator for alpha-conversion *)
let generator s =
  let s = Stdlib.ref s in
  let rec gen x =
    let n = x ^ "'" in
    if S.mem n !s
    then gen n
    else (s := S.add n !s; n)
  in gen

(* Helper class for alpha-conversion; parameterized
   by name-generating function ``g'' and a set of ``prohibited''
   free variables ``fvs''
*)
class substitutor gen fvs =
  object
    val s = (M.empty : string M.t)
    method subst  x = try M.find x s with Not_found -> x
    method rename x = if S.mem x fvs
                      then let x' = gen x in x', {< s = M.add x x' s >}
                      else x, {< >}
  end

(* Type of lambda expression; enables ``show'' and ``foldl'' transformations *)
@type lam =
| Var of GT.string
| App of lam * lam
| Lam of GT.string * lam with show,foldl

(* Opening runtime module to avoid explicit qualification *)
(* open GT *)

(* ``show'' function *)
let show = GT.transform(lam) (new @lam[show]) ()

(* Transformation class to collect variables; reuses ``foldl'' *)
  class var fself = object
    inherit [S.t, _] @lam[foldl] fself
    method! c_Var s _ x = S.add x s
  end

  (* let transform_lam = (let (module Op) = lam in Op.gcata) *)

  (* Function to collect variable names *)
  let vars (l: lam) =
    GT.transform lam (new var) S.empty l

  (* Context --- function to generate ``fresh'' names *)
  let context l = generator (vars l)

  (* Transformation class to collect free variables; reuses ``var'' *)
  let fv =
    GT.transform(lam)
      (fun fself -> object
        inherit var fself
        method! c_Lam s _ x l = S.union s (S.remove x (fself S.empty l))
      end)
      S.empty

  (* Substitution function (generic as well) *)
  let subst g x m =
    GT.transform(lam)
    (fun fself -> object
      inherit [substitutor, _, lam] lam_t
      method c_Var s _ y =
        if y = x then m else Var (s#subst y)
      method c_Lam s z y body =
        if y = x
        then z
        else let y', s' = s#rename y in Lam (y', fself s' body)
      method c_App s _ l r = App (fself s l, fself s r)
    end)
    (new substitutor g (fv m))

  (* Module type to abstract base class for implementing reduction orders *)
module type Reducer =
   sig

     (* Abstract type for inherited attribute *)
     type context
     (* Shortcut for augmented type of lambda-expression *)
     (* type aug    = (context, lam, lam, < >) a *)
     (* Shortcut for type of supplementary methods *)
     type mtype  = context -> lam -> lam

     (* Abstract function to provide ``default'' inherited attribute *)
     val default : lam -> context

     (* Template base class for reduction trnsformation *)
     class virtual reducer : (context -> lam -> lam) ->
       object inherit [context, _, lam] lam_t
         method virtual arg       : mtype
         method virtual subst_arg : mtype
         method         head      : mtype
         method         c_Var     : context -> lam -> string -> lam
         method         c_App     : context -> lam -> lam -> lam -> lam
       end

     (* Template class for the only trait which is sensitive to
        the ``context'' type
      *)
     class virtual reduce_under_abstractions : (context -> lam -> lam) ->
       object inherit reducer
       method c_Lam : context -> lam -> string -> lam -> lam
     end

    end

(* Functor to implement concrete reduction orders *)
module Reductions (R : Reducer) =
  struct
    (* Opening R to avoid qualifications *)
    open R

    (* Top-level reduction function: applies reduction
       order ``r'' to lambda-term ``l''
    *)
    let reduce r l = r (default l) l

    (* Basic reduction order traits *)
    class virtual dont_reduce_under_abstractions fself =
      object inherit reducer fself
      method c_Lam _ s _ _ = s
    end

    class virtual reduce_arguments fself =
      object inherit reducer fself
        method arg = fself
      end

    class virtual dont_reduce_arguments fself =
      object inherit reducer fself
        method arg _ x = x
      end

    class virtual non_strict fself =
      object inherit reducer fself
        method subst_arg _ m = m
      end

    class virtual strict fself =
      object inherit reducer fself
        method subst_arg c m = fself c m
      end

    (* Reduction orders *)
    class call_by_name fself = object
      inherit dont_reduce_under_abstractions fself
      inherit dont_reduce_arguments fself
      inherit non_strict fself
    end

    let bn = GT.transform(lam) (new call_by_name)

    class normal fself = object
      inherit reduce_under_abstractions fself
      inherit reduce_arguments fself
      inherit non_strict fself
      method  head c x = bn c x
    end
    let nor = GT.transform(lam) (new normal)

    class call_by_value fself = object
      inherit dont_reduce_under_abstractions fself
      inherit reduce_arguments fself
      inherit strict fself
    end
    let bv = GT.transform(lam) (new call_by_value)

    class applicative fself = object
      inherit call_by_value fself
      inherit reduce_under_abstractions fself
    end
    let ao = GT.transform(lam) (new applicative)

    class hybrid_applicative fself = object
      inherit applicative fself
      method head c x = bv c x
    end
    let ha = GT.transform(lam) (new hybrid_applicative)

    class head_spine fself = object
      inherit call_by_name fself
      inherit reduce_under_abstractions fself
    end

    let he = GT.transform(lam) (new head_spine)

    class hybrid_normal fself = object
      inherit normal fself
      method  head c x = he c x
    end
    let hn = GT.transform(lam) (new hybrid_normal)

    (* Top-level definitions *)
    let sample r l =
      Printf.printf "%s ----> %s\n" (show l) (show (r l))

    let main () =
      let run n r =
        Printf.printf "\n========== %s ================\n\n" n;
        List.iter (sample r) [
          Lam ("x", App (Lam ("y", Var "y"), Var "z"));
          App (Lam ("x", App (Lam ("y", Var "y"), Var "z")), Var "y");
          App (Var "x", App (Lam ("x", Var "x"), Var "y"));
          App (Lam ("x", App (Var "x", Var "y")), App (Lam ("x", Var "x"), Var "y"));
          App (Lam ("x", App (Var "y", Var "x")), App (Lam ("x", Var "x"), Var "y"));
        ]
      in
      run "Call-by-name"        (reduce bn);
      run "Normal Order"        (reduce nor);
      run "Call-by-value"       (reduce bv);
      run "Applicative"         (reduce ao);
      run "Hybrid Applicative"  (reduce ha);
      run "Head Spine"          (reduce he);
      run "Hybrid Normal Order" (reduce hn)

  end

(* Top-level definition *)
let _ =
  (* Simple case --- reduction with no context tracing *)
  let module Simple = Reductions (
    struct
      (* Inherited attribute: name-generation function *)
      type context = string -> string
      type aug    = lam (* (context, lam, lam, < >) a *)
      type mtype  = context -> aug -> lam

      let default = context

      (* Base reducer for the simple case *)
      class virtual reducer fself =
        object(this) inherit [context, lam, lam] @lam
          method  virtual arg       : mtype
          method  virtual subst_arg : mtype
          method          head      : mtype = fun c x -> fself c x
          method c_Var _ x _   = x
          method c_App c _ l m =
            match this#head c l with
            | Lam (x, l') -> fself c (subst c x (this#subst_arg c m) l')
            | l'          -> let l'' = fself c l' in
                             App (l'', this#arg c m)
         end

      (* Context-type-sensitive trait for the simple case *)
      class virtual reduce_under_abstractions fself = object
          inherit reducer fself
          method c_Lam c _ x l = Lam (x, fself c l)
        end
    end
  )
  in
  (* Advanced case with context reconstruction; the definitions
     of all but one trait and all reduction orders completely reused
  *)
  let module WithContext = Reductions (
    struct
      (* Inherited attribute: name-generating function and term with a hole *)
      type context = (string -> string) * (lam -> lam)
      type aug     = lam
      type mtype   = context -> aug -> lam

      (* Combinators to manipulate terms with holes *)
      let (@@) f g x = f (g x)
      let id   x     = x
      let abst x  e  = Lam (x, e)
      let appl e1 e2 = App (e1, e2)
      let appr e2 e1 = App (e1, e2)

      let default l = (context l, id)

      (* Base reducer with context reconstruction *)
      class virtual reducer fself =
        object(this) inherit [context, lam, lam] @lam
          method virtual arg       : mtype
          method virtual subst_arg : mtype
          method         head      : mtype = fself
          method c_Var _ x _ = x
          method c_App ((g, c) as i) _ l m =
            match this#head (g, c @@ appl l) l with
            | Lam (x, l') -> fself i (subst g x (this#subst_arg (g, c @@ appr l') m) l')
            | l'          -> let l'' = fself (g, c @@ appl l) l' in
                             App (l'', this#arg (g, c @@ appr l'') m)
         end

      (* Context-type-sensitive trait with context reconstruction *)
      class virtual reduce_under_abstractions fself = object
          inherit reducer fself
          method c_Lam (g, c) _ x l = Lam (x, fself (g, c @@ abst x) l)
        end
    end
  )
  in
  (* Running both cases *)
  Simple.main ();
  WithContext.main ()
