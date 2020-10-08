(* Set/map of strings *)
module S = Set.Make (String)
module M = Map.Make (String)

(* Name generator for alpha-conversion *)
let generator s = 
  let s = Pervasives.ref s in
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
  object (this)
    val s = (M.empty : string M.t)
    method subst  x = try M.find x s with Not_found -> x
    method rename x = if S.mem x fvs
                      then let x' = gen x in x', {< s = M.add x x' s >}
                      else x, {< >}
  end

(* Type of lambda expression; enables ``show'' and ``foldl'' transformations *)
type lam =
| Var of GT.string
| App of lam * lam
| Lam of GT.string * lam [@@deriving gt ~show ~foldl]

(* Opening runtime module to avoid explicit qualification *)
open GT

(* ``show'' function *)
(* let show = transform(lam) (new @lam[show]) () *)

(* Transformation class to collect variables; reuses ``foldl'' *)
(* class var fself = object
 *   inherit [S.t] foldl_lam fself
 *   method c_Var s x = S.add x s
 * end
 *
 * let transform_lam = (let (module Op) = lam in Op.gcata)
 *
 * (\* Function to collect variable names *\)
 * let rec vars (l: lam) =
 *   let rec helper init l = gcata_lam (new var helper) init l in
 *   helper S.empty l
 *
 * (\* Context --- function to generate ``fresh'' names *\)
 * let context l = generator (vars l)
 *
 * (\* Transformation class to collect free variables; reuses ``var'' *\)
 * let fv =
 *   let rec helper init  = gcata_lam
 *     (object
 *       inherit var helper
 *       method c_Lam s x l = S.union s (S.remove x (helper S.empty l ))
 *     end)
 *     init
 *   in
 *   helper S.empty
 *
 * (\* Substitution function (generic as well) *\)
 * let subst g x m = gcata_lam
 *   object inherit [substitutor, lam] class_lam
 *     method c_Var s _ y =
 *       if y = x then m else Var (s#subst y)
 *     method c_Lam s z y l =
 *       if y = x then z.x
 *                else let y', s' = s#rename y in Lam (y', l.fx s')
 *     method c_App s _ l m = App (l.fx s, m.fx s)
 *   end (new substitutor g (fv m))
 *
 * (\* Module type to abstract base class for implementing reduction orders *\)
 * module type Reducer =
 *   sig
 *
 *     (\* Abstract type for inherited attribute *\)
 *     type context
 *     (\* Shortcut for augmented type of lambda-expression *\)
 *     type aug    = (context, lam, lam, < >) a
 *     (\* Shortcut for type of supplementary methods *\)
 *     type mtype  = context -> aug -> lam
 *
 *     (\* Abstract function to provide ``default'' inherited attribute *\)
 *     val default : lam -> context
 *
 *     (\* Template base class for reduction trnsformation *\)
 *     class virtual reducer :
 *       object inherit [context, lam] @lam
 *         method virtual arg       : mtype
 *         method virtual subst_arg : mtype
 *         method         head      : mtype
 *         method         c_Var     : context -> aug -> string -> lam
 *         method         c_App     : context -> aug -> aug -> aug -> lam
 *       end
 *
 *     (\* Template class for the only trait which is sensitive to
 *        the ``context'' type
 *     *\)
 *     class virtual reduce_under_abstractions :
 *       object inherit reducer
 *       method c_Lam : context -> aug -> string -> aug -> lam
 *     end
 *
 *   end
 *
 * (\* Functor to implement concrete reduction orders *\)
 * module Reductions (R : Reducer) =
 *   struct
 *     (\* Opening R to avoid qualifications *\)
 *     open R
 *
 *     (\* Top-level reduction function: applies reduction
 *        order ``r'' to lambda-term ``l''
 *     *\)
 *     let reduce r l = r (default l) l
 *
 *     (\* Basic reduction order traits *\)
 *     class virtual dont_reduce_under_abstractions =
 *       object inherit reducer
 *       method c_Lam _ s _ _ = s.x
 *     end
 *
 *     class virtual reduce_arguments =
 *       object inherit reducer
 *         method arg c x = x.fx c
 *       end
 *
 *     class virtual dont_reduce_arguments =
 *       object inherit reducer
 *         method arg _ x = x.x
 *       end
 *
 *     class virtual non_strict =
 *       object inherit reducer
 *         method subst_arg _ m = m.x
 *       end
 *
 *     class virtual strict =
 *       object inherit reducer
 *         method subst_arg c m = m.fx c
 *       end
 *
 *     (\* Reduction orders *\)
 *     class call_by_name = object
 *       inherit dont_reduce_under_abstractions
 *       inherit dont_reduce_arguments
 *       inherit non_strict
 *     end
 *
 *     let bn = transform(lam) (new call_by_name)
 *
 *     class normal = object
 *       inherit reduce_under_abstractions
 *       inherit reduce_arguments
 *       inherit non_strict
 *       method  head c x = bn c x.x
 *     end
 *     let nor = transform(lam) (new normal)
 *
 *     class call_by_value = object
 *       inherit dont_reduce_under_abstractions
 *       inherit reduce_arguments
 *       inherit strict
 *     end
 *     let bv = transform(lam) (new call_by_value)
 *
 *     class applicative = object
 *       inherit call_by_value
 *       inherit reduce_under_abstractions
 *     end
 *     let ao = transform(lam) (new applicative)
 *
 *     class hybrid_applicative = object
 *       inherit applicative
 *       method head c x = bv c x.x
 *     end
 *     let ha = transform(lam) (new hybrid_applicative)
 *
 *     class head_spine = object
 *       inherit call_by_name
 *       inherit reduce_under_abstractions
 *     end
 *
 *     let he = transform(lam) (new head_spine)
 *
 *     class hybrid_normal = object
 *       inherit normal
 *       method  head c x = he c x.x
 *     end
 *     let hn = transform(lam) (new hybrid_normal)
 *
 *     (\* Top-level definitions *\)
 *     let sample r l =
 *       Printf.printf "%s ----> %s\n" (show l) (show (r l))
 *
 *     let main () =
 *       let run n r =
 *         Printf.printf "\n========== %s ================\n\n" n;
 *         List.iter (sample r) [
 *           Lam ("x", App (Lam ("y", Var "y"), Var "z"));
 *           App (Lam ("x", App (Lam ("y", Var "y"), Var "z")), Var "y");
 *           App (Var "x", App (Lam ("x", Var "x"), Var "y"));
 *           App (Lam ("x", App (Var "x", Var "y")), App (Lam ("x", Var "x"), Var "y"));
 *           App (Lam ("x", App (Var "y", Var "x")), App (Lam ("x", Var "x"), Var "y"));
 *         ]
 *       in
 *       run "Call-by-name"        (reduce bn);
 *       run "Normal Order"        (reduce nor);
 *       run "Call-by-value"       (reduce bv);
 *       run "Applicative"         (reduce ao);
 *       run "Hybrid Applicative"  (reduce ha);
 *       run "Head Spine"          (reduce he);
 *       run "Hybrid Normal Order" (reduce hn)
 *
 *   end
 *
 * (\* Top-level definition *\)
 * let _ =
 *   (\* Simple case --- reduction with no context tracing *\)
 *   let module Simple = Reductions (
 *     struct
 *       (\* Inherited attribute: name-generation function *\)
 *       type context = string -> string
 *       type aug    = (context, lam, lam, < >) a
 *       type mtype  = context -> aug -> lam
 *
 *       let default = context
 *
 *       (\* Base reducer for the simple case *\)
 *       class virtual reducer =
 *         object(this) inherit [context, lam] @lam
 *           method virtual arg       : mtype
 *           method virtual subst_arg : mtype
 *           method         head      : mtype = fun c x -> x.fx c
 *           method c_Var _ x _   = x.x
 *           method c_App c s l m =
 *             match this#head c l with
 *             | Lam (x, l') -> s.f c (subst c x (this#subst_arg c m) l')
 *             | l'          -> let l'' = s.f c l' in
 *                              App (l'', this#arg c m)
 *          end
 *
 *       (\* Context-type-sensitive trait for the simple case *\)
 *       class virtual reduce_under_abstractions =
 *         object inherit reducer
 *           method c_Lam c _ x l = Lam (x, l.fx c)
 *         end
 *     end
 *   )
 *   in
 *   (\* Advanced case with context reconstruction; the definitions
 *      of all but one trait and all reduction orders completely reused
 *   *\)
 *   let module WithContext = Reductions (
 *     struct
 *       (\* Inherited attribute: name-generating function and term with a hole *\)
 *       type context = (string -> string) * (lam -> lam)
 *       type aug     = (context, lam, lam, < >) a
 *       type mtype   = context -> aug -> lam
 *
 *       (\* Combinators to manipulate terms with holes *\)
 *       let (@@) f g x = f (g x)
 *       let id   x     = x
 *       let abst x  e  = Lam (x, e)
 *       let appl e1 e2 = App (e1, e2)
 *       let appr e2 e1 = App (e1, e2)
 *
 *       let default l = (context l, id)
 *
 *       (\* Base reducer with context reconstruction *\)
 *       class virtual reducer =
 *         object(this) inherit [context, lam] @lam
 *           method virtual arg       : mtype
 *           method virtual subst_arg : mtype
 *           method         head      : mtype = fun c x -> x.fx c
 *           method c_Var _ x _ = x.x
 *           method c_App ((g, c) as i) s l m =
 *             match this#head (g, c @@ appl l.x) l with
 *             | Lam (x, l') -> s.f i (subst g x (this#subst_arg (g, c @@ appr l') m) l')
 *             | l'          -> let l'' = s.f (g, c @@ appl l.x) l' in
 *                              App (l'', this#arg (g, c @@ appr l'') m)
 *          end
 *
 *       (\* Context-type-sensitive trait with context reconstruction *\)
 *       class virtual reduce_under_abstractions =
 *         object inherit reducer
 *           method c_Lam (g, c) _ x l = Lam (x, l.fx (g, c @@ abst x))
 *         end
 *
 *     end
 *   )
 *   in
 *   (\* Running both cases *\)
 *   Simple.main ();
 *   WithContext.main () *)
