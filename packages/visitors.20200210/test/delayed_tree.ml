(* To play with this code in an OCaml toplevel, launch [ocaml] and type this:
   #use "topfind";;
   #require "visitors.ppx";;
   #require "visitors.runtime";;
 *)

(* -------------------------------------------------------------------------- *)

(* Suppose we have an arbitrary data structure that contains elements
   of type ['a]. Here, it is a binary tree, but it could be anything: *)

type 'a sometree =
  | Leaf
  | Node of 'a sometree * 'a * 'a sometree

(* This annotation is used only at the very end and can be ignored upon
   first reading: *)

[@@deriving visitors { variety = "reduce"; polymorphic = true;
                       name = "sometree_reduce" }]

(* We would like to enumerate the elements of this data structure.
   More precisely, we would like to construct an iterator, that is,
   an on-demand producer of elements. Here is a simple definition
   of a stateful iterator: *)

type 'a iterator =
  unit -> 'a option

(* The question is, can we construct an iterator for the type ['a sometree],
   based on an automatically-generated visitor, so that the construction is
   entirely independent of the type ['a sometree]? *)

(* -------------------------------------------------------------------------- *)

(* For starters, let us define cascades, which are a more pleasant kind of
   iterators. A cascade is a persistent (stateless) iterator. It can be
   thought of as a delayed list, that is, a list whose elements are computed
   only on demand. *)

(* Cascades could (should) be part of a separate library. There is in fact a
   proposal to add them to OCaml's standard library: see the discussion at
   https://github.com/ocaml/ocaml/pull/1002 *)

type 'a cascade =
  unit -> 'a head

and 'a head =
  | Nil
  | Cons of 'a * 'a cascade

(* A delayed computation is represented as a function of type [unit -> _].
   Thus, no memoization takes place. It is easy to implement a function
   [memo: 'a cascade -> 'a cascade] that turns a nonmemoizing cascade into
   a memoizing one, so memoization can be requested a posteriori, if
   desired. *)

(* The empty cascade. *)

let nil : 'a cascade =
  fun () -> Nil

let cons (x : 'a) (xs : 'a cascade) : 'a cascade =
  fun () -> Cons (x, xs)

(* Forcing a cascade reveals its head. *)

let force xs =
  xs()

(* A cascade can be easily converted to a stateful iterator. *)

let cascade_to_iterator (xs : 'a cascade) : 'a iterator =
  let s = ref xs in
  fun () ->
    match force !s with
    | Nil ->
        (* Writing [nil] into [s] may seem superfluous, but is in fact
           necessary to guarantee that the computation that just led to
           a [Nil] outcome is not repeated in the future. *)
        s := nil;
        None
    | Cons (x, xs) ->
        s := xs;
        Some x

(* Because cascades are close cousins of lists, they are easy to work with.
   Constructing a cascade for a tree-like data structure is straightforward,
   whereas directly constructing a stateful iterator would be more involved. *)

(* -------------------------------------------------------------------------- *)

(* Now, can we use some kind of visitor to turn a tree of type ['a sometree]
   into a cascade of type ['a cascade]? *)

(* At first sight, this does not seem very easy, for two reasons: 1- a visitor
   usually traverses a tree in an eager manner, whereas we need the traversal
   to make progress only as cascade elements are demanded; and 2- a visitor
   performs a bottom-up computation, without a left-to-right bias (assuming
   mutable state is not used), whereas a cascade enumerates elements in a
   left-to-right manner. (Or in a right-to-left manner. As will be apparent
   below, both directions are possible.) *)

(* The trick is to use another intermediate step. Instead of turning a tree
   directly into a cascade, we first transform it into a generic tree-like
   structure: a *delayed tree*. Problem 1 is solved because, by introducing
   delays into the new tree, we allow its construction to be carried out on
   demand. Problem 2 is solved because this tree-to-tree transformation can be
   carried out in a purely bottom-up manner by a [reduce] visitor. Then,
   finally, it is straightforward to transform a delayed tree into a
   cascade. *)

(* -------------------------------------------------------------------------- *)

(* A delayed tree contains ordinary nodes of arity 0, 1, and 2. Furthermore,
   it contains [DTDelay] nodes, of arity 1, whose child is delayed, that is,
   computed only on demand. *)

type 'a delayed_tree =
  | DTZero
  | DTOne of 'a
  | DTTwo of 'a delayed_tree * 'a delayed_tree
  | DTDelay of (unit -> 'a delayed_tree)

(* A delayed tree is converted to a cascade as follows. We may choose, at this
   point, between left-to-right and right-to-left traversals. As usual, when
   building a cascade, one must take a continuation [k] as an argument, so as
   to avoid naive and costly cascade concatenation operations. *)

let rec delayed_tree_to_cascade (dt : 'a delayed_tree) (k : 'a cascade)
: 'a cascade =
  fun () -> delayed_tree_to_head dt k

and delayed_tree_to_head (dt : 'a delayed_tree) (k : 'a cascade) : 'a head =
  match dt with
  | DTZero ->
      force k
  | DTOne x ->
      Cons (x, k)
  | DTTwo (dt1, dt2) ->
      delayed_tree_to_head dt1 (delayed_tree_to_cascade dt2 k)
  | DTDelay dt ->
      delayed_tree_to_head (force dt) k

let delayed_tree_to_cascade (dt : 'a delayed_tree) : 'a cascade =
  delayed_tree_to_cascade dt nil

let delayed_tree_to_iterator (dt : 'a delayed_tree) : 'a iterator =
  cascade_to_iterator (delayed_tree_to_cascade dt)

(* -------------------------------------------------------------------------- *)

(* We now set up four constructor functions and constructor methods, which
   construct delayed trees, and which we will use in a [reduce] visitor. *)

(* The type ['a delay] is a synonym for ['a]. It is used as a decoration, in a
   type definition, to indicate that a call to the method [visit_delay] is
   desired. *)

type 'a delay = 'a

class ['self] delayed_tree_monoid = object (_ : 'self)

  (* Delayed trees form a monoid, in the sense that we concatenate them using
     [DTTwo], and the neutral element is [DTZero]. We package these two data
     constructors in the methods [zero] and [plus], which are automatically
     called in an automatically-generated [reduce] visitor. *)

  method zero =
    DTZero

  method plus s1 s2 =
    match s1, s2 with
    | DTZero, s
    | s, DTZero ->
        (* This optimization is not mandatory. It helps allocate fewer nodes. *)
        s
    | (DTOne _ | DTTwo _ | DTDelay _), _ ->
        DTTwo (s1, s2)

  (* The visitor method [visit_delay] delays the visit of a subtree by
     constructing and returning a [DTDelay] node, which carries a delayed
     recursive call to a visitor. *)

  method visit_delay: 'env 'a .
    ('env -> 'a -> 'b delayed_tree) ->
    'env -> 'a delay -> 'b delayed_tree
  = fun visit_'a env x ->
      DTDelay (fun () -> visit_'a env x)

end

(* The visitor function [yield] will be invoked at elements of type ['a].
   It constructs a one-element delayed tree. *)

let yield _env x =
  DTOne x

(* -------------------------------------------------------------------------- *)

(* It is now time to generate a [reduce] visitor for the type ['a sometree].
   This is the only part of the code which is specific of [sometree].
   Everything else is generic. *)

(* We must insert [delay]s into the structure of the type ['a sometree] so as
   to indicate where [visit_delay] should be called and (therefore) where
   [DTDelay] nodes should be allocated. To do this, we write a copy of the
   definition of the type ['a sometree], with extra delays in it. The new type
   is actually considered equal to ['a sometree] by OCaml. Its role is purely
   to carry a [@@deriving visitors] annotation. *)

(* In the data constructor [Node], the left-hand [delay] is in fact
   superfluous. With or without it, our iterators will eagerly descend along
   the leftmost branch of a tree, anyway. *)

type 'a mytree = 'a sometree =
  | Leaf
  | Node of 'a mytree delay * 'a * 'a mytree delay

and 'a mytree_delay =
  'a mytree delay

[@@deriving visitors { variety = "reduce"; polymorphic = true;
                       concrete = true; ancestors = ["delayed_tree_monoid"] }]

(* -------------------------------------------------------------------------- *)

(* For demonstration purposes, let us make our visitor verbose. *)

class ['self] verbose_reduce = object (_ : 'self)
  inherit [_] reduce as super
  method! visit_Leaf visit_'a env =
    Printf.printf "Visiting a leaf.\n%!";
    super#visit_Leaf visit_'a env
  method! visit_Node visit_'a env t1 x t2 =
    Printf.printf "Visiting a node.\n%!";
    super#visit_Node visit_'a env t1 x t2
end

(* In production, one should remove [verbose_reduce] and use [reduce]
   instead. *)

let sometree_to_delayed_tree (t : 'a sometree) =
  new verbose_reduce # visit_mytree_delay yield () t
    (* We use [visit_mytree_delay], even though [visit_mytree] would work
       just as well, so as to ensure that we get a delayed tree whose root
       is a [DTDelay] node. *)

(* Problem solved! *)

let sometree_to_iterator (t : 'a sometree) : 'a iterator =
  delayed_tree_to_iterator (sometree_to_delayed_tree t)

(* -------------------------------------------------------------------------- *)

(* Demo. *)

let t : int sometree =
  Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))

let i : int iterator =
  sometree_to_iterator t

(* Transcript of an OCaml toplevel session:

  # i();;
  Visiting a node.
  Visiting a node.
  Visiting a leaf.
  - : int option = Some 1
  # i();;
  Visiting a leaf.
  - : int option = Some 2
  # i();;
  Visiting a node.
  Visiting a leaf.
  - : int option = Some 3
  # i();;
  Visiting a leaf.
  - : int option = None
  # i();;
  - : int option = None

 *)

(* -------------------------------------------------------------------------- *)

(* Variant: it is possible to use the visitor [sometree_reduce] which was
   generated at the very beginning. This removes the need for defining the
   type [mytree]. The trick is to override the method [visit_sometree] so as
   to insert a delay at every tree node. *)

module Variant1 = struct

  class ['self] reduce = object (self : 'self)
    inherit [_] sometree_reduce as super
    inherit [_] delayed_tree_monoid
    method! visit_sometree visit_'a env t =
      self#visit_delay (super#visit_sometree visit_'a) env t
  end

  (* The rest of the code is unchanged. It is reproduced here for testing. *)

  class ['self] verbose_reduce = object (_ : 'self)
    inherit [_] reduce as super
    method! visit_Leaf visit_'a env =
      Printf.printf "Visiting a leaf.\n%!";
      super#visit_Leaf visit_'a env
    method! visit_Node visit_'a env t1 x t2 =
      Printf.printf "Visiting a node.\n%!";
      super#visit_Node visit_'a env t1 x t2
  end

  let sometree_to_delayed_tree (t : 'a sometree) =
    new verbose_reduce # visit_sometree yield () t

  let sometree_to_iterator (t : 'a sometree) : 'a iterator =
    delayed_tree_to_iterator (sometree_to_delayed_tree t)

  let t : int sometree =
    Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))

  let i : int iterator =
    sometree_to_iterator t

end

(* -------------------------------------------------------------------------- *)

module Variant2 = struct

  (* The function [delayed_tree_to_cascade] could have been written directly
     as follows, without the auxiliary function [delayed_tree_to_head]: *)

  let rec _delayed_tree_to_cascade (dt : 'a delayed_tree) (k : 'a cascade)
  : 'a cascade =
    match dt with
    | DTZero ->
        k
    | DTOne x ->
        cons x k
    | DTTwo (dt1, dt2) ->
        _delayed_tree_to_cascade dt1 (_delayed_tree_to_cascade dt2 k)
    | DTDelay dt ->
        fun () -> _delayed_tree_to_cascade (force dt) k ()

  (* In this form, [delayed_tree_to_cascade] is the only operation that is
     ever applied to a delayed tree, so we can refunctionalize delayed trees,
     that is, wherever we used to build a delayed tree [t], we now directly
     build a closure that is equivalent to [delayed_tree_to_cascade t]. *)

  type 'a producer =
    'a cascade -> 'a cascade

  type 'a delayed_tree =
    'a producer

  let _DTZero k =
    k

  let _DTOne x k =
    cons x k

  let _DTTwo dt1 dt2 k =
    dt1 (dt2 k)

  let _DTDelay dt k =
    fun () -> force dt k ()

  let (_ : 'a delayed_tree) = _DTZero
  let (_ : 'a -> 'a delayed_tree) = _DTOne
  let (_ : 'a delayed_tree -> 'a delayed_tree -> 'a delayed_tree) = _DTTwo
  let (_ : (unit -> 'a delayed_tree) -> 'a delayed_tree) = _DTDelay

  let delayed_tree_to_cascade (dt : 'a delayed_tree) : 'a cascade =
    dt nil

  let delayed_tree_to_iterator (dt : 'a delayed_tree) : 'a iterator =
    cascade_to_iterator (delayed_tree_to_cascade dt)

  (* The delayed monoid uses the new constructors. In [plus], we lose the
     little optimization whereby [DTZero] were recognized and eliminated on
     the fly. *)

  class ['self] delayed_tree_monoid = object (_ : 'self)

    method zero =
      _DTZero

    method plus =
      _DTTwo

    method visit_delay: 'env 'a .
      ('env -> 'a -> 'b delayed_tree) ->
      'env -> 'a delay -> 'b delayed_tree
    = fun visit_'a env x ->
        _DTDelay (fun () -> visit_'a env x)

  end

  let yield _env x =
    _DTOne x

  (* The rest of the code is as before. It is reproduced here for testing. *)

  class ['self] reduce = object (self : 'self)
    inherit [_] sometree_reduce as super
    inherit [_] delayed_tree_monoid
    method! visit_sometree visit_'a env t =
      self#visit_delay (super#visit_sometree visit_'a) env t
  end

  class ['self] verbose_reduce = object (_ : 'self)
    inherit [_] reduce as super
    method! visit_Leaf visit_'a env =
      Printf.printf "Visiting a leaf.\n%!";
      super#visit_Leaf visit_'a env
    method! visit_Node visit_'a env t1 x t2 =
      Printf.printf "Visiting a node.\n%!";
      super#visit_Node visit_'a env t1 x t2
  end

  let sometree_to_delayed_tree (t : 'a sometree) =
    new verbose_reduce # visit_sometree yield () t

  let sometree_to_iterator (t : 'a sometree) : 'a iterator =
    delayed_tree_to_iterator (sometree_to_delayed_tree t)

  let t : int sometree =
    Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))

  let i : int iterator =
    sometree_to_iterator t

end
