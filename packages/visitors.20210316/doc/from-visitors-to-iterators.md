I have been asked whether
an automatically-generated visitor,
as produced by the [visitors](https://gitlab.inria.fr/fpottier/visitors)
syntax extension for OCaml,
can be used to construct an iterator.

It turns out that this can be done
in a simple and efficient manner.
(Up to a constant factor, the time complexity of this solution is optimal.)
As the problem is interesting and
its solution is somewhat nonobvious,
I am describing them here.

To play with this code in an OCaml toplevel,
first install `visitors` via the command `opam install visitors`.
Then, launch `ocaml` and type this:

```
#use "topfind";;
#require "visitors.ppx";;
#require "visitors.runtime";;
```

## Problem Statement

Suppose we have an arbitrary data structure that contains elements
of type `'a`. Here, it is a binary tree, but it could be anything:

```
type 'a sometree =
  | Leaf
  | Node of 'a sometree * 'a * 'a sometree
```

We would like to enumerate the elements of this data structure.
More precisely, we would like to construct an iterator, that is,
an on-demand producer of elements. Here is a simple definition
of a stateful iterator:

```
type 'a iterator =
  unit -> 'a option
```

The question is, can we construct an iterator for the type `'a sometree`,
based on an automatically-generated visitor, so that the construction is
entirely independent of the type `'a sometree`?

## Cascades

For starters, let us define cascades, which are a more pleasant kind of
iterators. A cascade is a persistent (stateless) iterator. It can be thought
of as a delayed list, that is, a list whose elements are computed only on
demand.

Cascades could (should) be part of a separate library.
As the time of writing (March, 2017), there is in fact
[a proposal](https://github.com/ocaml/ocaml/pull/1002)
to add them to OCaml's standard library.

```
type 'a cascade =
  unit -> 'a head

and 'a head =
  | Nil
  | Cons of 'a * 'a cascade
```

A delayed computation is represented as a function of type `unit -> _`.

The cascade constructors are as follows:

```
let nil : 'a cascade =
  fun () -> Nil

let cons (x : 'a) (xs : 'a cascade) : 'a cascade =
  fun () -> Cons (x, xs)
```

Forcing a cascade reveals its head:

```
let force xs =
  xs()
```

A cascade can be easily converted to a stateful iterator:

```
let cascade_to_iterator (xs : 'a cascade) : 'a iterator =
  let s = ref xs in
  fun () ->
    match force !s with
    | Nil ->
        s := nil;
        None
    | Cons (x, xs) ->
        s := xs;
        Some x
```

In the above code, writing `nil` into `s` may seem superfluous, but is in fact
necessary to guarantee that the computation that led to a `Nil` outcome is not
repeated in the future.

Because cascades are close cousins of lists, they are easy to work with.
Constructing a cascade for a tree-like data structure is straightforward,
whereas directly constructing a stateful iterator would be more involved.

## Back to the problem

Now, can we use some kind of visitor to turn a tree of type `'a sometree`
into a cascade of type `'a cascade`?

At first sight, this does not seem very easy, because of two issues:

 * a visitor usually traverses a tree in an eager manner, whereas we need the
   traversal to make progress only as cascade elements are demanded;

 * a visitor performs a bottom-up computation, without a left-to-right bias
   (assuming mutable state is not used), whereas a cascade enumerates elements
   in a left-to-right manner. (Or in a right-to-left manner. As will be
   apparent below, both directions are possible.)

The trick is to use another intermediate step. Instead of turning a tree
directly into a cascade, we first transform it into a generic tree-like
structure: a *delayed tree*. The first issue above is solved because, by
introducing delays into the new tree, we allow its construction to be carried
out on demand. The second issue is solved because this tree-to-tree
transformation can be carried out in a purely bottom-up manner by a `reduce`
visitor. Then, finally, it is straightforward to transform a delayed tree into
a cascade.

## From Delayed Trees to Cascades and Iterators

A delayed tree contains ordinary nodes of arity 0, 1, and 2. Furthermore,
it contains `DTDelay` nodes, of arity 1, whose child is delayed, that is,
computed only on demand.

```
type 'a delayed_tree =
  | DTZero
  | DTOne of 'a
  | DTTwo of 'a delayed_tree * 'a delayed_tree
  | DTDelay of (unit -> 'a delayed_tree)
```

A delayed tree is converted to a cascade as follows. As is often the case,
when building a cascade, one must take a continuation `k` as an argument, so
as to avoid naive and costly cascade concatenation operations. Thus, the
specification of the function call `delayed_tree_to_cascade dt k` is to
construct a cascade whose elements are the elements of the delayed tree `dt`
(listed from left to right), followed with the elements of the cascade `k`.

```
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
```

In the above code, we have chosen to perform a left-to-right traversal of
the delayed tree, but could just as well have chosen a right-to-left traversal.

It is possible to make the delayed tree data structure implicit in the code;
this is explained [further on](#variant-getting-rid-of-explicit-delayed-trees).

## Constructing Delayed Trees

The type `'a delay` is a synonym for `'a`. We will use it as a decoration, in
a type definition, to indicate that a call to the method `visit_delay` is
desired.

```
type 'a delay = 'a
```

We now set up four constructor functions or constructor methods, which
construct delayed trees, and which we will use in a `reduce` visitor.

Delayed trees form a monoid, in the sense that we concatenate them using
`DTTwo`, and the neutral element is `DTZero`. We package these two data
constructors in the methods `zero` and `plus`, which are automatically called
in an automatically-generated `reduce` visitor.

The visitor method `visit_delay` delays the visit of a subtree by constructing
and returning a `DTDelay` node, which carries a delayed recursive call to a
visitor.

The visitor function `yield` will be invoked at elements of type `'a`. It
constructs a one-element delayed tree.

```
class ['self] delayed_tree_monoid = object (_ : 'self)

  method zero =
    DTZero

  method plus s1 s2 =
    match s1, s2 with
    | DTZero, s
    | s, DTZero ->
        s
    | _, _ ->
        DTTwo (s1, s2)

  method visit_delay: 'env 'a .
    ('env -> 'a -> 'b delayed_tree) ->
    'env -> 'a delay -> 'b delayed_tree
  = fun visit_'a env x ->
      DTDelay (fun () -> visit_'a env x)

end

let yield _env x =
  DTOne x
```

In the method `plus`, above, treating `DTZero` specially is not mandatory. It
is an optimization, which helps allocate fewer nodes.

## Generating a Visitor

It is now time to generate a `reduce` visitor for the type `'a sometree`.
This is the only part of the code which is specific of `sometree`.
Everything else is generic.

We must insert *delays* into the structure of the type `'a sometree` so as to
indicate where `visit_delay` should be called and (therefore) where `DTDelay`
nodes should be allocated. To do this, we write a copy of the definition of
the type `'a sometree`, with extra uses of `delay` in it. The new type is
actually considered equal to `'a sometree` by OCaml. Its role is purely to
carry a `@@deriving visitors` annotation.

In the data constructor `Node`, the left-hand `delay` is in fact superfluous.
With or without it, our iterators will eagerly descend along the leftmost
branch of a tree, anyway.

```
type 'a mytree = 'a sometree =
  | Leaf
  | Node of 'a mytree delay * 'a * 'a mytree delay

and 'a mytree_delay =
  'a mytree delay

[@@deriving visitors { variety = "reduce"; polymorphic = true;
                       concrete = true; ancestors = ["delayed_tree_monoid"] }]
```

This approach is pleasant insofar as one controls exactly where delays are
inserted. However, it requires copying the type definition, which may be
unpleasant. Another approach is described
[further on](#variant-avoiding-duplication-of-the-type-definition).

## Using the Generated Visitor

For demonstration purposes, let us make our visitor verbose. In production,
one should remove `verbose_reduce` and use `reduce` instead.

```
class ['self] verbose_reduce = object (_ : 'self)
  inherit [_] reduce as super
  method! visit_Leaf visit_'a env =
    Printf.printf "Visiting a leaf.\n%!";
    super#visit_Leaf visit_'a env
  method! visit_Node visit_'a env t1 x t2 =
    Printf.printf "Visiting a node.\n%!";
    super#visit_Node visit_'a env t1 x t2
end
```

The problem is solved! There remains to write a couple lines of glue code:

```
let sometree_to_delayed_tree (t : 'a sometree) =
  new verbose_reduce # visit_mytree_delay yield () t

let sometree_to_iterator (t : 'a sometree) : 'a iterator =
  delayed_tree_to_iterator (sometree_to_delayed_tree t)
```

We use `visit_mytree_delay` above, even though `visit_mytree` would work just
as well, so as to ensure that we get a delayed tree whose root is a `DTDelay`
node. Thus, absolutely no work is performed when the iterator is created;
iteration begins only when the first element is demanded.

## Demo

A little demo helps see what is going on.

```
let t : int sometree =
  Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf))

let i : int iterator =
  sometree_to_iterator t
```

Here is a transcript of an OCaml toplevel session:

```
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
```

## Variant: Avoiding Duplication of the Type Definition

Earlier, we have generated a visitor for the existing type `'a sometree` in *a
posteriori* style. We have manually created an isomorphic copy of the type `'a
sometree`, which we have named `'a mytree`, and have annotated this copy with
`[@@deriving visitors { ... }]`. Furthermore, we have taken this opportunity
to insert `delay` type constructors into the type, so as to influence the
generated visitor.

This style is relatively pleasant because it is declarative and lets us
control exactly where delays are inserted. However, it requires duplicating
the definition of the type `'a sometree`, which may be unpleasant (if the
definition is large) or impossible (if the definition is hidden behind an
abstraction barrier).

Another approach is to generate a visitor in *a priori* style. When the type
`'a sometree` is first defined, a `reduce` visitor can be immediately
generated for it, as follows:

```
type 'a sometree =
  | Leaf
  | Node of 'a sometree * 'a * 'a sometree

[@@deriving visitors { variety = "reduce"; polymorphic = true;
                       name = "sometree_reduce" }]
```

At this point, we pretend that we do not know yet what this visitor will be
used for, so we do not annotate the type definition with `delay`, and do not
use `delayed_tree_monoid` as a base class. We get a visitor class, named
`sometree_reduce`. This class has two virtual methods, `zero` and `plus`.

Then, we create a subclass, named `reduce`, which we tailor to our needs.
We mix the generated class `sometree_reduce` with the class `delayed_tree_monoid`,
and insert a delay at every tree node by overriding the method `visit_sometree`:

```
class ['self] reduce = object (self : 'self)
  inherit [_] sometree_reduce as super
  inherit [_] delayed_tree_monoid
  method! visit_sometree visit_'a env t =
    self#visit_delay (super#visit_sometree visit_'a) env t
end
```

The rest of the code is unchanged (except the method `visit_mytree_delay` no
longer exists; one calls `visit_sometree` instead).

## Variant: Getting Rid of Explicit Delayed Trees

I like to present delayed trees as an explicit data structure, because this
helps understand what is going on. However, if desired, it is possible to
hide them by *refunctionalization* (the opposite of *defunctionalization*).

Above, the function `delayed_tree_to_cascade` was written with the help of an
auxiliary function, `delayed_tree_to_head`. We could also have written it
directly, as follows:

```
let rec delayed_tree_to_cascade (dt : 'a delayed_tree) (k : 'a cascade)
: 'a cascade =
  match dt with
  | DTZero ->
      k
  | DTOne x ->
      cons x k
  | DTTwo (dt1, dt2) ->
      delayed_tree_to_cascade dt1 (delayed_tree_to_cascade dt2 k)
  | DTDelay dt ->
      fun () -> delayed_tree_to_cascade (force dt) k ()
```

In this form, the only function that is ever applied to a delayed tree is
`delayed_tree_to_cascade`. Therefore, wherever we construct a delayed tree
`t`, we could instead directly build a closure whose behavior is equivalent to
`delayed_tree_to_cascade t`. This is *refunctionalization*.

The data structure of delayed trees seems to disappears.
The type `'a delayed_tree` can be defined as a synonym
for a `'a cascade -> 'a cascade`. I usually refer to
this type as `'a producer`: it is the type of a function
that concatenates elements in front of the cascade that it
receives as an argument.

```
type 'a producer =
  'a cascade -> 'a cascade

type 'a delayed_tree =
  'a producer
```

The four data constructors are defined as follows:

```
let _DTZero k =
  k

let _DTOne x k =
  cons x k

let _DTTwo dt1 dt2 k =
  dt1 (dt2 k)

let _DTDelay dt k =
  fun () -> force dt k ()
```

The reader can check that the types of these data constructors are the
same as in the previous version of the code. For instance, `_DTDelay`
has type `(unit -> 'a delayed_tree) -> 'a delayed_tree`.

The root function `delayed_tree_to_cascade` (without a continuation argument)
is defined as follows:

```
let delayed_tree_to_cascade (dt : 'a delayed_tree) : 'a cascade =
  dt nil
```

The delayed tree monoid uses the new versions of the four data constructors:

```
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
```

The four functions `_DTZero`, `_DTOne`, `_DTTwo`, and `_DTDelay` could be
inlined, if desired, so as to make the above code seem even more concise.

The rest of the code is unchanged.

## Acknowledgements

KC Sivaramakrishnan asked whether a visitor can be used to construct an
iterator. Gabriel Scherer pointed out that the delayed tree data structure can
be hidden by refunctionalization.
