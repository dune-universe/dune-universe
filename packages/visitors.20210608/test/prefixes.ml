class ['self] base = object(_ : 'self)
  method on_int () i j = i + j
end

type inttree = Node of (int * inttree * inttree) | Leaf of int
[@@deriving visitors { variety = "fold2"; visit_prefix = "on_";
                       build_prefix = "mk_"; fail_prefix = "err_";
                       nude = true; ancestors = ["base"]}]

let add_inttree : inttree -> inttree -> int =
  let v = object
    inherit [_] fold2 as super
    method mk_Node () (i, l, r) = i + l + r
    method mk_Leaf () i = i
    method! err_inttree () _l _r = 0
    method! on_inttree = super # on_inttree
  end
  in v # on_inttree ()

let t = Node (1, Leaf 2, Leaf 3)

let (_i : int) =
  add_inttree t t
