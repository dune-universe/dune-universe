(** Exceptions raised in vector and matrix implementations. *)
exception Out_of_bounds

exception Dimensions_mismatch

(** Type of generic input vectors (vectors from which we {e get} elements) *)
type ('s, 'i, 'e) vec = Vec of 's * ('i -> 'e)

(** Type of generic output vectors (vectors to which we {e set} elements). *)
type ('s, 'i, 'e, 'w) ovec = OVec of 's * ('i -> 'e -> 'w)

(** Module type of tensor shapes. *)
module type Tensor = sig
  type 'a k

  include Basic_intf.Lang.Shape

  (** The type of [Path.t] allows to specify a rank-one sub-tensor
      out of a rank-n tensor. *)
  module Path : sig
    (** The type of paths. A rank-n tensor is a product of lower rank tensors,
        so a path amounts to a sequence of left-right projects down to a rank-one
        tensor.*)
    type 'a t

    (** The empty path. *)
    val empty : pos t

    (** Select the left sub-tensor. *)
    val l : 'a t -> ('a * 'b) t

    (** Select the right sub-tensor. *)
    val r : 'a t -> ('b * 'a) t

    (** [left ()] evaluates to  [l empty]. *)
    val left : unit -> (pos * 'a) t

    (** [right ()] evaluates to  [l empty]. *)
    val right : unit -> ('a * pos) t
  end

  (** [proj s p] is the rank one component of [s] at the path specified by [p]. *)
  val proj : 'a t -> 'a Path.t -> pos t

  (** Rank one tensor shape with prescribed dimension. *)
  val rank_one : pos m -> pos t

  (** Rank two tensor shape with prescribed dimensions. *)
  val rank_two : pos m -> pos m -> (pos * pos) t

  (** [scalar] is a rank one tensor shape with dimension one. *)
  val scalar : pos t

  (** [empty] is a rank one tensor shape with dimension zero. *)
  val empty : pos t

  (** Dimension along a one-dimensional slice specified by the given path. *)
  val dim : 'a t -> 'a Path.t -> pos m

  (** Total number of positions in the given shape. *)
  val numel : 'a t -> pos m
  (* nb: this assumes that [pos] admits a ring structure which is
     typically the case. *)

  (** Tensor product of shapes. *)
  val tensor : 'a t -> 'b t -> ('a * 'b) t

  (** [concat s1 s2 p] concatenates [s1] and [s2] along the dimension
      specified by the path [p].

      @raise Dimensions_mismatch if [s1] and [s2] have a different shape
      along the orthogonal complement to [p]. *)
  val concat : 'a t -> 'a t -> 'a Path.t -> 'a t k

  (** Project the first component out of a tensor shape. *)
  val fst : ('a * 'b) t -> 'a t

  (** Project the second component out of a tensor shape. *)
  val snd : ('a * 'b) t -> 'b t

  module Morphism : sig
    type 'a obj := 'a t

    include module type of Morphism

    (** [tensor m1 m2] is the parallel composition of [m1] and [m2] *)
    val tensor : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t

    (** [pullback_at_path m p s] extends [m] to [s] by constructing a morphism
        equal to [m] at [p] and acting as the identity on [s] elsewhere.

        @raise Dimensions_mismatch if [range m] is different from [proj s p]. *)
    val pullback_at_path : (pos, pos) t -> 'a Path.t -> 'a obj -> ('a, 'a) t k

    (** [pullback_pointwise m s] extends [m] to [s] by applying it pointwise
        to every position in

        @raise Dimensions_mismatch if any rank one sub-tensor of [s] is
        different from [range m]. *)
    val pullback_pointwise : (pos, pos) t -> 'a obj -> ('a, 'a) t k

    (** [sub ofs dom range] constructs a morphism corresponding to the
        inclusion of [dom], a rank-one tensor of dimension say [m],
        into [range], a rank-one tensor of dimension say [n], such that
        the underlying positions of [dom] are mapped starting at index
        [ofs] in [range].

        Concretely, [sub ofs dom range] constructs a morphism mapping positions
        [n] in [dom] to positions [n + ofs] in [range].

        @raise Dimensions_mismatch if [ofs <= 0 || dim dom <= 0 || ofs + dim dom > dim range]. *)
    val sub : ofs:pos m -> pos obj -> pos obj -> (pos, pos) t k
  end
end

(** Module type of vectors. *)
module type Vec = sig
  (** Ambient monad. *)
  type 'a k

  (** Representation. *)
  type 'a m

  (** Type of shapes. *)
  type 'a shape

  (** Type of shape morphisms. *)
  type ('a, 'b) morphism

  (** Type of elements. *)
  type elt

  type 'i t = ('i shape, 'i m, elt m) vec

  type 'i out = ('i shape, 'i m, elt m, unit m) ovec

  (** Dimensions of an input vector. *)
  val idim : 'i t -> 'i shape

  (** Dimensions of an output vector. *)
  val odim : 'i out -> 'i shape

  (** Creates an input vector from a dimension and a function. *)
  val make : 'i shape -> ('i m -> elt m) -> 'i t

  (** Pullback a vector along a shape morphism. *)
  val pullback : ('j, 'i) morphism -> 'i t -> 'j t k

  (** Get an elemement of an input vector.
      @raise Out_of_bounds if given index is not in the domain of the vector. *)
  val get : 'i t -> 'i m -> elt m k

  (** Get an elemement of an input vector.
      Does not perform bound checking. *)
  val unsafe_get : 'i t -> 'i m -> elt m

  (** Set an elemement in an output vector.
      @raise Out_of_bounds if given index is not in the domain of the vector. *)
  val set : 'i out -> 'i m -> elt m -> unit m k

  val map :
    ('a m -> 'b m) -> ('i shape, 'i m, 'a m) vec -> ('i shape, 'i m, 'b m) vec

  val mapi :
    ('i m -> 'a m -> 'b m) ->
    ('i shape, 'i m, 'a m) vec ->
    ('i shape, 'i m, 'b m) vec

  val map2 :
    ('a m -> 'b m -> 'c m) ->
    ('i shape, 'i m, 'a m) vec ->
    ('i shape, 'i m, 'b m) vec ->
    ('i shape, 'i m, 'c m) vec k

  val map2i :
    ('i m -> 'a m -> 'b m -> 'c m) ->
    ('i shape, 'i m, 'a m) vec ->
    ('i shape, 'i m, 'b m) vec ->
    ('i shape, 'i m, 'c m) vec k

  val assign :
    ('i shape, 'i m, 'a m, unit m) ovec ->
    ('i shape, 'i m, 'a m) vec ->
    ('i shape, 'i m, unit m) vec k

  (** Everywhere zero vector. *)
  val zero : 'i shape -> 'i t

  (** Everywhere one vector. *)
  val one : 'i shape -> 'i t

  (** Constant vector. *)
  val const : 'i shape -> elt m -> 'i t

  (** [basis s i r] is the vector of shape [s] everywhere equal to
      R.zero except at index [i] where it is equal to [r].
      Raises [Out_of_bounds] if [i] does not belong to [s]. *)
  val basis : 'i shape -> 'i m -> elt m -> 'i t k

  (** Pointwise addition.
      Raises [Dimensions_mismatch] if the shape of operands are not equal. *)
  val add : 'i t -> 'i t -> 'i t k

  (** Pointwise subtraction.
      Raises [Dimensions_mismatch] if the shape of operands are not equal. *)
  val sub : 'i t -> 'i t -> 'i t k

  (** Pointwise multiplication.
      Raises [Dimensions_mismatch] if the shape of operands are not equal. *)
  val mul : 'i t -> 'i t -> 'i t k

  (** Pointwise negation. *)
  val neg : 'i t -> 'i t

  (** Multiplication by a scalar. *)
  val smul : elt m -> 'i t -> 'i t

  (** Swapping of indices.
      Raises [Out_of_bounds] if given indices are invalid. *)
  val swap : 'i m -> 'i m -> 'i t -> 'i t k

  (** [iter v] iterates the effectful computation at each index *)
  val iter : ('i shape, 'i m, unit m) vec -> unit m

  (** [reduce op zero v] folds the binary, associative operator [op] over
      the elements of [v] with initial value [zero]. Fold ordering is
      implementation-dependent: consider using commutative operators. *)
  val reduce :
    (elt m -> elt m -> elt m) -> elt m -> ('i shape, 'i m, elt m) vec -> elt m

  (** Vector assignement.
      @raise Dimensions_mismatch if the size of operands are not equal. *)
  val ( := ) :
    ('i shape, 'i m, 'a m, unit m) ovec ->
    ('i shape, 'i m, 'a m) vec ->
    unit m k

  (** Pointwise addition, stores result in first operand.
      @raise Dimensions_mismatch if the size of operands are not equal. *)
  val add_ : 'i out -> 'i t -> 'i t -> unit m k

  (** Pointwise subtraction, stores result in first operand.
      @raise Dimensions_mismatch if the size of operands are not equal. *)
  val sub_ : 'i out -> 'i t -> 'i t -> unit m k

  (** Pointwise multiplication, stores result in first operand.
      @raise Dimensions_mismatch if the size of operands are not equal. *)
  val mul_ : 'i out -> 'i t -> 'i t -> unit m k

  (** Dot product.
      @raise Dimensions_mismatch if the size of operands are not equal. *)
  val dot : 'i t -> 'i t -> elt m k

  module Infix : sig
    (** Alias to the monadic return. *)
    val ( ~! ) : 'i t -> 'i t k

    (** Infix alias for [add]. *)
    val ( + ) : 'i t k -> 'i t k -> 'i t k

    (** Infix alias for [sub]. *)
    val ( - ) : 'i t k -> 'i t k -> 'i t k

    (** Infix alias for [mul]. *)
    val ( * ) : 'i t k -> 'i t k -> 'i t k

    (** Infix alias for [neg]. *)
    val ( ~- ) : 'i t k -> 'i t k

    (** Infix alias for [smul]. *)
    val ( %* ) : elt m -> 'i t k -> 'i t k

    (** Infix alias for [dot]. *)
    val ( <*> ) : 'i t k -> 'i t k -> elt m k

    (** Infix alias for [get]. *)
    val ( .%{} ) : 'i t k -> 'i m -> elt m k

    (** Infix alias for [set]. *)
    val ( .%{}<- ) : 'i out -> 'i m -> elt m -> unit m k

    (** Reexport of [:=]. *)
    val ( := ) :
      ('i shape, 'i m, 'a m, unit m) ovec ->
      ('i shape, 'i m, 'a m) vec k ->
      unit m k
  end
end

(** Module type of matrices. *)
module type Mat = sig
  type 'a k

  type 'a m

  type base_index

  type 'a shape

  type index := base_index * base_index

  include
    Vec
      with type 'a k := 'a k
       and type 'a m := 'a m
       and type 'a shape := 'a shape

  (** Indexing *)
  val index : c:base_index m -> r:base_index m -> index m

  (** [cols m] returns the shape of the columns of [m]. *)
  val cols : index t -> base_index shape

  (** [rows m] returns the shape of the rows of [m] *)
  val rows : index t -> base_index shape

  (** Identity matrix *)
  val identity : base_index shape -> index t

  (** Square matrix with given vector on diagonal *)
  val diagonal : (base_index shape, base_index m, elt m) vec -> index t

  (** Get a column.
      @raise Out_of_bounds if given indices are invalid. *)
  val col :
    index t -> base_index m -> (base_index shape, base_index m, elt m) vec k

  (** Convert a vector into a matrix with this vector as single column. *)
  val of_col : (base_index shape, base_index m, elt m) vec -> index t

  (** Get a row.
      @raise Out_of_bounds if given indices are invalid. *)
  val row :
    index t -> base_index m -> (base_index shape, base_index m, elt m) vec k

  (** Convert a vector into a matrix with this vector as single row. *)
  val of_row : (base_index shape, base_index m, elt m) vec -> index t

  (** Swap two rows.
      @raise Out_of_bounds if given indices are invalid. *)
  val swap_rows : index t -> base_index m -> base_index m -> index t k

  (** Swap two columns.
      @raise Out_of_bounds if given indices are invalid. *)
  val swap_cols : index t -> base_index m -> base_index m -> index t k

  (** Concatenate two matrices horizontally, provided they have
      the same number of rows.
      @raise Dimensions_mismatch if this condition is not verified. *)
  val concat_horiz : index t -> index t -> index t k

  (** Concatenate two matrices vertically, provided they have
      the same number of columns.
      @raise Dimensions_mismatch if this condition is not verified. *)
  val concat_vert : index t -> index t -> index t k

  (** Matrix multiplication.
      @raise Dimensions_mismatch if the nummber of columns of the left-hand side is
      not equal to the number of rows of the right-hand side. *)
  val mm : index t -> index t -> index t k
end
