(** Phantom algebra interfaces

    This modules provides the interface implemented by the core
    module, splits in smaller logical units.

    There are intended to be used by people interested by the
    type-level interface but not by the simple OCaml implementation provided.

*)

open Type_functions
type k = float
exception Unexpected_ranks of int list

module type Core = sig

  type (+'dim,+'rank) t
  (** Tensor core type:
      - rank is either 2 (for matrix), 1 (for vector) or zero for scalars
      - dim belongs to \{1,2,3,4\}.
  *)

  (** Printer function *)
  val pp: Format.formatter -> ('dim,'rank) t -> unit


  (** {1 Type abreviations} *)
  type +'x scalar = ('a one, 'b z) t constraint 'x = 'a * 'b

  type +'x vec2 = ('a two,'b one) t constraint 'x = 'a * 'b
  type +'x vec3 = ('a three,'b one) t constraint 'x = 'a * 'b
  type +'x vec4 = ('a four,'b one) t constraint 'x = 'a * 'b

  type +'x mat2 = ('a two,'b two) t constraint 'x = 'a * 'b
  type +'x mat3 = ('a three,'b two) t constraint 'x = 'a * 'b
  type +'x mat4 = ('a four,'b two) t constraint 'x = 'a * 'b

  (** {1 Constructors} *)
  val scalar: k -> _ scalar
  val (~+): k -> _ scalar
  (** [scalar f] build a scalar from the naked scalar type.
      It can be abbreviated to [(+s)].
  *)

  val vec2: k -> k -> _ vec2
  val vec3: k -> k -> k -> _ vec3
  val vec4: k -> k -> k -> k -> _ vec4

  val mat2: _ vec2 -> _ vec2 -> _ mat2
  val mat3: _ vec3 -> _ vec3 -> _ vec3 -> _ mat3
  val mat4: _ vec4 -> _ vec4 -> _ vec4 -> _ vec4 -> _ mat4


  (** {1 Vector stretching and concatenation } *)

  (** [vec$n' v] extends a vector of dimension [d <= n]
      to a vector of dimension n by repeating the last value
      of the vector *)
  val vec2': ([< _ one | _ two], [< _ one | _ z] ) t -> _ vec2
  val vec3': ([< _ one | _ two| _ three] , [< _ one | _ z] ) t -> _ vec3
  val vec4':
    ([< _ one | _ two| _ three | _ four ] , [< _ one | _ z] ) t -> _ vec4

  (** Vector concatenation :
      [ v |+| w ] is [(v_0, v_1, … , v_{dim-1}, w_0, …, w_{dim-1})]*)
  val ( |+| ): (('dim1,'dim2,'dim3,_) nat_sum, [< _ one | _ z] ) t ->
    ('dim2, [< _ one | _ z] ) t -> ('dim3, _ one) t

  (** {1 Map functions} *)

  val map: (k -> k ) -> ('dim,'rank) t -> ('dim,'rank) t
  val map2: (k -> k -> k ) -> ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t


      (** {2 Core linear algebra functions} *)

  (**
      [x + y] is the standard vector sum, except for scalar argument
      which are broadcasted to a constant tensor *)
  val (+): ('dim1,('rank1,'rank2,'rank3,'dim1,'dim2,'dim3, _) sum ) t
    -> ('dim2,'rank2) t -> ('dim3,'rank3) t


  (** [x <+> y] is the standard vector sum, without broadcasting *)
  val (<+>): ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t

  (** [~-x] is the standard addition inverse *)
  val (~-): ('dim,'rank) t -> ('dim,'rank) t

  (** [x - y] is the standard vector difference, except for scalar
      argument which are broadcasted to a constant tensor *)
  val (-): ('a,('rank1,'rank2,'rank3,'dim1,'dim2,'dim3, _) sum ) t
    -> ('a,'rank2) t -> ('a,'rank3) t

  (** [x <-> y] is the standard vector difference, without broadcasting *)
  val (<->): ('dim,'rank) t -> ('dim,'rank) t -> ('dim,'rank) t

  (** [ x * y] is:
      - the external product if x or y is a scalar
      - the matrix product if x or y is a matrix
      - the element-wise (hadamard) product otherwise
  *)
  val ( * ) : ('dim1, ('rank1, 'rank2, 'rank3,'dim1,'dim2,'dim3, _ ) product) t
    -> ('dim2, 'rank2) t ->
    ('dim3,'rank3) t

  (** [ x / y] is:
      - the external product division if x or y is a scalar
      - the right matrix division if y is a matrix and x is either a matrix
        or a vector
      - the element-wise division if both x and y are vectors
  *)
  val ( / ) : ('dim1, ('rank1, 'rank2, 'rank3,'dim1,'dim2,'dim3, _ ) div) t
    -> ('dim2, 'rank2) t ->
    ('dim3,'rank3) t

  (** {2 Exponentiation functions } *)

  (** [ t ** k] is [ t * … * t ] k-times *)
  val ( ** ) : ('dim,'rank) t -> int -> ('dim,'rank) t

  (** [exp] is the algebraic exponential:
      [exp m = 1 + m + m ** 2 / 2 + m **3 / 3! + … ] *)
  val exp: ('dim,'rank) t -> ('dim,'rank) t


  (** {2 Scalar products and norms} *)

  (** [ (x|*|y)] is the canonical scalar product *)
  val ( |*| ) : ('dim, 'rank) t -> ('dim,'rank) t -> k

  (** [norm x] is the canonical 2-norm of x *)
  val norm:  ('dim, 'rank) t -> k

  (** [normalize x] is [x / scalar (norm x)] *)
  val normalize: ('dim,'rank) t -> ('dim,'rank) t

  (** [orthonomalize [v_0;...;v_n]] returns a list of orthonormal vectors
      [[w_0;...;w_k]] that spans the same vector subspace as [v_0;...;v_n].
      If the family [[v_0;...;v_n]] was free then [k = n], otherwise [k<n]. *)
  val orthonormalize: ('dim, _ one) t list -> ('dim, _ one) t list

  (** [distance x y] is [norm (x - y)] *)
  val distance: ('dim,'rank) t -> ('dim,'rank) t -> k

  (** [norm_1 x] is ∑ |x_i| *)
  val norm_1:  ('dim, 'rank) t -> k

  (** [norm_q q x] is (∑ |x_i|^q) ^ 1/q *)
  val norm_q: float -> ('dim, 'rank) t -> k

  (** {2 Cross and external product }*)

  (** [cross v w] is the cross product, it maps either two 3d vectors to
      a 3d pseudo-vector, or two 2d vectors to a scalar *)
  val cross:  ( ('dim, 'dim2 * 'rank2, _ ) cross , _ one) t ->
    ('dim, _ one) t -> ('dim2, 'rank2) t

  (** See {!cross} for the 2d and 3d cross-product for vectors
      [ v ^ w ] is the infinitesimal rotation matrix in the plane
      generated by [v] and [w] with an amplitude [|v||w| sin θ ].
      In other words the matrix representation of the 2-form [dv ^ dw] in
      the corresponding graded algebra.
  *)
  val ( ^ ): ('dim, _ one) t -> ('dim, _ one ) t -> ('dim, _ two ) t

  (** {2 More linear algebra functions } *)

  (** [commutator m n] is [m * n - n * m] *)
  val commutator: ('dim, _ two) t -> ('dim, _ two) t -> ('dim, _ two) t

  (** [anticommutator m n] is [m * n + n * m] *)
  val anticommutator: ('dim, _ two) t -> ('dim, _ two) t -> ('dim, _ two) t

  (** [trace m] is [∑_i m_ii] *)
  val trace: ('dim, _ two) t -> k

  (** [det m] is the signed volume of the convex hull of
      of the matrix rows *)
  val det: ('dim, _ two) t -> k

  (** [transpose m] is the matrix with row and column reversed *)
  val transpose: ('dim, _ two) t -> ('dim, _ two) t
end

(** {1 Indexing }

    The two following module types provide a definition for indices
    and their associated type-level values and the indexing and
    slicing functions.

*)

module type Index = sig
(** Index data type *)

  type (+'dim,+'len,+'rank,+'group) index
(**  An index of type [(+'dim,'+len,+'rank,'group) index]
     can be used to index a tensor, each type parameter informs
     on which kind of tensor can be used (['dim] and ['rank]),
     on the type of the resulting vector (['len] and ['rank]),
     or on with which indices it can be combined when swizzling.
     More precisely,
     - [`dim] is the list of tensor dimension compatible with the index,
     for instance `x` works for all dimension, whereas `w` is only meaningful
     for a 4-vector.
     - [`rank] is the number of coordinate specified by the index, a tensor
     can be indexed only if its own rank is equal or superior to the index rank.
     For instance, [xx] is a rank 2 tensor and can only index matrices, whereas
     [x] is a valid index for both vector and tensor.
     When slicing, the rank of the slice will be the difference between the
     tensor rank and the index rank. For a vector [v] and a matrix [m],
     v.%[x] is a scalar ([1 - 1 = 0]) like [m.%[xx]] ( [ 2 - 2 = 0]) but
     m.%[x] is a vector ([2-1=0]) corresponding to the first row of the matrix
     - ['len] is the number of indices combined in the aggregated index by
     swizzling, if ['len > 1] it increases the rank of the resulting tensor
     by one and sets its dimension to ['len]. See the slice function for
     more information.
     - ['group] corresponds to the index namespace, only index of the
     same namespace can be combined by swizzling. Availaibles namespace
     are [`xyzx],[`rgba] and [`stpq].
 *)


  (** Index concatenation *)
  val (&): ( 'dim, ('len1,'len2,'len3,_) simple_sum, 'rank, 'group) index
    -> ('dim,'len2,'rank,'group) index
    -> ('dim,'len3,'rank,'group) index


  (** {1 XYZW group} *)
  val x': ([< _ one| _ two| _ three| _ four], _ one, _ one , [`xyzw]) index
  val y': ([< _ two| _ three| _ four], _ one, _ one , [`xyzw]) index
  val z': ([< _ three| _ four], _ one, _ one , [`xyzw]) index
  val w': ([< _ four], _ one, _ one , [`xyzw]) index

  val xx': ([< _ one|_ two|_ three|_ four], _ one, _ two, [`xyzw]) index
  val yx': ([< _ two|_ three|`four], _ one, _ two, [`xyzw]) index
  val zx': ([< _ three|`four], _ one, _ two, [`xyzw]) index
  val wx': ([< _ four], _ one, _ two, [`xyzw]) index
  val xy': ([< _ two|_ three|_ four], _ one, _ two, [`xyzw]) index
  val yy': ([< _ two|_ three|_ four], _ one, _ two, [`xyzw]) index
  val zy': ([< _ three|_ four], _ one, _ two, [`xyzw]) index
  val wy': ([< _ four], _ one, _ two, [`xyzw]) index
  val xz': ([<_ three|_ four], _ one, _ two, [`xyzw]) index
  val yz': ([< _ three|_ four], _ one, _ two, [`xyzw]) index
  val zz': ([< _ three|_ four], _ one, _ two, [`xyzw]) index
  val wz': ([< _ four], _ one, _ two, [`xyzw]) index
  val xw': ([ | _ four], _ one, _ two, [`xyzw]) index
  val yw': ([ | _ four], _ one, _ two, [`xyzw]) index
  val zw': ([ | _ four], _ one, _ two, [`xyzw]) index
  val ww': ([ | _ four], _ one, _ two, [`xyzw]) index


  (** {1 RGBA} *)
  val r': ([< _ one| _ two| _ three| _ four], _ one, _ one , [`rgba]) index
  val g': ([< _ two| _ three| _ four], _ one, _ one , [`rgba]) index
  val b': ([< _ three| _ four], _ one, _ one , [`rgba]) index
  val a': ([< _ four], _ one, _ one , [`rgba]) index

  val rr': ([< _ one|_ two|_ three|_ four], _ one, _ two, [`rgba]) index
  val gr': ([< _ two|_ three|`four], _ one, _ two, [`rgba]) index
  val br': ([< _ three|`four], _ one, _ two, [`rgba]) index
  val ar': ([< _ four], _ one, _ two, [`rgba]) index
  val rg': ([< _ two|_ three|_ four], _ one, _ two, [`rgba]) index
  val gg': ([< _ two|_ three|_ four], _ one, _ two, [`rgba]) index
  val bg': ([< _ three|_ four], _ one, _ two, [`rgba]) index
  val ag': ([< _ four], _ one, _ two, [`rgba]) index
  val rb': ([<_ three|_ four], _ one, _ two, [`rgba]) index
  val gb': ([< _ three|_ four], _ one, _ two, [`rgba]) index
  val bb': ([< _ three|_ four], _ one, _ two, [`rgba]) index
  val ab': ([< _ four], _ one, _ two, [`rgba]) index
  val ra': ([ | _ four], _ one, _ two, [`rgba]) index
  val ga': ([ | _ four], _ one, _ two, [`rgba]) index
  val ba': ([ | _ four], _ one, _ two, [`rgba]) index
  val aa': ([ | _ four], _ one, _ two, [`rgba]) index

    (** {1 STPQ group} *)
  val s': ([< _ one| _ two| _ three| _ four], _ one, _ one , [`stpq]) index
  val t': ([< _ two| _ three| _ four], _ one, _ one , [`stpq]) index
  val p': ([< _ three| _ four], _ one, _ one , [`stpq]) index
  val q': ([< _ four], _ one, _ one , [`stpq]) index

  val ss': ([< _ one|_ two|_ three|_ four], _ one, _ two, [`stpq]) index
  val ts': ([< _ two|_ three|`four], _ one, _ two, [`stpq]) index
  val ps': ([< _ three|`four], _ one, _ two, [`stpq]) index
  val qs': ([< _ four], _ one, _ two, [`stpq]) index
  val st': ([< _ two|_ three|_ four], _ one, _ two, [`stpq]) index
  val tt': ([< _ two|_ three|_ four], _ one, _ two, [`stpq]) index
  val pt': ([< _ three|_ four], _ one, _ two, [`stpq]) index
  val qt': ([< _ four], _ one, _ two, [`stpq]) index
  val sp': ([<_ three|_ four], _ one, _ two, [`stpq]) index
  val tp': ([< _ three|_ four], _ one, _ two, [`stpq]) index
  val pp': ([< _ three|_ four], _ one, _ two, [`stpq]) index
  val qp': ([< _ four], _ one, _ two, [`stpq]) index
  val sq': ([ | _ four], _ one, _ two, [`stpq]) index
  val tq': ([ | _ four], _ one, _ two, [`stpq]) index
  val pq': ([ | _ four], _ one, _ two, [`stpq]) index
  val qq': ([ | _ four], _ one, _ two, [`stpq]) index
end


module type Indexing = sig

  type (+'dim,+'len,'rank,'group) index
  type (+'dim,+'rank) t

  (** [slice t n] or [ t.%[n] ] computes a slice of rank
      [tensor_rank - index_rank], in other words for a vector [v]
      and a matrix [m], [v.%[x]] and [m.%[xx]] are a scalar, whereas
      [m.%[x]] is the first row vector of the matrix [m] *)
  val slice: ('dim1,('rank1,'rank2,'rank3, 'dim1,'dim3,'len,_) superindexing) t
    -> ('dim1, 'len, 'rank2, 'group) index -> ('dim3,'rank3) t

#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
  val (.%[]): ('dim1,('rank1,'rank2,'rank3, 'dim1,'dim3,'len,_) superindexing) t
    -> ('dim1, 'len, 'rank2, 'group) index -> ('dim3,'rank3) t
#endif

  (** [t.%(x)] returns the value of the tensor at index [x] *)
  val get: ('dim,'rank) t -> ('dim,_ one, 'rank, 'group) index -> k

#if OCAML_MAJOR>=4 && OCAML_MINOR>=6
  val (.%()): ('dim,'rank) t -> ('dim, _ one,'rank,'group) index -> k
  #endif
end

module type Basic = sig
  (** Some useful linear algebra values and functions *)

  type 'a dim
  type 'a rank
  type (+'a,+'b) tensor

  (** [zero dim rank] is the zero scalar, vector or matrix
      with dimension [dim] *)
  val zero: 'dim dim -> 'rank rank -> ('dim, 'rank) tensor

  (** [id rank dim] [t ** 0] for any tensor of corresponding rank
      and dimension *)
  val id:  'dim dim -> 'rank rank -> ('dim,'rank) tensor

  (** [eye dim] is [id matrix dim], the identity matrix with ones
      on the diagonal *)
    val eye: 'a dim -> ('a, _ two) tensor

  (** [diag vec] is the diagonal matrix with [vec] on the diagonal *)
    val diag: ('dim, _ one) tensor -> ('dim, _ two) tensor

  (** [rotation x y θ] computes the rotation matrix in the plane
      spanned by x y with a θ angle. *)
  val rotation: ('dim,_ one) tensor -> ('dim,_ one) tensor -> k
    -> ('dim,_ two) tensor
end


(** {1 Advanced modules }
    The following functions are useful when writing higher functions
    of which the output depends on the dimension or the rank,
    or with input of arbitrary dimensions or ranks.
    Note that type errors often become atrocious in this use case
 *)


module type Rank = sig
  (** Rank-related types *)

  (** Tensor rank: scalar, vector or matrix *)
  type +_ rank
  val scalar: _ z rank
  val vector: _ one rank
  val matrix: _ two rank
  val rank_to_int: _ rank -> int
end



module type Dim = sig
  (** Dimension-related types *)

  (** Dimension for vectors and matrix, 1d vectors are considered scalars *)
  type +_ dim

  val d1: _ one dim
  val d2: _ two dim
  val d3: _ three dim
  val d4: _ four dim

  val dim_to_int: _ dim -> int
end


module type Matching = sig
  (** Pattern matching over dimension or rank *)
  type +'a dim
  type +'a rank

  val rank_match:
    [< `zero of 'a & 'r | `one of 'b & 'r | `two of 'c & 'r ] rank
    -> ( _   z rank   -> 'a )
    -> ( _ one rank -> 'b )
    -> ( _ two rank -> 'c )
    -> 'r

  val dim_match:
    [< `one of 'a & 'r | `two of 'b & 'r | `three of 'c & 'r
    | `four of 'd & 'r ] dim
    -> ( _   one dim -> 'a )
    -> ( _   two dim -> 'b )
    -> ( _ three dim -> 'c )
    -> ( _  four dim -> 'd )
    -> 'r
end

module type Cloning = sig
  (** Cloning a value is useful to duplicate the type-level information
      associated to this value when this value is provided as an
      function argument.
      This useful when using multiple time this value with functions performing
      different type-level computations.
*)

  type (+'dim,+'rank) t

  (** [clone_2 v] returns two clones [x,y] of the value [v] with
      the same types as the original type of [ v] *)
  val clone_2:
    (
      [< `one   of ('dim1 * 'dim2 as 't)
                    & _ one   * _ one
    |  `two   of 't & _ two   * _ two
    |  `three of 't & _ three * _ three
    |  `four  of 't & _ four  * _ four
    ],
    [< `zero of ('rank1 * 'rank2 as 'r)
                 & _ z   * _ z
    | `one of 'r & _ one * _ one
    | `two of 'r & _ two * _ two
    ]) t -> ('dim1,'rank1) t * ('dim2,'rank2) t

  (** [clone_k] are not strictly required, but they
      are here to avoid the pattern
      {[ let a, t = clone_2 t in
         let b, t = clone_2 t in
         …
    ]} required by the sole use of [clone_2]
  *)
  val clone_3:
    (
      [< `one   of ('dim1 * 'dim2 * 'dim3 as 't)
                    & _ one   * _ one   * _ one
    |  `two   of 't & _ two   * _ two   * _ two
    |  `three of 't & _ three * _ three * _ three
    |  `four  of 't & _ four  * _ four  * _ four
    ],
    [< `zero of ('rank1 * 'rank2 * 'rank3 as 'r)
                 & _ z   * _ z   * _ z
    | `one of 'r & _ one * _ one * _ one
    | `two of 'r & _ two * _ two * _ two
    ]) t -> ('dim1,'rank1) t * ('dim2,'rank2) t * ('dim3,'rank3) t

  val clone_7:
    (
      [< `one of
           ('dim1 * 'dim2 * 'dim3 * 'dim4 * 'dim5 * 'dim6 * 'dim7 as 'd)
              & _ one   * _ one   * _ one  * _ one  * _ one  * _ one  * _ one
      |  `two of
              'd
              & _ two   * _ two   * _ two  * _ two  * _ two  * _ two  * _ two
      |`three of
              'd
              & _ three * _ three * _ three* _ three* _ three* _ three* _ three
      | `four of
              'd
              & _ four  * _ four  * _ four * _ four * _ four * _ four * _ four
      ],
      [< `zero of
           ('rank1 * 'rank2 * 'rank3 * 'rank4 * 'rank5 * 'rank6 * 'rank7 as 'r)
           & _ z   * _ z   * _ z  * _ z   * _ z   * _ z   * _ z
      | `one of
           'r
           & _ one * _ one * _ one* _ one * _ one * _ one * _ one
      | `two of 'r
                & _ two * _ two * _ two* _ two * _ two * _ two * _ two
      ]) t ->
        ('dim1,'rank1) t
      * ('dim2,'rank2) t
      * ('dim3,'rank3) t
      * ('dim4,'rank4) t
      * ('dim5,'rank5) t
      * ('dim6,'rank6) t
      * ('dim7,'rank7) t

end

(** {1 Full interface}
    The module type [S] combines all previous interface together.
*)

module type S = sig
  include Dim
  type +_ rank
  module Rank: Rank with type 'a rank := 'a rank
  include Core
  include Index
  include Indexing with
    type ('a,'b,'c,'d) index := ('a,'b,'c,'d) index
    and type ('a,'b) t := ('a,'b) t
  include Basic with
    type 'a dim := 'a dim and type 'a rank := 'a rank
    and type ('a,'b) tensor := ('a,'b) t
  include Matching with
    type 'a dim := 'a dim and type 'a rank := 'a rank
  include Cloning with type ('a,'b) t := ('a,'b) t
end
