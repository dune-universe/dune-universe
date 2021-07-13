(** Bitwuzla is an SMT solver for QF_AUFBVFP problems. *)

(** Create a new Bitwuzla session
    ({!val:check_sat} can only be called once). *)
module Once () : sig

  (** {1 Phantom type} *)
  (** Phantom types are annotations that allow the compiler to statically
      catch some sort mismatch errors. Size mismatch errors will still be
      caught at runtime. *)

  (** The bit-vector kind. *)
  type bv = [ `Bv ]

  (** The rounding-mode kind. *)
  type rm = [ `Rm ]

  (** The floating-point kind. *)
  type fp = [ `Fp ]

  (** The array kind with ['a] index and ['b] element. *)
  type ('a, 'b) ar = [ `Ar of ('a -> 'b) ]
    constraint 'a = [< bv | rm | fp ]
    constraint 'b = [< bv | rm | fp ]
  (** Both index and element should be of bit-vector, rounding-mode
      or floating-point kind *)

  (** The function kind taking ['a] argument and
      returning {!type:'b} element. *)
  type ('a, 'b) fn = [ `Fn of ('a -> 'b) ]
    constraint 'b = [< bv ]
  (** Functions accept only bit-vector, rounding-mode or floating-point as
      argument and return only bit-vector. *)

  (** {1 Core types } *)

  (** A sort of ['a] kind. *)
  type 'a sort

  (** A term of ['a] kind. *)
  type 'a term

  (** A value of ['a] kind. *)
  type 'a value = private 'a term
  (** Values are subtype of terms and can be downcasted using [:>] operator. *)

  module Sort : sig

    (** A sort of ['a] kind. *)
    type 'a t = 'a sort

    (** A Boolean sort is a bit-vector sort of size 1. *)
    val bool : bv t

    (**
       [bv size]
       create a bit-vector sort of given size.

       @param size The size of the bit-vector sort.

       @return A bit-vector sort of given size.
    *)
    val bv   : int -> bv t

    (**
       [size sort]
       get the size of a bit-vector sort.

       @param sort The sort.

       @return The size of the bit-vector sort.
    *)
    val size : bv t -> int

    (**
       [fp exp_size size]
       create a floating-point sort of given size with [exp_size] exponent bits.

       @param exp_size The size of the exponent.
       @param size The total size of the floating-point.

       @return A floating-point sort of given format.
    *)
    val fp          : exponent:int -> int -> fp t

    (**
       [exponent sort]
       get the exponent size of a floating-point sort.

       @param sort The sort.

       @return The exponent size of the floating-point sort.
    *)
    val exponent    : fp t -> int

    (**
       [significand sort]
       get the significand size of a floating-point sort.

       @param sort The sort.

       @return The significand size of the floating-point sort.
    *)
    val significand : fp t -> int

    (** A Roundingmode sort. *)
    val rm : rm t

    (**
       [ar index element]
       create an array sort.

       @param index The index sort of the array sort.
       @param element The element sort of the array sort.

       @return An array sort which maps sort [index] to sort [element].
    *)
    val ar      : 'a t -> 'b t -> ('a, 'b) ar t

    (**
       [index sort]
       get the index sort of an array sort.

       @param sort The sort.

       @return The index sort of the array sort.
    *)
    val index   : ('a, 'b) ar t -> 'a t

    (**
       [element sort]
       get the element sort of an array sort.

       @param sort The sort.

       @return The element sort of the array sort.
    *)
    val element : ('a, 'b) ar t -> 'b t

    (** Statically typed list of function argument sorts. *)
    type 'a variadic =
      |  []  : unit variadic
      | (::) : ([< bv | rm | fp ] as 'a) sort * 'b variadic ->
          ('a -> 'b) variadic
      (** Functions accept only bit-vector, rounding-mode or floating-point
          as argument *)

    (**
       [fn domain codomain]
       create a function sort.

       @param domain The domain sorts (the sorts of the arguments).
       @param codomain The codomain sort (the sort of the return value).

       @return A function sort of given domain and codomain sorts.
    *)
    val fn       : 'a variadic -> 'b t -> ('a, 'b) fn t

    (**
       [arity sort]
       get the arity of a function sort.

       @param sort The sort.

       @return The number of arguments of the function sort.
    *)
    val arity    : ('a, 'b) fn t -> int

    (**
       [domain sort]
       get the domain sorts of a function sort.

       @param sort The sort.

       @return The domain sorts of the function sort as an array of sort.
    *)
    val domain   : ('a, 'b) fn t -> 'a variadic

    (**
       [codomain sort]
       get the codomain sort of a function sort.

       @param sort The sort.

       @return The codomain sort of the function sort.
    *)
    val codomain : ('a, 'b) fn t -> 'b t

    (**
       [hash sort]
       compute the hash value for a sort.

       @param sort The sort.

       @return The hash value of the sort.
    *)
    val hash : 'a t -> int

    (**
       [equal sort0 sort1]
       determine if two sorts are equal.

       @param sort0 The first sort.
       @param sort1 The second sort.

       @return [true] if the given sorts are equal.
    *)
    val equal : 'a t -> 'a t -> bool

    (**
       [pp formatter sort]
       pretty print sort.

       @param formatter The outpout formatter
       @param sort The sort.
    *)
    val pp : Format.formatter -> 'a t -> unit

  end

  module Term : sig

    (** A term of ['a] kind. *)
    type 'a t = 'a term
      constraint 'a = [< bv | rm | fp | ('b, 'c) ar ]

    (** Statically typed list of function argument terms. *)
    type 'a variadic =
      |  []  : unit variadic
      | (::) : ([< bv | rm | fp ] as 'a) term * 'b variadic ->
          ('a -> 'b) variadic
      (** Functions accept only bit-vector, rounding-mode or floating-point
          as argument *)

    (** {1 Constructor} *)

    (** Boolean *)
    module Bl : sig
      (** Operation over bit-vectors of size 1. *)

      (** A boolean term. *)
      type t = bv term

      (** A bit-vector value 0 of size 1. *)
      val false' : t

      (** A bit-vector value 1 of size 1. *)
      val true'  : t

      (**
         [of_bool b]
         create a bit-vector value of size 1 from a [bool].

         @param b The boolean value.

         @return Either {!val:true'} or {!val:false'}.
      *)
      val of_bool : bool -> t

      (**
         [of_bv t]
         create a bit-wise {b or} reduction of all bits.

         @param t The bit-vector term.

         @return A term equal to [t <> 0].
      *)
      val of_bv   : bv term -> t

      (**
         [logand t0 t1]
         create a boolean {b and}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [and]
      *)
      val logand  : t -> t -> t

      (**
         [lognand t0 t1]
         create a boolean {b nand}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [bvnand]
      *)
      val lognand : t -> t -> t

      (**
         [redand ts]
         create a {i n}ary boolean {b and}.

         @param ts The boolean terms.

         @return SMT-LIB: [and]
      *)
      val redand  : t array -> t

      (**
         [logor t0 t1]
         create a boolean {b or}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [or]
      *)
      val logor   : t -> t -> t

      (**
         [lognor t0 t1]
         create a boolean {b nor}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [bvnor]
      *)
      val lognor  : t -> t -> t

      (**
         [redor ts]
         create a {i n}ary boolean {b or}.

         @param ts The boolean terms.

         @return SMT-LIB: [or]
      *)
      val redor   : t array -> t

      (**
         [logxor t0 t1]
         create a boolean {b xor}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [xor]
      *)
      val logxor  : t -> t -> t

      (**
         [logxnor t0 t1]
         create a boolean {b xnor}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [bvxnor]
      *)
      val logxnor : t -> t -> t

      (**
         [redxor ts]
         create a {i n}ary boolean {b xor}.

         @param ts The boolean terms.

         @return SMT-LIB: [xor]
      *)
      val redxor  : t array -> t

      (**
         [lognot t]
         create a boolean {b not}.

         @param t The boolean term.

         @return SMT-LIB: [not]
      *)
      val lognot  : t -> t

      (**
         [iff t0 t1]
         create a boolean {b if and only if}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [=]
      *)
      val iff     : t -> t -> t

      (**
         [implies t0 t1]
         create a boolean {b implies}.

         @param t0 The first boolean term.
         @param t1 The second boolean term.

         @return SMT-LIB: [=>]
      *)
      val implies : t -> t -> t

      (**
         [assignment t]
         get boolean representation of the current model value of given
         boolean term.

         @param t The term to query a model value for.

         @return boolean representation of current model value of term [t].
      *)
      val assignment : bv value -> bool

    end

    (** Bit-vector *)
    module Bv : sig

      (** A bit-vector term. *)
      type t = bv term

      (**
         [zero sort]
         create a bit-vector value zero.

         @param sort The sort of the value.

         @return A term representing the bit-vector value 0 of given sort.
      *)
      val zero : bv sort -> t

      (**
         [one sort]
         create a bit-vector value one.

         @param sort The sort of the value.

         @return A term representing the bit-vector value 1 of given sort.
      *)
      val one  : bv sort -> t

      (**
         [ones sort]
         create a bit-vector value where all bits are set to 1.

         @param sort The sort of the value.

         @return A term representing the bit-vector value of given sort
         where all bits are set to 1.
      *)
      val ones : bv sort -> t

      (**
         [min_signed sort]
         create a bit-vector minimum signed value.

         @param sort The sort of the value.

         @return A term representing the bit-vector value of given sort
         where the MSB is set to 1 and all remaining bits are set to 0.
      *)
      val min_signed : bv sort -> t

      (**
         [max_signed sort]
         create a bit-vector maximum signed value.

         @param sort The sort of the value.

         @return A term representing the bit-vector value of given sort
         where the MSB is set to 0 and all remaining bits are set to 1.
      *)
      val max_signed : bv sort -> t

      (**
         [of_int sort value]
         create a bit-vector value from its unsigned integer representation.

         If given value does not fit into a bit-vector of given size (sort),
         the value is truncated to fit.

         @param sort The sort of the value.
         @param value The unsigned integer representation of
         the bit-vector value.

         @return A term representing the bit-vector value of given sort.
      *)
      val of_int    : bv sort -> int -> t

      (**
         [of_z sort value]
         create a bit-vector value from its unsigned integer representation.

         If given value does not fit into a bit-vector of given size (sort),
         the value is truncated to fit.

         @param sort The sort of the value.
         @param value The unsigned integer representation of
         the bit-vector value.

         @return A term representing the bit-vector value of given sort.
      *)
      val of_z      : bv sort -> Z.t -> t

      (**
         [of_string sort value]
         create a bit-vector value from its string representation.

         Prefix determine the base of the string representation:
         - [0b]   for binary;
         - [0x]   for hexadecimal;
         - others for decimal.

         Given value must fit into a bit-vector of given size (sort).

         @param sort The sort of the value.
         @param value A string representing the value.

         @return A term representing the bit-vector value of given sort.
      *)
      val of_string : bv sort -> string -> t

      (** The term operator. *)
      type ('a, 'b) operator =
        | Add             : (t -> t -> t, t * t) operator
        (** Bit-vector addition.

            SMT-LIB: [bvadd] *)
        | And             : (t -> t -> t, t * t) operator
        (** Bit-vector and.

            SMT-LIB: [bvand] *)
        | Ashr            : (t -> t -> t, t * t) operator
        (** Bit-vector arithmetic right shift.

            SMT-LIB: [bvashr] *)
        | Concat          : (t -> t -> t, t * t) operator
        (** Bit-vector concat.

            SMT-LIB: [concat] *)
        | Extract         :
            (hi:int -> lo:int -> t -> t, int * int * t) operator
        (** Bit-vector extract.

            SMT-LIB: [extract] (indexed) *)
        | Mul             : (t -> t -> t, t * t) operator
        (** Bit-vector multiplication.

            SMT-LIB: [bvmul] *)
        | Neg             : (t -> t, t) operator
        (** Bit-vector negation (two's complement).

            SMT-LIB: [bvneg] *)
        | Not             : (t -> t, t) operator
        (** Bit-vector not (one's complement).

            SMT-LIB: [bvnot] *)
        | Or              : (t -> t -> t, t * t) operator
        (** Bit-vector or.

            SMT-LIB: [bvor] *)
        | Rol             : (t -> t -> t, t * t) operator
        (** Bit-vector rotate left (not indexed).

            This is a non-indexed variant of SMT-LIB [rotate_left]. *)
        | Ror             : (t -> t -> t, t * t) operator
        (** Bit-vector rotate right.

            This is a non-indexed variant of SMT-LIB [rotate_right]. *)
        | Sdiv            : (t -> t -> t, t * t) operator
        (** Bit-vector signed division.

            SMT-LIB: [bvsdiv] *)
        | Sge             : (t -> t -> t, t * t) operator
        (** Bit-vector signed greater than or equal.

            SMT-LIB: [bvsge] *)
        | Sgt             : (t -> t -> t, t * t) operator
        (** Bit-vector signed greater than.

            SMT-LIB: [bvsgt] *)
        | Shl             : (t -> t -> t, t * t) operator
        (** Bit-vector signed less than.

            SMT-LIB: [bvslt] *)
        | Shr             : (t -> t -> t, t * t) operator
        (** Bit-vector logical right shift.

            SMT-LIB: [bvshr] *)
        | Sle             : (t -> t -> t, t * t) operator
        (** Bit-vector signed less than or equal.

            SMT-LIB: [bvsle] *)
        | Slt             : (t -> t -> t, t * t) operator
        (** Bit-vector signed less than.

            SMT-LIB: [bvslt] *)
        | Smod            : (t -> t -> t, t * t) operator
        (** Bit-vector signed modulo.

            SMT-LIB: [bvsmod] *)
        | Srem            : (t -> t -> t, t * t) operator
        (** Bit-vector signed remainder.

            SMT-LIB: [bvsrem] *)
        | Sub             : (t -> t -> t, t * t) operator
        (** Bit-vector subtraction.

            SMT-LIB: [bvsub] *)
        | Udiv            : (t -> t -> t, t * t) operator
        (** Bit-vector unsigned division.

            SMT-LIB: [bvudiv] *)
        | Uge             : (t -> t -> t, t * t) operator
        (** Bit-vector unsigned greater than or equal.

            SMT-LIB: [bvuge] *)
        | Ugt             : (t -> t -> t, t * t) operator
        (** Bit-vector unsigned greater than.

            SMT-LIB: [bvugt] *)
        | Ule             : (t -> t -> t, t * t) operator
        (** Bit-vector unsigned less than or equal.

            SMT-LIB: [bvule] *)
        | Ult             : (t -> t -> t, t * t) operator
        (** Bit-vector unsigned less than.

            SMT-LIB: [bvult] *)
        | Urem            : (t -> t -> t, t * t) operator
        (** Bit-vector unsigned remainder.

            SMT-LIB: [bvurem] *)
        | Xor             : (t -> t -> t, t * t) operator
        (** Bit-vector xor.

            SMT-LIB: [bvxor] *)

      (**
         [term op ..]
         create a bit-vector term constructor of given kind.

         @param op The operator kind.

         @return A function to build a term representing an operation
                 of given kind.
      *)
      val term : ('a, 'b) operator -> 'a

      (**
         [pred t]
         create a bit-vector decrement.

         @param t The bit-vector term.

         @return Decrement by one.
      *)
      val pred          : t -> t

      (**
         [succ t]
         create a bit-vector increment.

         @param t The bit-vector term.

         @return Increment by one.
      *)
      val succ          : t -> t

      (**
         [neg t]
         create a bit-vector negation.

         @param t The bit-vector term.

         @return SMT-LIB: [bvneg].
      *)
      val neg           : t -> t

      (**
         [add t0 t1]
         create a bit-vector addition.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvadd]
      *)
      val add           : t -> t -> t

      (**
         [sadd_overflow t0 t1]
         create a bit-vector signed addition overflow test.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return Single bit to indicate if signed addition
         produces an overflow
      *)
      val sadd_overflow : t -> t -> t

      (**
         [uadd_overflow t0 t1]
         create a bit-vector unsigned addition overflow test.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return Single bit to indicate if unsigned addition
         produces an overflow
      *)
      val uadd_overflow : t -> t -> t

      (**
         [sub t0 t1]
         create a bit-vector substraction.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvsub]
      *)
      val sub           : t -> t -> t

      (**
         [ssub_overflow t0 t1]
         create a bit-vector signed substraction overflow test.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return Single bit to indicate if signed substraction
         produces an overflow
      *)
      val ssub_overflow : t -> t -> t

      (**
         [usub_overflow t0 t1]
         create a bit-vector ubsigned substraction overflow test.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return Single bit to indicate if unsigned substraction
         produces an overflow
      *)
      val usub_overflow : t -> t -> t

      (**
         [mul t0 t1]
         create a bit-vector multiplication.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvmul]
      *)
      val mul           : t -> t -> t

      (**
         [smul_overflow t0 t1]
         create a bit-vector signed multiplication overflow test.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return Single bit to indicate if signed multiplication
         produces an overflow
      *)
      val smul_overflow : t -> t -> t

      (**
         [umul_overflow t0 t1]
         create a bit-vector unsigned multiplication overflow test.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return Single bit to indicate if unsigned multiplication
         produces an overflow
      *)
      val umul_overflow : t -> t -> t

      (**
         [sdiv t0 t1]
         create a bit-vector signed division.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvsdiv]
      *)
      val sdiv          : t -> t -> t

      (**
         [sdiv_overflow t0 t1]
         create a bit-vector signed division overflow test.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return Single bit to indicate if signed division
         produces an overflow
      *)
      val sdiv_overflow : t -> t -> t

      (**
         [udiv t0 t1]
         create a bit-vector unsigned division.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvudiv]
      *)
      val udiv          : t -> t -> t

      (**
         [smod t0 t1]
         create a bit-vector signed modulo.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvsmod]
      *)
      val smod          : t -> t -> t

      (**
         [srem t0 t1]
         create a bit-vector signed reminder.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvsrem]
      *)
      val srem          : t -> t -> t

      (**
         [urem t0 t1]
         create a bit-vector unsigned reminder.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvurem]
      *)
      val urem          : t -> t -> t

      (**
         [logand t0 t1]
         create a bit-vector and.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvand]
      *)
      val logand  : t -> t -> t

      (**
         [lognand t0 t1]
         create a bit-vector nand.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvnand]
      *)
      val lognand : t -> t -> t

      (**
         [redand t]
         create a bit-vector and reduction.

         @param t The bit-vector term.

         @return Bit-wise {b and} reduction, all bits are {b and}'ed
         together into a single bit.
      *)
      val redand  : t -> t

      (**
         [logor t0 t1]
         create a bit-vector or.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvor]
      *)
      val logor   : t -> t -> t

      (**
         [lognor t0 t1]
         create a bit-vector nor.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvnor]
      *)
      val lognor  : t -> t -> t

      (**
         [redor t]
         create a bit-vector or reduction.

         @param t The bit-vector term.

         @return Bit-wise {b or} reduction, all bits are {b or}'ed
         together into a single bit.
      *)
      val redor   : t -> t

      (**
         [logxor t0 t1]
         create a bit-vector xor.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvxor]
      *)
      val logxor  : t -> t -> t

      (**
         [logxnor t0 t1]
         create a bit-vector xnor.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvxnor]
      *)
      val logxnor : t -> t -> t

      (**
         [redxor t]
         create a bit-vector xor reduction.

         @param t The bit-vector term.

         @return Bit-wise {b xor} reduction, all bits are {b xor}'ed
         together into a single bit.
      *)
      val redxor  : t -> t

      (**
         [lognot t]
         create a bit-vector not (one's complement).

         @param t The first bit-vector term.

         @return SMT-LIB: [bvnot]
      *)
      val lognot  : t -> t

      (**
         [shift_left t0 t1]
         create a bit-vector logical left shift.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB [bvshl]
      *)
      val shift_left          : t -> t -> t

      (**
         [shift_right t0 t1]
         create a bit-vector arithmetic right shift.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB [bvashr]
      *)
      val shift_right         : t -> t -> t

      (**
         [shift_right_logical t0 t1]
         create a bit-vector logical right shift.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB [bvshr]
      *)
      val shift_right_logical : t -> t -> t

      (**
         [rotate_left t0 t1]
         create a bit-vector left rotation.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return non indexed variant of SMT-LIB [rotate_left]
      *)
      val rotate_left         : t -> t -> t

      (**
         [rotate_lefti t n]
         create a bit-vector left rotation.

         @param t The bit-vector term.
         @param n The rotation count.

         @return SMT-LIB: [rotate_left] (indexed)
      *)
      val rotate_lefti        : t -> int -> t

      (**
         [rotate_right t0 t1]
         create a bit-vector right rotation.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return non indexed variant of SMT-LIB [rotate_right]
      *)
      val rotate_right        : t -> t -> t

      (**
         [rotate_righti t n]
         create a bit-vector right rotation.

         @param t The bit-vector term.
         @param n The rotation count.

         @return SMT-LIB: [rotate_right] (indexed)
      *)
      val rotate_righti       : t -> int -> t

      (**
         [zero_extend n t]
         create a bit-vector unsigned extension.

         @param n The number of bits.
         @param t The bit-vector term.

         @return SMT-LIB: [zero_extend] (indexed)
      *)
      val zero_extend : int -> t -> t

      (**
         [sign_extend n t]
         create a bit-vector signed extension.

         @param n The number of bits.
         @param t The bit-vector term.

         @return SMT-LIB: [sign_extend] (indexed)
      *)
      val sign_extend : int -> t -> t

      (**
         [append t0 t1]
         create a bit-vector concat.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [concat]
      *)
      val append      : t -> t -> t

      (**
         [concat ts]
         create a bit-vector {i n}ary concat.

         @param ts The bit-vector terms.

         @return SMT-LIB: [concat]
      *)
      val concat      : t array -> t

      (**
         [repeat n t]
         create a bit-vector repetition.

         @param n The number of repetitions.
         @param t The bit-vector term.

         @return SMT-LIB: [repeat] (indexed)
      *)
      val repeat      : int -> t -> t

      (**
         [extract hi lo t]
         create a bit-vector extract.

         @param hi MSB index.
         @param lo LSB index.
         @param t The bit-vector term.

         @return SMT-LIB: [extract] (indexed)
      *)
      val extract : hi:int -> lo:int -> t -> t

      (**
         [sge t0 t1]
         create a bit-vector signed greater than or equal.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvsge]
      *)
      val sge      : t -> t -> t

      (**
         [uge t0 t1]
         create a bit-vector unsigned greater than or equal.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvuge]
      *)
      val uge      : t -> t -> t

      (**
         [sgt t0 t1]
         create a bit-vector signed greater than.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvsgt]
      *)
      val sgt      : t -> t -> t

      (**
         [ugt t0 t1]
         create a bit-vector unsigned greater than.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvugt]
      *)
      val ugt      : t -> t -> t

      (**
         [sle t0 t1]
         create a bit-vector signed lower than or equal.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvsle]
      *)
      val sle      : t -> t -> t

      (**
         [ule t0 t1]
         create a bit-vector unsigned lower than or equal.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvadd]
      *)
      val ule      : t -> t -> t

      (**
         [slt t0 t1]
         create a bit-vector signed lower than.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvslt]
      *)
      val slt      : t -> t -> t

      (**
         [ult t0 t1]
         create a bit-vector unsigned lower than.

         @param t0 The first bit-vector term.
         @param t1 The second bit-vector term.

         @return SMT-LIB: [bvult]
      *)
      val ult      : t -> t -> t

      (** Same as {!val:Bl.of_bv}. *)
      val to_bl : t -> t

      (**
         [assignment t]
         get bit-vector representation of the current model value of given term.

         @param t The term to query a model value for.

         @return bit-vector representation of current model value of term [t].
      *)
      val assignment : bv value -> Z.t

    end

    (** Rounding-mode *)
    module Rm : sig

      (** A rounding-mode term. *)
      type t = rm term

      (**
         Rounding mode for floating-point operations.

         For some floating-point operations, infinitely precise results
         may not be representable in a given format. Hence, they are rounded
         modulo one of five rounding modes to a representable floating-point
         number.

         The following rounding modes follow the SMT-LIB theory for
         floating-point arithmetic, which in turn is based on
         IEEE Standard 754.
         The rounding modes are specified in Sections 4.3.1 and 4.3.2
         of the IEEE Standard 754.
      *)
      type 'a operator =
        | Rne : rm term operator
        (** Round to the nearest even number.
            If the two nearest floating-point numbers bracketing an
            unrepresentable infinitely precise result are equally near,
            the one with an even least significant digit will be delivered.

            SMT-LIB: [RNE] roundNearestTiesToEven
        *)
        | Rna : rm term operator
        (** Round to the nearest number away from zero.
            If the two nearest floating-point numbers bracketing an
            unrepresentable infinitely precise result are equally near,
            the one with larger magnitude will be selected.

            SMT-LIB: [RNA] roundNearestTiesToAway
        *)
        | Rtn : rm term operator
        (** Round towards negative infinity (-oo).
            The result shall be the format’s floating-point number
            (possibly -oo) closest to and no less than the infinitely
            precise result.

            SMT-LIB: [RTN] roundTowardNegative
        *)
        | Rtp : rm term operator
        (** Round towards positive infinity (+oo).
            The result shall be the format’s floating-point number
            (possibly +oo) closest to and no less than the infinitely
            precise result.

            SMT-LIB: [RTP] roundTowardPositive
        *)
        | Rtz : rm term operator
        (** Round towards zero.
            The result shall be the format’s floating-point number
            closest to and no greater in magnitude than the infinitely
            precise result.

            SMT-LIB: [RTZ] roundTowardZero
        *)

      (**
         [term op]
         create a rounding-mode term of given kind.

         @param op The operator kind.

         @return A term representing an operation of given kind.
      *)
      val term : 'a operator -> 'a

      (** Same as {!val:term} {!constructor:Rne} *)
      val rne : t

      (** Same as {!val:term} {!constructor:Rna} *)
      val rna : t

      (** Same as {!val:term} {!constructor:Rtn} *)
      val rtn : t

      (** Same as {!val:term} {!constructor:Rtp} *)
      val rtp : t

      (** Same as {!val:term} {!constructor:Rtz} *)
      val rtz : t

    end

    (** Floating-point *)
    module Fp : sig

      (** A floating-point term. *)
      type t = fp term

      (**
         [pos_zero sort]
         create a floating-point positive zero value (SMT-LIB: [+zero]).

         @param sort The sort of the value.

         @return A term representing the floating-point positive zero value
         of given floating-point sort.
      *)
      val pos_zero : fp sort -> t

      (**
         [neg_zero sort]
         create a floating-point negative zero value (SMT-LIB: [-zero]).

         @param sort The sort of the value.

         @return A term representing the floating-point negative zero value
         of given floating-point sort.
      *)
      val neg_zero : fp sort -> t

      (**
         [pos_inf sort]
         create a floating-point positive infinity value (SMT-LIB: [+oo]).

         @param sort The sort of the value.

         @return A term representing the floating-point positive infinity value
         of given floating-point sort.
      *)
      val pos_inf  : fp sort -> t

      (**
         [neg_inf sort]
         create a floating-point negative infinity value (SMT-LIB: [-oo]).

         @param sort The sort of the value.

         @return A term representing the floating-point negative infinity value
         of given floating-point sort.
      *)
      val neg_inf  : fp sort -> t

      (**
         [nan sort]
         create a floating-point NaN value.

         @param sort The sort of the value.

         @return A term representing the floating-point NaN value of given
         floating-point sort.
      *)
      val nan      : fp sort -> t

      (**
         [of_float sort rm value]
         create a floating-point value from its float representation,
         with respect to given rounding mode.

         @param sort The sort of the value.
         @param rm The rounding mode.
         @param value The floating-point value.

         @return A term representing the floating-point value of given sort.
      *)
      val of_float    : fp sort -> rm term Rm.operator -> float -> t

      (**
         [of_real sort rm real]
         create a floating-point value from its real representation, given as a
         decimal string, with respect to given rounding mode.

         @param sort The sort of the value.
         @param rm The rounding mode.
         @param real The decimal string representing a real value.

         @return A term representing the floating-point value of given sort.
      *)
      val of_real     : fp sort -> rm term Rm.operator -> string -> t

      (**
         [from_rational sort rm num den]
         create a floating-point value from its rational representation,
         given as a two decimal strings representing the numerator and
         denominator, with respect to given rounding mode.

         @param sort The sort of the value.
         @param rm The rounding mode.
         @param num The decimal string representing the numerator.
         @param den The decimal string representing the denominator.

         @return A term representing the floating-point value of given sort.
      *)
      val of_rational : fp sort -> rm term Rm.operator ->
        num:string -> den:string -> t

      (** Floating-point IEEE 754 representation. *)
      type ieee_754 = private
        { sign: bv term; exponent: bv term; significand: bv term }

      (** The term operator. *)
      type ('a, 'b, 'c) operator =
        | Abs          : (t -> t, t, fp) operator
        (** Floating-point absolute value.

            SMT-LIB: [fp.abs] *)
        | Add          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        (** Floating-point addition.

            SMT-LIB: [fp.add] *)
        | Div          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        (** Floating-point division.

            SMT-LIB: [fp.div] *)
        | Eq           : (t -> t -> bv term, t * t, bv) operator
        (** Floating-point equality.

            SMT-LIB: [fp.eq] *)
        | Fma          :
            (rm term -> t -> t -> t -> t, rm term * t * t * t, fp) operator
        (** Floating-point fused multiplcation and addition.

            SMT-LIB: [fp.fma] *)
        | Fp           :
            (sign:bv term -> exponent:bv term -> bv term -> t,
             ieee_754, fp) operator
        (** Floating-point IEEE 754 value.

            SMT-LIB: [fp] *)
        | Geq          : (t -> t -> bv term, t * t, bv) operator
        (** Floating-point greater than or equal.

            SMT-LIB: [fp.geq] *)
        | Gt           : (t -> t -> bv term, t * t, bv) operator
        (** Floating-point greater than.

            SMT-LIB: [fp.gt] *)
        | Is_inf       : (t -> bv term, t, bv) operator
        (** Floating-point is infinity tester.

            SMT-LIB: [fp.isInfinite] *)
        | Is_nan       : (t -> bv term, t, bv) operator
        (** Floating-point is Nan tester.

            SMT-LIB: [fp.isNaN] *)
        | Is_neg       : (t -> bv term, t, bv) operator
        (** Floating-point is negative tester.

            SMT-LIB: [fp.isNegative] *)
        | Is_normal    : (t -> bv term, t, bv) operator
        (** Floating-point is normal tester.

            SMT-LIB: [fp.isNormal] *)
        | Is_pos       : (t -> bv term, t, bv) operator
        (** Floating-point is positive tester.

            SMT-LIB: [fp.isPositive] *)
        | Is_subnormal : (t -> bv term, t, bv) operator
        (** Floating-point is subnormal tester.

            SMT-LIB: [fp.isSubnormal] *)
        | Is_zero      : (t -> bv term, t, bv) operator
        (** Floating-point is zero tester.

            SMT-LIB: [fp.isZero] *)
        | Leq          : (t -> t -> bv term, t * t, bv) operator
        (** Floating-point less than or equal.

            SMT-LIB: [fp.leq] *)
        | Lt           : (t -> t -> bv term, t * t, bv) operator
        (** Floating-point less than.

            SMT-LIB: [fp.lt] *)
        | Max          : (t -> t -> t, t * t, fp) operator
        (** Floating-point max.

            SMT-LIB: [fp.max] *)
        | Min          : (t -> t -> t, t * t, fp) operator
        (** Floating-point min.

            SMT-LIB: [fp.min] *)
        | Mul          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        (** Floating-point multiplcation.

            SMT-LIB: [fp.mul] *)
        | Neg          : (t -> t, t, fp) operator
        (** Floating-point negation.

            SMT-LIB: [fp.neg] *)
        | Rem          : (t -> t -> t, t * t, fp) operator
        (** Floating-point remainder.

            SMT-LIB: [fp.rem] *)
        | Rti          : (rm term -> t -> t, rm term * t, fp) operator
        (** Floating-point round to integral.

            SMT-LIB: [fp.roundToIntegral] *)
        | Sqrt         : (rm term -> t -> t, rm term * t, fp) operator
        (** Floating-point round to square root.

            SMT-LIB: [fp.sqrt] *)
        | Sub          : (rm term -> t -> t -> t, rm term * t * t, fp) operator
        (** Floating-point round to subtraction.

            SMT-LIB: [fp.sqrt] *)
        | From_bv      : (exponent:int -> int -> bv term -> t,
                          int * int * bv term, fp) operator
        (** Floating-point to_fp from IEEE 754 bit-vector.

            SMT-LIB: [to_fp] (indexed) *)
        | From_fp      :
            (exponent:int -> int -> rm term -> t -> t,
             int * int * rm term * t, fp) operator
        (** Floating-point to_fp from floating-point.

            SMT-LIB: [to_fp] (indexed) *)
        | From_sbv     :
            (exponent:int -> int -> rm term -> bv term -> t,
             int * int * rm term * bv term, fp) operator
        (** Floating-point to_fp from signed bit-vector value.

            SMT-LIB: [to_fp] (indexed) *)
        | From_ubv     :
            (exponent:int -> int -> rm term -> bv term -> t,
             int * int * rm term * bv term, fp) operator
        (** Floating-point to_fp from unsigned bit-vector value.

            SMT-LIB: [to_fp_unsigned] (indexed) *)
        | To_sbv       : (int -> rm term -> t -> bv term,
                          int * rm term * t, bv) operator
        (** Floating-point to_sbv.

            SMT-LIB: [fp.to_sbv] (indexed) *)
        | To_ubv       : (int -> rm term -> t -> bv term,
                          int * rm term * t, bv) operator
        (** Floating-point to_ubv.

            SMT-LIB: [fp.to_ubv] (indexed) *)

      (**
         [term op ..]
         create a floating-point term constructor of given kind.

         @param op The operator kind.

         @return A function to build a term representing an operation
                 of given kind.
      *)
      val term : ('a, 'b, 'c) operator -> 'a

      (**
         [make ~sign ~exponent significand]
         create a floating-point value from its IEEE 754 standard
         representation given as three bitvectors representing the sign bit,
         the exponent and the significand.

         @param sign The sign bit.
         @param exponent The exponent bit-vector.
         @param significand The significand bit-vector.

         @return A term representing the floating-point value.
      *)
      val make   : sign:bv term -> exponent:bv term -> bv term -> t

      (**
         [of_sbv ~exponent size rm t]
         create a floatingt-point to_fp from signed bit-vector value.

         @param exponent The size of the exponent.
         @param size The total size of the floating-point.
         @param rm The rounding-mode.
         @param t The bit-vector term.

         @return SMT-LIB: [to_fp] (indexed)
      *)
      val of_sbv : exponent:int -> int -> rm term -> bv term -> t

      (**
         [of_ubv ~exponent size rm t]
         create a floatingt-point to_fp from unsigned bit-vector value.

         @param exponent The size of the exponent.
         @param size The total size of the floating-point.
         @param rm The rounding-mode.
         @param t The bit-vector term.

         @return SMT-LIB: [to_fp] (indexed)
      *)
      val of_ubv : exponent:int -> int -> rm term -> bv term -> t

      (**
         [of_bv ~exponent size rm t]
         create a floatingt-point to_fp from IEEE 754 bit-vector.

         @param exponent The size of the exponent.
         @param size The total size of the floating-point.
         @param rm The rounding-mode.
         @param t The bit-vector term.

         @return SMT-LIB: [to_fp] (indexed)
      *)
      val of_bv  : exponent:int -> int -> bv term -> t

      (**
         [of_fp ~exponent size rm t]
         create a floatingt-point to_fp from floating-point.

         @param exponent The size of the exponent.
         @param size The total size of the floating-point.
         @param rm The rounding-mode.
         @param t The floating-point term.

         @return SMT-LIB: [to_fp] (indexed)
      *)
      val of_fp  : exponent:int -> int -> rm term -> fp term -> t

      (**
         [abs t]
         create a floatingt-point absolute value.

         @param t The floating-point term.

         @return SMT-LIB: [fp.abs]
      *)
      val abs  : t -> t

      (**
         [neg t]
         create a floatingt-point negation.

         @param t The floating-point term.

         @return SMT-LIB: [fp.neg]
      *)
      val neg  : t -> t

      (**
         [add rm t0 t1]
         create a floating-point addition.

         @param rm The roundint-mode.
         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.add]
      *)
      val add  : rm term -> t -> t -> t

      (**
         [sub rm t0 t1]
         create a floating-point substraction.

         @param rm The roundint-mode.
         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.sub]
      *)
      val sub  : rm term -> t -> t -> t

      (**
         [mul rm t0 t1]
         create a floating-point multiplication.

         @param rm The roundint-mode.
         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.mul]
      *)
      val mul  : rm term -> t -> t -> t

      (**
         [div rm t0 t1]
         create a floating-point division.

         @param rm The roundint-mode.
         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.div]
      *)
      val div  : rm term -> t -> t -> t

      (**
         [fma rm t0 t1 t2]
         create a floating-point fused multiplication and addition.

         @param rm The roundint-mode.
         @param t0 The first floating-point term.
         @param t1 The second floating-point term.
         @param t2 The third floating-point term.

         @return SMT-LIB: [fp.fma]
      *)
      val fma  : rm term -> t -> t -> t -> t


      (**
         [sqrt rm t]
         create a floating-point round to square root.

         @param rm The roundint-mode.
         @param t0 The floating-point term.

         @return SMT-LIB: [fp.sqrt]
      *)
      val sqrt : rm term -> t -> t

      (**
         [rem t0 t1]
         create a floating-point remainder.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.rem]
      *)
      val rem  : t -> t -> t

      (**
         [rti rm t]
         create a floating-point round to integral.

         @param rm The roundint-mode.
         @param t0 The floating-point term.

         @return SMT-LIB: [fp.roundToIntegral]
      *)
      val rti  : rm term -> t -> t


      (**
         [min t0 t1]
         create a floating-point min.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.min]
      *)
      val min  : t -> t -> t

      (**
         [max t0 t1]
         create a floating-point max.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.max]
      *)
      val max  : t -> t -> t

      (**
         [leq t0 t1]
         create a floating-point less than or equal.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.leq]
      *)
      val leq  : t -> t -> bv term

      (**
         [lt t0 t1]
         create a floating-point less than.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.lt]
      *)
      val lt   : t -> t -> bv term

      (**
         [geq t0 t1]
         create a floating-point greater than or equal.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.geq]
      *)
      val geq  : t -> t -> bv term

      (**
         [gt t0 t1]
         create a floating-point greater than.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.gt]
      *)
      val gt   : t -> t -> bv term

      (**
         [eq t0 t1]
         create a floating-point equality.

         @param t0 The first floating-point term.
         @param t1 The second floating-point term.

         @return SMT-LIB: [fp.eq]
      *)
      val eq   : t -> t -> bv term

      (**
         [is_normal t]
         create a floatingt-point is normal tester.

         @param t The floating-point term.

         @return SMT-LIB: [fp.isNormal]
      *)
      val is_normal    : t -> bv term

      (**
         [is_subnormal t]
         create a floatingt-point is subnormal tester.

         @param t The floating-point term.

         @return SMT-LIB: [fp.isSubnormal]
      *)
      val is_subnormal : t -> bv term

      (**
         [is_zero t]
         create a floatingt-point is zero tester.

         @param t The floating-point term.

         @return SMT-LIB: [fp.isZero]
      *)
      val is_zero      : t -> bv term

      (**
         [is_infinite t]
         create a floatingt-point is infinite tester.

         @param t The floating-point term.

         @return SMT-LIB: [fp.isInfinite]
      *)
      val is_infinite  : t -> bv term

      (**
         [is_nan t]
         create a floatingt-point is Nan tester.

         @param t The floating-point term.

         @return SMT-LIB: [fp.isNan]
      *)
      val is_nan       : t -> bv term

      (**
         [is_negative t]
         create a floatingt-point is negative tester.

         @param t The floating-point term.

         @return SMT-LIB: [fp.isNegative]
      *)
      val is_negative  : t -> bv term

      (**
         [is_positive t]
         create a floatingt-point is positive tester.

         @param t The floating-point term.

         @return SMT-LIB: [fp.isPositive]
      *)
      val is_positive  : t -> bv term

      (**
         [to_sbv t]
         create a floatingt-point is to_sbv term.

         @param t The floating-point term.

         @return SMT-LIB: [fp.to_sbv] (indexed)
      *)
      val to_sbv : int -> rm term -> t -> bv term

      (**
         [to_ubv t]
         create a floatingt-point is to_ubv term.

         @param t The floating-point term.

         @return SMT-LIB: [fp.to_ubv] (indexed)
      *)
      val to_ubv : int -> rm term -> t -> bv term

      (**
         [assignement t ]
         get floatint-point representation of the current model value of
         given floating-point term.

         @param term The term to query a model value for.

         @return Floating-point representations of the given term.
      *)
      val assignment : rm term Rm.operator -> fp value -> float

    end

    (** Array *)
    module Ar : sig

      (** An array term which maps ['a] to ['b]. *)
      type ('a, 'b) t = ('a, 'b) ar term

      (**
         [make sort value]
         create a one-dimensional constant array of given sort,
         initialized with given value.

         @param sort The sort of the array.
         @param value The value to initialize the elements of the array with.

         @return A term representing a constant array of given sort.
      *)
      val make : ('a, 'b) ar sort -> 'b term -> ('a, 'b) t

      (**
         [select t i]
         create an array access.

         @param t The array term.
         @param i The index term.

         @return SMT-LIB: [select]
      *)
      val select : ('a, 'b) t -> 'a term -> 'b term

      (**
         [store t i e]
         create an array write.

         @param t The array term.
         @param i The index term.
         @param e The element term.

         @return SMT-LIB: [store]
      *)
      val store  : ('a, 'b) t -> 'a term -> 'b term -> ('a, 'b) t

      (**
         [assignment t]
         get the current model value of given array term.

         The value of indices and values can be queried via
         {!val:Bv.assignment} and {!val:Fp.assignment}.

         @param t The term to query a model value for.

         @return An array of associations between indices and values.
           The value of all other indices is [Some default] when
           base array is constant array, otherwise, it is [None].
      *)
      val assignment : ('a, 'b) ar value ->
        ('a value * 'b value) array * 'b value option

    end

    (** Uninterpreted function *)
    module Uf : sig

      (** A function term which maps ['a] to ['b]. *)
      type ('a, 'b) t = ('a, 'b) fn term

      (**
         [lambda sorts f]
         create a function definition.

         @param sorts The argument sorts.
         @param f A function that take the function formal parameters and
         return a term.

         @return a function definition
      *)
      val lambda : 'a Sort.variadic -> ('a variadic -> 'b term) -> ('a, 'b) t

      (**
         [apply t args]
         create a function application.

         @param t The function term.
         @param args The argument terms.

         @return a function application
      *)
      val apply  : ('a, 'b) t -> 'a variadic -> 'b term

      (** Statically typed list of function argument values. *)
      type 'a variadic =
      |  []  : unit variadic
      | (::) : ([< bv | rm | fp ] as 'a) value * 'b variadic ->
          ('a -> 'b) variadic

      (**
         [assignment t]
         get the current model value of given function term.

         The value of arguments and values can be queried via
         {!val:Bv.assignment} and {!val:Fp.assignment}.

         @param t The term to query a model value for.

         @return An array of associations between `arity` arguments and a value.
      *)
      val assignment : ('a, 'b) fn value -> ('a variadic * 'b value) array

    end

    (**
       [const sort symbol]
       create a (first-order) constant of given sort with given symbol.

       This creates a 0-arity function symbol.

       @param sort The sort of the constant.
       @param symbol The symbol of the constant.

       @return A term representing the constant.
    *)
    val const      : 'a sort -> string -> 'a term

    (**
       [equal t0 t1]
       create an equality term.

       @param t0 The first term.
       @param t1 The second term.

       @return SMT-LIB: [=]
    *)
    val equal    : 'a t -> 'a t -> bv t

    (**
       [distinct t0 t1]
       create an disequality term.

       @param t0 The first term.
       @param t1 The second term.

       @return SMT-LIB: [not (= t0 t1)]
    *)
    val distinct : 'a t -> 'a t -> bv t

    (**
       [ite t0 t1 t2]
       create an if-then-else term.

       @param t0 The condition term.
       @param t1 The {i then} term.
       @param t2 The {i else} term.

       @return SMT-LIB: [ite]
    *)
    val ite : bv t -> 'a t -> 'a t -> 'a t

    (**
       [hash t]
       compute the hash value for a term.

       @param t The term.

       @return The hash value of the term.
    *)
    val hash : 'a t -> int

    (**
       [sort t]
       get the sort of a term.

       @param t The term.

       @return The sort of the term.
    *)
    val sort : 'a t -> 'a sort

    (**
       [pp formatter t]
       pretty print term.

       @param formatter The outpout formatter
       @param t The term.
    *)
    val pp : Format.formatter -> 'a term -> unit

    (** {1 View} *)

    (** Algebraic view of formula terms. *)
    type 'a view =
      | Value    : 'a value -> 'a view
      | Const    : 'a sort * string -> 'a view
      | Var      :  ([< bv | rm | fp ] as 'a) sort -> 'a view
      | Lambda   : 'a variadic * 'b term -> ('a, 'b) fn view
      | Equal    :
          ([< bv | rm | fp | ('b, 'c) ar ] as 'a) term * 'a term -> bv view
      | Distinct :
          ([< bv | rm | fp | ('b, 'c) ar ] as 'a) term * 'a term -> bv view
      | Ite      :
          bv term * ([< bv | rm | fp | ('b, 'c) ar ] as 'a) term * 'a term
          -> 'a view
      | Bv       : ('a, 'b) Bv.operator * 'b -> bv view
      | Fp       : ('a, 'b, 'c) Fp.operator * 'b -> 'c view
      | Select   : ('a, 'b) ar term * 'a term -> 'b view
      | Store    : ('a, 'b) ar term * 'a term * 'b term -> ('a, 'b) ar view
      | Apply    : ('a, 'b) fn term * 'a variadic -> 'b view

    (**
       [view t]
       destructurate a term.

       @param t The term.

       @return The view of the term and its children.
    *)
    val view : 'a term -> 'a view

  end

  (** {1 Formula} *)

  (**
     [assert' ~name t]
     assert that the condition [t] is [true].

     [t] must be a boolean term ({!type:bv} {!type:term} of size 1).

     @param name The name of the assertion, if any.
     @param t The formula term to assert.
  *)
  val assert' : ?name: string -> bv term -> unit

  (** A satisfiability result. *)
  type result =
    | Sat      (** sat *)
    | Unsat    (** unsat *)
    | Unknown  (** unknown *)

  (**
     [pp formatter result]
     pretty print result.

     @param formatter The output formatter.
     @param result The result to print.
  *)
  val pp_result : Format.formatter -> result -> unit

  (**
     [check_sat ~interrupt ()]
     check satisfiability of current input formula.

     @param interrupt Stop the research and return {!constructor:Unknown}
     if the callback function returns a positive value.
     Run into completion otherwise.

     @return {!constructor:Sat} if the input formula is satisfiable and
     {!constructor:Unsat} if it is unsatisfiable, and {!constructor:Unknown}
     when neither satistifiability nor unsatisfiability was determined,
     for instance when it was terminated by {!val:timeout}.
  *)
  val check_sat : ?interrupt:(('a -> int) * 'a) -> unit -> result

  (**
     [timeout t f]
     configure the interruptible function [f] with a timeout of [t] seconds.

     [timeout] can be used to limit the time spend on
     {!val:check_sat} or {!val:check_sat_assuming}.
     For instance, for a 1 second time credit, use:
     - [(timeout 1. check_sat) ()]
     - [(timeout 1. check_sat_assuming) assumptions]

     @param t Timeout in second.
     @param f The function to configure.

     @return An interruptible function configured to stop on timeout.
  *)
  val timeout : float -> (?interrupt:((float -> int) * float) -> 'b) -> 'b

  (**
     [get_value t]
     get a term representing the model value of a given term.

     Requires that the last {!val:check_sat} query returned {!constructor:Sat}.

     @param t The term to query a model value for.

     @return A term representing the model value of term [t].
  *)
  val get_value : 'a term -> 'a value

  (**
     [unafe_close ()]
     close the session.

     UNSAFE: call this ONLY to release the resources earlier
     if the session is about to be garbage collected.
  *)
  val unsafe_close : unit -> unit

end

(** Create a new Bitwuzla session in incremental mode. *)
module Incremental () : sig

  include module type of Once ()

  (** {1 Formula} *)

  (**
     [push nlevels]
     push context levels.

     @param nlevels The number of context levels to push.
  *)
  val push : int -> unit


  (**
     [pop nlevels]
     pop context levels.

     @param nlevels The number of context levels to pop.
  *)
  val pop  : int -> unit

  (**
     [check_sat_assuming ~interrupt ~names assumptions]
     check satisfiability of current input formula, with the search for
     a solution guided by the given assumptions.

     An input formula consists of assertions added via {!val:assert'}
     combined with assumptions via Boolean [and].
     Unsatifiable assumptions can be queried via {!val:get_unsat_assumptions}.

     @param interrupt Stop the research and return {!constructor:Unknown}
     if the callback function returns a positive value.
     Run into completion otherwise.
     @param names The assumption names, if any.
     @param assumption The set of assumptions guiding the research of solutions.

     @return {!constructor:Sat} if the input formula is satisfiable and
     {!constructor:Unsat} if it is unsatisfiable, and {!constructor:Unknown}
     when neither satistifiability nor unsatisfiability was determined,
     for instance when it was terminated by {!val:timeout}.
  *)
  val check_sat_assuming :
    ?interrupt:(('a -> int) * 'a) -> ?names: string array -> bv term array ->
    result

  (**
     [get_unsat_assumptions ()]
     get the set of unsat assumptions.

     Unsat assumptions are assumptions that force an input formula to become
     unsatisfiable. Unsat assumptions handling in Bitwuzla is analogous to
     failed assumptions in MiniSAT.

     Requires that the last {!val:check_sat} query returned
     {!constructor:Unsat}.

     @return An array with unsat assumptions.
  *)
  val get_unsat_assumptions : unit -> bv term array

end

(** Create a new Bitwuzla session in incremental mode while enabling
    unsatifiable core generation. *)
module Unsat_core () : sig

  include module type of Incremental ()

  (**
     [get_unsat_core ()]
     get the set unsat core (unsat assertions).

     The unsat core consists of the set of assertions that force an
     input formula to become unsatisfiable.

     Requires that the last {!val:check_sat} query returned
     {!constructor:Unsat}.

     @return An array with unsat assertions.
  *)
  val get_unsat_core : unit -> bv term array

end
