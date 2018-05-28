(* File: fftw3.mli

   Copyright (C) 2008-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: https://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Interface for FFTW version 3.

   @author Christophe Troestler <Christophe.Troestler\@umons.ac.be>
   @version 0.8.1
*)
(** We advise against opening this module as it contains submodules with
    the same names as the [Bigarray] ones.  Instead, declare
    {[module FFT = Fftw3.D 						]}
    or
    {[module FFT = Fftw3.S 						]}
    depending to the precision you need (this way of proceeding makes it
    easy to change the precision of the FFT sould it be necessary) and
    then use it as
    {[
      let input  = FFT.Array1.create FFT.complex Bigarray.c_layout dim in
      let output = FFT.Array1.create FFT.complex Bigarray.c_layout dim in
      let dft    = FFT.Array1.dft FFT.Forward input output in
      (* code to initialize input and output arrays here *)
      FFT.exec dft (* compute the DFT *)
    ]}
    The plan creation function will raise [FFT.Failure] in case of problems.
    The last line can be repeated as many times as needed to compute the
    FFT of [input] into [output].
    {b Beware} that creating the plan ([FFT.Array1.dft])
    usually destroys the content of [input] and [output], so only fill them
    afterwards.

    HINT: Plan creation functions like {!Fftw3.Sig.Array1.dft} have
    many optional arguments for maximum flexibility.  The
    important one is [~meas].  The other ones can be ignored at first.
*)


(** Precision independent signature for FFTW3 submodules. *)
module type Sig = sig
  open Bigarray

  (** {2 Precision} *)

  type float_elt (** Precision of float numbers. *)

  type complex_elt (** Precision of complex numbers. *)

  val float : (float, float_elt) Bigarray.kind
    (** Float of the precision of this module.  Use this to create
        precision independent code. *)

  val complex : (Complex.t, complex_elt) Bigarray.kind
    (** Complex of the precision of this module.  Use this to create
        precision independent code. *)


  (** {2 Specifying plans} *)

  type 'a plan (** FFTW plan. *)

  type c2c     (** [c2c plan] usual discrete Fourier transform,
                   from complex to complex *)

  type r2c     (** [r2c plan] real to complex transform *)

  type c2r     (** [c2r plan] complex to real transform *)

  type r2r     (** [r2r plan] real to real transform *)

  (** Direction of the transform — see the FFTW manual. *)
  type dir = Forward | Backward

  (** Planning-rigor flags. *)
  type measure =
    | Estimate (** No measurements are made, use a simple heuristic to
                   pick a (probably sub-optimal) plan quickly. *)
    | Measure (** Find an optimized plan by actually computing several
                  FFTs and measuring their execution time. *)
    | Patient (** Like [Measure], but considers a wider range of
                  algorithms and often produces a "more optimal" plan
                  at the expense of several times longer planning
                  time. *)
    | Exhaustive (** Like [Patient], but considers an even wider range
                     of algorithms, including many that are thought
                     unlikely to be fast, to produce the most optimal
                     plan but with a substantially increased planning
                     time. *)

  (** Real-to-Real transform kinds.  The real-even (resp. real-odd) DFT
      are somtimes called Discrete Cosine Transform (DCT)
      (resp. Discrete Sine Transform (DST)).  Note that the explanations
      of the various transforms are for an {i input} array of dimension
      [n] and C layout (i.e. the input array is [input[0..n-1]]).  The
      logical size [N] is [N=2(n-1)] for [REDFT00], [N=2(n+1)] for
      [RODFT00], and [N=2n] otherwise.  See the FFTW manual for more
      details. *)
  type r2r_kind =
    | R2HC (** real to halfcomplex *)
    | HC2R (** halfcomplex to real *)
    | DHT  (** discrete Hartley Transform *)
    | REDFT00 (** real-even DFT: even around j=0 and even around j=n-1 *)
    | REDFT01 (** real-even DFT: even around j=0 and odd around j=n *)
    | REDFT10 (** real-even DFT: even around j=-0.5 and even around j=n-0.5 *)
    | REDFT11 (** real-even DFT: even around j=-0.5 and odd around j=n-0.5 *)
    | RODFT00 (** real-odd DFT; odd around j=-1 and odd around j=n *)
    | RODFT01 (** real-odd DFT; odd around j=-1 and even around j=n-1 *)
    | RODFT10 (** real-odd DFT; odd around j=-0.5 and odd around j=n-0.5 *)
    | RODFT11 (** real-odd DFT; odd around j=-0.5 and even around j=n-0.5 *)


  exception Failure of string
    (** Exception raised to indicate that a plan could not be
        created. *)

  (** {2 Executing plans} *)

  val exec : 'a plan -> unit
    (** [exec plan] executes the [plan] on the arrays given at the
        creation of this plan.  This is the normal way to execute any
        kind of plan.

        This function is thread safe (and may run the actual
        computation on a different core than the main program).  *)

  (** Guru execution of plans.

      If you want to transform other arrays than those specified in the
      plan, you are advised to create a new plan — it won't be too
      expensive if the wisdom can be reused.  To transform a known bunch
      of arrays of the same size, you should {b not} use the following
      functions but instead create a plan with [?howmany] set
      appropriately.

      These functions are thread safe.  You can even execute the {i same
      plan} in parallel by multiple threads by providing different
      arrays than the ones with which the plan was created.
  *)
  module Guru : sig
    (*
      val dft : c2c plan -> 'l complex_array -> 'l complex_array -> unit

      val split_dft : c2c plan ->
      'l float_array -> 'l float_array ->
      'l float_array -> 'l float_array -> unit

      val r2c : r2c plan -> 'l float_array -> 'l complex_array -> unit

      val split_r2c : r2c plan ->
      'l float_array -> 'l float_array -> 'l float_array -> unit

      val c2r : c2r plan -> 'l complex_array -> 'l float_array -> unit

      val split_c2r : c2r plan ->
      'l float_array -> 'l float_array -> 'l float_array -> unit

      val r2r : r2r plan -> 'l float_array -> 'l float_array -> unit
    *)
  end


  (** {2 Creating plans} *)

  (** FFT of Bigarray.Genarray. *)
  module Genarray :
  sig
    external create: ('a, 'b) kind -> 'c layout -> int array
      -> ('a, 'b, 'c) Bigarray.Genarray.t = "fftw3_ocaml_ba_create"
      (** Creates a new array, just as [Bigarray.Genarray.create] does,
	  but guarantees that it is aligned so one gets the better
	  performance from FFTW.

          Remark: In order to deserialize such a bigarray, this module
          must be linked to the program as the deserialization
          function also aligns the data. *)

    type 'l complex_array = (Complex.t, complex_elt, 'l) Bigarray.Genarray.t
        (** Double precision complex array. *)

    type 'l float_array   = (float, float_elt, 'l) Bigarray.Genarray.t
        (** Double precision float array. *)

    type coord = int array
        (** Coordinates of elements or dimensions of an ND array
            (therefore the length of such an array of coordinates must
            be equal to the number of dimensions of the matrix). *)

    val dft :
      dir ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> c2c plan
      (** [dft dir i o] returns a plan for computing the FFT in the
	  direction [dir] from [i] to [o].  [i] and [o] must have the
	  same number of (logical) dimensions and may be equal.  If
	  [i], [ofsi] and [o], [ofso] are respectively the same, the
	  transform is done in-place.  If not, the sub-matrices should
	  not overlap.  Raises {!Fftw3.Sig.Failure} if the plan cannot be
	  created.

          Note that FFTW computes an unnormalized DFT: computing a
          forward followed by a backward transform (or vice versa)
          results in the original array scaled by N, the product of
          the lofical dimensions [Array.fold_left ( * ) 1 ni
          = Array.fold_left ( * ) 1 no].

	  - [meas] controls how much time is dedicated to the creation
	  of the plan.  Default: [Measure].  {b Beware} that, unless
	  [~meas] is [Estimate], creating a plan requires some trials
	  that will destroy the content of the arrays.

	  - [destroy_input] specifies that an out-of-place transform
	  may {i overwrite its input} array.  Overwriting input may
	  sometimes allow more efficient algorithms to be employed.
	  Default: [false] (i.e. perserve the content of the input
	  array) except for c2r and HC2R.

          - [unaligned] specifies that the algorithm may not impose
          any alignment requirements.  You normally do not need this
          flag unless you want to use the plan with {i other unaligned
          arrays} (using the guru interface).  Default: [false]
          meaning that alignment may be used to speed up the
          computations (when [in] and [out] are aligned of course).

          {5 Subarrays}

          Fftw3 allows you to perform the FFT transform on subarrays
          defined by offset, strides and dimensions.  (Only the offset
          specification is dependent on the layout, the other two are
          the same regardless of whether the matrix has a C or FORTRAN
          layout.)

          - [ni] is the array with an entry for each dimension [k] of
          [i].  [ni.(k)] indicates how many increments [inci.(k)] we
          want to consider in the dimension [k].  Of course, the
          [ni.(k)] must be small enough so that the the subarrays fits
          in [i], i.e., for all [k], [ofsi.(k) + (ni.(k) - 1)
          abs(inci.(k))] must be [< dim i k] (c_layout) or [<= dim i k]
          (fortran_layout).  If [ni.(k) = 0], it means that we want
          the larger dimension [ni.(k)] that the choice of [ofsi.(k)]
          and [inci.(k)] allow.  In this case, [ni.(k)] will be {i
          overwritten} with the dimension that was automatically
          determined.  Note that [ni.(k) = 1] means that the direction
          [k] is to be ignored (i.e. the [k]th index is constant with
          value [ofsi.(k)]).

          - [ofsi] the initial element in the input array.  Default:
          [[|0;...;0|]] for c_layout and [[|1;...;1|]] for fortran_layout.

          - [inci] an array of increments for each (physical)
          dimension of the input array [i].  [inci.(k)] can be
          negative, indicating that the range [ofsi.(k)] .. [ofsi.(k) +
          (ni.(k) - 1) abs(inc.(k))] is traversed backward.  This is
          the same behavior is as lacaml (LAPACK).  If the increment
          [inci.(k) = 0], that means that the dimension [k] must be
          ignored (i.e. the index in dimension [k] is constant with
          value [ofsi.(k)]).  Default: [[|1;...;1|]].

          - [no] same as [ni] but for output.  [no] must denote a
          transform of the same dimensions as [ni] i.e., neglecting
          the dimensions [1], the two matrices must be the same.

          - [ofso] same as [ofsi] but for output.

          - [inco] same as [inci] but for output.

          For example, if one wants the submatrix indicated by the
          stars of the following (C layout) matrix:
          {v
            a = [[x x x x x x     one sets:  ofs = [|1; 1|]
                  x * x * x x                inc = [|1; 2|]
 	          x * x * x x	             dim = [|2; 2|]
	          x x x x x x ]]
          v}
          The slice represented by the stars
          {v
            a = [[x * x x x
                  x * x x x
                  x * x x x ]]
          v}
          is defined by [ofs = [|0; 1|]] and [inc = [|1; 0|]]

          {5:many Multiple transforms}

          FFTW allows to compute several transforms at once by
          specifying submatrices of [i] and [o].  This is more
          efficient than to create a different plan for each
          transform.  It is your responsability to ensure that the
          many submatrices do not overlap.

	  - [howmany_n] is an array of the (logical) dimensions of the
          array indexing the many transforms.  Default: [[| |]],
          i.e. only a single transform is performed.  If [howmanyi] is
          given but no [howmany_n], then the maximum dimensions
          possible by the dimensions of [i] (resp. [o]) are used.  A
          value of [0] for a dimension also means to make it as large
          as possible.

          - [howmanyi] is a list of vectors [[v1;...;vp]] generating
          the lattice of multiple arrays.  In other words, if [a] is
          an element of (vector) index [k] in the "first" array, then
          the same element in the other arrays is at indices [k + i₁ *
          v1 + ... + iₚ * vp].  The dimension of each [vᵢ] must be
          equal to the number of dimensions of the input array [i].

          - [howmanyo] same as [howmanyi] but for output.

          For example, for the two subarrays are identified by * and +
          {v
            a = [[x * + * +
                  x x x x x
                  x * + * +
                  x x x x x ]]
          v}
          one sets: [ofsi = [|0; 1|]], [inci = [|2; 2|]] and [howmanyi
          = [ [|0; 1|] ]] (or [ofso], [inco] and [howmanyo] if it is an
          output array).
       *)

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> r2c plan
      (** [r2c i o] returns a plan for computing the {i forward}
          transform from the real array [i] to the complex array [o].
          Note that the last (for the C layout, or first for the
          fortran layout) dimension of [o] must be d/2+1 where d
          denotes the last dimension of [i].

	  See {!Fftw3.Sig.Genarray.dft} for the meaning of the other
	  optional parameters. *)

    val c2r : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> c2r plan
      (** [c2r i o] returns a plan for computing the {i backward}
          transform from the complex array [i] to the complex array
          [o].  Note that, by default, executing the plan returned by
          [c2r] destroys the input array [i].  You can use
          [~destroy_input:false] to generate a plan that does not
          modify [i] at the expense of being slower — it is only possible
          in 1D and if no such plan can be created, {!Fftw3.Sig.Failure}
          is raised.

	  See {!Fftw3.Sig.Genarray.dft} for the meaning of the other
	  optional parameters. *)

    val r2r : r2r_kind array ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> r2r plan
      (** [r2r kind i o] returns a plan for computing the transform
          from the complex array [i] to the complex array [o].  The
          type of transform along the dimension [k] is given by
          [kind.(k)] (you must give as many kinds as there are
          dimensions to the input array [i]).

          Note that the default value of [destroy_input] is [false]
          but you may want to change it to [true], especially in case
          one of the {!r2r_kind} is [HC2R] in order to allow the use
          of more efficient algorithms.  Try this if
          {!Fftw3.Sig.Failure} is raised.

          See {!Fftw3.Sig.Genarray.dft} for the meaning of optional
          parameters. *)
  end


  (** FFT of Bigarray.Array1. *)
  module Array1 :
  sig
    val create: ('a, 'b) kind -> 'c layout -> int -> ('a, 'b, 'c) Array1.t
      (** See {!Fftw3.Sig.Genarray.create}. *)

    val of_array : ('a, 'b) kind -> 'c layout -> 'a array -> ('a, 'b, 'c) Array1.t
      (** [of_array kind layout a] build a one-dimensional aligned big
          array initialized from the given array. *)

    type 'l complex_array = (Complex.t, complex_elt, 'l) Array1.t
        (** Double precision complex 1D array. *)

    type 'l float_array   = (float, float_elt, 'l) Array1.t
        (** Double precision float 1D array. *)


    val dft : dir -> ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi:int list ->
      ?ni:int -> ?ofsi:int -> ?inci:int -> 'l complex_array ->
      ?howmanyo:int list ->
      ?no:int -> ?ofso:int -> ?inco:int -> 'l complex_array
      -> c2c plan
      (** [dft dir x y] returns a plan to compute the DFT of [x] and
          store it in [y].

          The parameters [meas], [destroy_input], [unaligned]
          are as for {!Fftw3.Sig.Genarray.dft}.

          @param n the logical length of the array.  If not provided, it
          is automatically computed from [ofsi], [inci] and [Array1.dim
          x].

          Remark: If you want to transform several 1D arrays at once,
          use {!Fftw3.Sig.Array2.dft} with [~howmanyi] and [~howmanyo]
          set {{:../Genarray#many} appropriately}. *)

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: int list ->
      ?ni: int -> ?ofsi: int -> ?inci: int -> 'l float_array ->
      ?howmanyo: int list ->
      ?no: int -> ?ofso: int -> ?inco: int -> 'l complex_array
      -> r2c plan
      (** See {!Fftw3.Sig.Genarray.r2c}. *)

    val c2r : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: int list ->
      ?ni: int -> ?ofsi: int -> ?inci: int -> 'l complex_array ->
      ?howmanyo: int list ->
      ?no: int -> ?ofso: int -> ?inco: int -> 'l float_array
      -> c2r plan
      (** See {!Fftw3.Sig.Genarray.c2r}. *)

    val r2r : r2r_kind -> ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi:int list ->
      ?ni:int -> ?ofsi:int -> ?inci:int -> 'l float_array ->
      ?howmanyo:int list ->
      ?no:int -> ?ofso:int -> ?inco:int -> 'l float_array
      -> r2r plan
      (** See {!Fftw3.Sig.Genarray.r2r}. *)
  end


  (** FFT of Bigarray.Array2. *)
  module Array2 :
  sig
    val create: ('a, 'b) kind -> 'c layout -> int -> int -> ('a, 'b, 'c) Array2.t
      (** See {!Fftw3.Sig.Genarray.create}. *)

    type 'l complex_array = (Complex.t, complex_elt, 'l) Array2.t
        (** Double precision complex 2D array. *)

    type 'l float_array   = (float, float_elt, 'l) Array2.t
        (** Double precision float 2D array. *)

    type coord = int * int
        (** Coordinates of emlements of the 2D array. *)

    val dft : dir ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> c2c plan
      (** See {!Fftw3.Sig.Genarray.dft}. *)

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> r2c plan
      (** See {!Fftw3.Sig.Genarray.r2c}. *)

    val c2r : ?meas:measure ->
      ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> c2r plan
      (** See {!Fftw3.Sig.Genarray.c2r}. *)

    val r2r : r2r_kind * r2r_kind -> ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> r2r plan
      (** See {!Fftw3.Sig.Genarray.r2r}. *)
  end


  (** FFT of Bigarray.Array3. *)
  module Array3 :
  sig
    val create: ('a, 'b) kind -> 'c layout -> int -> int -> int
      -> ('a, 'b, 'c) Array3.t
      (** See {!Fftw3.Sig.Genarray.create}. *)

    type 'l complex_array = (Complex.t, complex_elt, 'l) Array3.t
        (** Double precision complex 3D array. *)

    type 'l float_array   = (float, float_elt, 'l) Array3.t
        (** Double precision float 3D array. *)

    type coord = int * int * int
        (** Coordinates of emlements of the 3D array. *)

    val dft : dir ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n: int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> c2c plan
      (** See {!Fftw3.Sig.Genarray.dft}. *)

    val r2c : ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l complex_array
      -> r2c plan
      (** See {!Fftw3.Sig.Genarray.r2c}. *)

    val c2r : ?meas:measure ->
      ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l complex_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> c2r plan
      (** See {!Fftw3.Sig.Genarray.c2r}. *)

    val r2r : r2r_kind * r2r_kind * r2r_kind ->
      ?meas:measure ->
      ?destroy_input:bool -> ?unaligned:bool ->
      ?howmany_n:int array ->
      ?howmanyi: coord list ->
      ?ni: coord -> ?ofsi: coord -> ?inci: coord -> 'l float_array ->
      ?howmanyo: coord list ->
      ?no: coord -> ?ofso: coord -> ?inco: coord -> 'l float_array
      -> r2r plan
      (** See {!Fftw3.Sig.Genarray.r2r}. *)
  end
end


(** Double precision FFTW. *)
module D : Sig
  with type float_elt = Bigarray.float64_elt
  and type complex_elt = Bigarray.complex64_elt

(** Single precision FFTW.  This is only available if the single
    precision FFTW3 library was found when this module was
    compiled (if not, all functions raise [Failure]). *)
module S : Sig
  with type float_elt = Bigarray.float32_elt
  and type complex_elt = Bigarray.complex32_elt

(** Managing wisdom.  Save and restore plans to/from disk or other
    media. *)
module Wisdom :
sig

  val export : (char -> unit) -> unit
    (** [Wisdom.export write] exports the current wisdom to any medium,
        as specified by the callback function [write].

        This function is not thread safe.
    *)

  val to_file : string -> unit
    (** [Wisdom.to_file fname] writes the current wisdom to the file
        [fname].
         *)
    (* FIXME: @param mode The file is created with permissions [mode].
        Default: [0o644].  *)

  val to_string : unit -> string
    (** [Wisdom.to_string()] exports the current wisdom as a string. *)


  val import : (unit -> char) -> unit
    (** [Wisdom.import read] imports wisdom from any input medium, as
        specified by the callback function [read].  If the end of the
        input data is reached (which should never happen for valid
        data), [read] should raise [End_of_file].  The imported wisdom
        {i replaces} any wisdom accumulated by the running program.

        This function is not thread safe.

        @raise Failure if the wisdom was not successfully read. *)

  val from_file : string -> unit
    (** [Widsom.from_file fname] replace the current wisdom with the
        one read from the file [fname].

        @raise Failure if the wisdom was not successfully read. *)

  val from_string : string -> unit
    (** [Wisdom.from_string s] replace the current wisdom whith the
        one read from [s].

        @raise Failure if the wisdom was not successfully read. *)

  val from_system : unit -> unit
    (** [Wisdom.from_system()] replace the current wisdom with one read
        from an implementation-defined standard file (e.g. /etc/fftw/wisdom).

        @raise Failure if the wisdom was not successfully read. *)


  val forget : unit -> unit
    (** [Wisdom.forget()] causes all accumulated wisdom to be
        discarded.  (New wisdom can be gathered subsequently
        though.) *)
end
