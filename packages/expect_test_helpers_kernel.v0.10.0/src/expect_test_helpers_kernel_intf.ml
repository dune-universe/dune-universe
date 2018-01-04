open! Core_kernel

module Allocation_limit = struct
  type t =
    | Major_words of int
    | Minor_words of int
  [@@deriving sexp_of]
end

module CR = struct
  type t = CR | CR_soon | CR_someday | Comment
  [@@deriving sexp_of]
end

module type Print = sig
  val print_endline : string -> unit
  val print_string  : string -> unit
end

module type Print_bin_ios_arg = sig
  type t [@@deriving sexp_of]
  include Binable.S with type t := t
end

module type Print_bin_ios_with_max_arg = sig
  include Print_bin_ios_arg
  val max_binable_length : int
end

module type Set = sig
  type t [@@deriving sexp_of]

  val diff     : t -> t -> t
  val equal    : t -> t -> bool
  val is_empty : t -> bool
end

module type With_containers = sig
  type t [@@deriving sexp]
  include Comparable with type t := t
  include Hashable   with type t := t
end

module type With_comparable = sig
  type t [@@deriving sexp]
  include Comparable with type t := t
end

module type With_hashable = sig
  type t [@@deriving compare, sexp]
  include Hashable with type t := t
end

module type With_compare = sig
  type t [@@deriving compare, sexp_of]
end

module type With_equal = sig
  type t [@@deriving sexp_of]
  include Equal.S with type t := t
end

module type Expect_test_helpers_kernel = sig

  module Allocation_limit : module type of struct include Allocation_limit end

  module CR : sig
    include module type of struct include CR end

    (** [hide_unstable_output t] returns [false] if [t = CR] and [true] otherwise.  Useful
        to provide a default for arguments such as [?hide_positions] in functions that
        also have a [?cr] argument. *)
    val hide_unstable_output : t -> bool
  end

  (** [hide_positions_in_string] does line-based regexp matching to replace line numbers
      and column numbers that appear in source-code positions with constant text [LINE]
      and [COL].  This can be useful in making displayed test output less fragile. *)
  val hide_positions_in_string : string -> string

  (** Renders an s-expression as a string.  With [~hide_positions:true], patterns in the
      string that match OCaml-style file positions are modified to hide the line number,
      column number, and character positions, to make output less fragile. *)
  val sexp_to_string
    :  ?hide_positions : bool (** default is [false] *)
    -> Sexp.t
    -> string

  (** For printing an s-expression to stdout.  [hide_positions] works as in
      [sexp_to_string]. *)
  val print_s
    :  ?hide_positions : bool (** default is [false] *)
    -> Sexp.t
    -> unit

  (** [print_and_check_stable_type] prints the bin-io digest for the given type, and the
      bin-io and sexp serializations of the given values.  Prints an error message for any
      serializations that fail to round-trip, and for any bin-io serializations that
      exceed [max_binable_length]. *)
  val print_and_check_stable_type
    :  ?cr                 : CR.t (** default is [CR] *)
    -> ?hide_positions     : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?max_binable_length : int  (** default is [Int.max_value] *)
    -> Source_code_position.t
    -> (module Stable_without_comparator with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_stable_int63able_type] works like [print_and_check_stable_type],
      and includes [Int63.t] serializations. *)
  val print_and_check_stable_int63able_type
    :  ?cr                 : CR.t (** default is [CR] *)
    -> ?hide_positions     : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?max_binable_length : int  (** default is [Int.max_value] *)
    -> Source_code_position.t
    -> (module Stable_int63able with type t = 'a)
    -> 'a list
    -> unit

  (** [print_bin_ios] prints the shape digest of a [Binable] type, and the bin-io
      serialization of example values.  [print_bin_ios] is used to write expect tests that
      can detect if the serialization format of a stable type changes. *)
  val print_bin_ios
    :  (module Print_bin_ios_arg with type t = 'a)
    -> 'a list
    -> unit

  (** [print_bin_ios_with_max] is like [print_bin_ios], except it causes a CR to be
      printed (using [require]) if any serializations are longer than the supplied
      [max_binable_length].  This is useful for ensuring that serializations fit in some
      required size, e.g. an ethernet MTU. *)
  val print_bin_ios_with_max
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (module Print_bin_ios_with_max_arg with type t = 'a)
    -> 'a list
    -> unit

  (** [print_cr here message] prints a [CR require-failed], which will appear in
      expect-test output. The CR will appear in the feature owner's [fe todo], thus
      preventing release of the feature. [print_cr] is an expect-test-friendly version of
      [assert false]. It works with the normal expect-test workflow because it does not
      raise, and it prevents mistakenly releasing features that violate a required
      property. There is no need to 'X' a [CR require-failed]; simply fix the property
      that triggered the [print_cr] and re-run the test to restore the empty output. *)
  val print_cr
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> Sexp.t
    -> unit

  (** [require here bool] is a no-op if [bool = true], but if not, prints a [CR
      require-failed] similarly to [print_cr], with a message determined by the
      [if_false_then_print_s] argument, if any.

      [if_false_then_print_s] is useful for including information that may help debug the
      problem, but that would otherwise be too voluminous.  [if_false_then_print_s] is
      lazy to avoid construction of the sexp except when needed. *)
  val require
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?if_false_then_print_s : Sexp.t Lazy.t
    -> Source_code_position.t
    -> bool
    -> unit

  (** [require_equal] compares its two arguments using the equality predicate of the
      provided module. If the comparison fails, prints a message that renders the
      arguments as sexps. *)
  val require_equal
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?message        : string
    -> Source_code_position.t
    -> (module With_equal with type t = 'a)
    -> 'a
    -> 'a
    -> unit

  (** Like [require_equal], but derives an equality predicate from a comparison
      function. *)
  val require_compare_equal
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?message        : string
    -> Source_code_position.t
    -> (module With_compare with type t = 'a)
    -> 'a
    -> 'a
    -> unit

  (** Like [require_equal], but when equality fails produces a message including sexps of
      both [Set.diff first second] and [Set.diff second first] to aid in debugging. *)
  val require_sets_are_equal
    :  ?cr             : CR.t    (** default is [CR]    *)
    -> ?hide_positions : bool    (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?names          : string * string  (** default is ["first", "second"] *)
    -> Source_code_position.t
    -> (module Set with type t = 'a)
    -> 'a
    -> 'a
    -> unit

  (** [show_raise] calls [f ()] and prints the exception that it raises, or, if it doesn't
      raise, prints [did not raise].  [show_raise] ignores the result of [f] so that one
      doesn't have to put an [ignore] inside the body of an [f] that is expected to raise.
      [~hide_positions:true] operates as in [print_s], to make output less fragile.  Using
      [~show_backtrace:true] will result in a CR in the expectation, but it's still
      available here as it is still valuable when initially writing tests and
      debugging. *)
  val show_raise
    :  ?hide_positions : bool (** default is [false] *)
    -> ?show_backtrace : bool (** default is [false] *)
    -> (unit -> _)
    -> unit

  (** [require_does_not_raise] is like [show_raise], but does not print anything if the
      function does not raise, and prints a CR along with the exception if it does raise.
      Unlike for [show_raise], the supplied function is required to return [unit] to avoid
      mistakes like incomplete partial application that silently would not raise, but for
      the wrong reason. *)
  val require_does_not_raise
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?show_backtrace : bool (** default is [false] *)
    -> Source_code_position.t
    -> (unit -> unit)
    -> unit

  (** [require_does_raise] is like [show_raise], but additionally prints a CR if the
      function does not raise. *)
  val require_does_raise
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?show_backtrace : bool (** default is [false] *)
    -> Source_code_position.t
    -> (unit -> _)
    -> unit

  (** [require_allocation_does_not_exceed] is a specialized form of [require] that only
      produces output when [f ()] allocates more than the given limits.  The output will
      include the actual number of major and minor words allocated.  We do NOT include
      these numbers in the successful case because those numbers are not stable with
      respect to compiler versions and build flags.

      If [f] returns a value that should be ignored, use this idiom:

      {[
        ignore (show_allocation f : t)
      ]}

      rather than this idiom:

      {[
        show_allocation (fun () -> ignore (f () : t))
      ]}

      With the latter idiom, the compiler may optimize the computation of [f ()] taking
      advantage of the fact that the result is ignored, and eliminate allocation that is
      intended to be measured.  With the former idiom, the compiler cannot do such
      optimization and must compute the result of [f ()]. *)
  val require_allocation_does_not_exceed
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Allocation_limit.t
    -> Source_code_position.t
    -> (unit -> 'a)
    -> 'a

  (** [require_no_allocation here f] is equivalent to [require_allocation_does_not_exceed
      (Minor_words 0) here f]. *)
  val require_no_allocation
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (unit -> 'a)
    -> 'a

  (** [print_and_check_container_sexps] prints the sexp representation of maps, sets, hash
      tables, and hash sets based on the given values.  For sets and hash sets, prints a
      CR if the sexp does not correspond to a list of elements.  For maps and hash tables,
      prints a CR if the sexp does not correspond to an association list keyed on
      elements. *)
  val print_and_check_container_sexps
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (module With_containers with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_comparable_sexps] is like [print_and_check_container_sexps] for
      maps and sets only. *)
  val print_and_check_comparable_sexps
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (module With_comparable with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_hashable_sexps] is like [print_and_check_container_sexps] for hash
      tables and hash sets only. *)
  val print_and_check_hashable_sexps
    :  ?cr             : CR.t (** default is [CR] *)
    -> ?hide_positions : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> Source_code_position.t
    -> (module With_hashable with type t = 'a)
    -> 'a list
    -> unit

  (** [quickcheck] is similar to [Quickcheck.test].  It stops after the first iteration
      that prints a CR, as detected by [on_print_cr]. *)
  val quickcheck
    :  Source_code_position.t
    -> ?cr              : CR.t (** default is [CR] *)
    -> ?hide_positions  : bool (** default is [false] when [cr=CR], [true] otherwise *)
    -> ?seed            : Quickcheck.seed
    -> ?sizes           : int Sequence.t
    -> ?trials          : int
    -> ?attempts        : int
    -> ?filter          : ('a -> bool)
    -> ?shrinker        : 'a Quickcheck.Shrinker.t
    -> ?shrink_attempts : Quickcheck.shrink_attempts
    -> ?examples        : 'a list
    -> sexp_of          : ('a -> Sexp.t)
    -> f                : ('a -> unit)
    -> 'a Quickcheck.Generator.t
    -> unit

  (** [on_print_cr] determines the behavior of all functions above that print CRs, such as
      [print_cr] and [require].  The rendered string form of the CR is passed to
      [!on_print_cr].  The default value is [print_endline]; this can be overridden to
      replace or extend the default behavior.  For example, some testing harnesses may
      choose to abort a series of tests after the first CR is printed. *)
  val on_print_cr : (string -> unit) ref

  (** We export [Expect_test_config] to override [Expect_test_config.run f] so that, if [f
      ()] raises, [run] prints the exception rather than raising.  Printing works better
      with the expect-test workflow than an unhandled exception, because there is a
      [.corrected] file that one can accept and inspect. *)
  module Expect_test_config : Expect_test_config.S with type 'a IO.t = 'a
end
