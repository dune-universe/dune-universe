open! Core_kernel

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

module type S = sig

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
    :  ?cr                 : CR.t (** default is [CR]            *)
    -> ?hide_positions     : bool (** default is [false]         *)
    -> ?max_binable_length : int  (** default is [Int.max_value] *)
    -> Source_code_position.t
    -> (module Stable_without_comparator with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_stable_int63able_type] works like [print_and_check_stable_type],
      and includes [Int63.t] serializations. *)
  val print_and_check_stable_int63able_type
    :  ?cr                 : CR.t (** default is [CR]            *)
    -> ?hide_positions     : bool (** default is [false]         *)
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
    :  ?cr             : CR.t (** default is [CR]   *)
    -> ?hide_positions : bool (** default is [false] *)
    -> Source_code_position.t
    -> (module Print_bin_ios_with_max_arg with type t = 'a)
    -> 'a list
    -> unit

  (** [require here bool] is a no-op if [bool = true], but if not, prints a [CR
      require-failed:], which will appear in the expect-test output.  The CR will appear
      in the feature owner's [fe todo], thus preventing release of the feature.  [require]
      is an expect-test-friendly version of [assert].  It works with the normal
      expect-test workflow because it does not raise, and it prevents mistakenly releasing
      features that violate a required property.  There is no need to 'X' a [CR
      require-failed]; simply fix the property tested by the [require] and re-run the test
      to restore the empty output.

      [require] prints [if_false_then_print_s] only if [bool = false].  It is useful for
      including information that may help debug the problem, but that would otherwise be
      too voluminous.  [if_false_then_print_s] is lazy to avoid construction of the sexp
      except when needed. *)
  val require
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] *)
    -> ?if_false_then_print_s : Sexp.t Lazy.t
    -> Source_code_position.t
    -> bool
    -> unit

  (** [show_raise] calls [f ()] and prints the exception that it raises, or, if it doesn't
      raise, prints [did not raise].  [show_raise] ignores the result of [f] so that one
      doesn't have to put an [ignore] inside the body of an [f] that is expected to raise.
      [~hide_positions:true] operates as in [print_s], to make output less fragile. *)
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
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] *)
    -> ?show_backtrace : bool (** default is [false] *)
    -> Source_code_position.t
    -> (unit -> unit)
    -> unit

  (** [show_allocation] calls [f ()] and prints out the allocations, major and minor, of
      [f].  If [f] returns a value that should be ignored, use this idiom:

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
  val show_allocation : (unit -> 'a) -> 'a

  (** [require_no_allocation f] combines [show_allocation] with [require].  If [f] does
      not allocate, nothing is printed.  If [f] allocates, a summary of the allocation is
      printed along with a CR comment.  If [f] returns a value that should be ignored, you
      should use the idiom as described above for [show_allocation]. *)
  val require_no_allocation
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] *)
    -> Source_code_position.t
    -> (unit -> 'a)
    -> 'a

  (** [print_and_check_container_sexps] prints the sexp representation of maps, sets, hash
      tables, and hash sets based on the given values.  For sets and hash sets, prints a
      CR if the sexp does not correspond to a list of elements.  For maps and hash tables,
      prints a CR if the sexp does not correspond to an association list keyed on
      elements. *)
  val print_and_check_container_sexps
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] *)
    -> Source_code_position.t
    -> (module With_containers with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_comparable_sexps] is like [print_and_check_container_sexps] for
      maps and sets only. *)
  val print_and_check_comparable_sexps
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] *)
    -> Source_code_position.t
    -> (module With_comparable with type t = 'a)
    -> 'a list
    -> unit

  (** [print_and_check_hashable_sexps] is like [print_and_check_container_sexps] for hash
      tables and hash sets only. *)
  val print_and_check_hashable_sexps
    :  ?cr             : CR.t (** default is [CR]    *)
    -> ?hide_positions : bool (** default is [false] *)
    -> Source_code_position.t
    -> (module With_hashable with type t = 'a)
    -> 'a list
    -> unit
end

module type Expect_test_helpers_kernel = sig
  module type S = S

  include S

  module CR : module type of struct include CR end

  (** We export [Expect_test_config] to override [Expect_test_config.run f] so that, if [f
      ()] raises, [run] prints the exception rather than raising.  Printing works better
      with the expect-test workflow than an unhandled exception, because there is a
      [.corrected] file that one can accept and inspect. *)
  module Expect_test_config : Expect_test_config.S with type 'a IO.t = 'a

  module Make (Print : Print) : S
end
