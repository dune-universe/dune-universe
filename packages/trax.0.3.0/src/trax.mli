type location = Text of string
              | Raw_backtrace of Printexc.raw_backtrace
  (** The type of locations that make up the exception trace *)

exception Traced of exn * location list
  (** Any exception and its trace *)

val wrap : location -> exn -> exn
  (** Add location to this exception's trace.
      If the input exception is not already wrapped, it gets wrapped into
      a [Traced] exception. If the original exception is already wrapped
      in a [Traced] exception, it gets unwrapped and rewrapped with
      the new, extended trace.
  *)

val unwrap : exn -> exn
  (** Recover the original exception, for inspection purposes.
      For instance [Traced(Not_found, [...])] would become [Not_found]. *)

val wrap_with_stack_trace : exn -> exn
  (** Wrap an exception with the current exception backtrace
      (stack trace recorded at the point where
      the exception was raised, assuming no other exception was raised
      in-between). This is only guaranteed to work
      right after catching an exception with a try-with. *)

val raise_at : location -> exn -> 'a
  (** Raise or reraise an exception after adding a location to its trace. *)

val raise : string -> exn -> 'a
  (** Raise or reraise an exception after adding a text location
      to its trace. Typical usage is [Trax.raise __LOC__ e]. *)

val reraise_with_stack_trace : exn -> 'a
  (** Re-raise an exception after wrapping it with the current
      exception backtrace (stack trace recorded at the point where
      the exception was raised, assuming no other exception was raised
      in-between). This is only guaranteed to work
      right after catching an exception with a try-with. *)

val to_string : exn -> string
  (** Format the exception and its trace into text. *)

val get_trace : exn -> string
  (** Format the trace into text *)

val print : out_channel -> exn -> unit
  (** Print the exception and its trace. *)
