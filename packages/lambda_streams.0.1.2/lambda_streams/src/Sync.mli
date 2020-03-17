(** Generalizes the notion of a synchronous, readable input stream. *)
type 'a input = private unit -> 'a

(** Generalizes the notion of a synchronous, writable output stream. *)
type 'a output = private 'a -> unit

(** Represents a connection-based input stream with an output stream to close it. *)
type 'a connection = ('a input, unit output) Connection.t

val make_input : (unit -> 'a) -> 'a input

val make_output : ('a -> unit) -> 'a output

(**
 Creates a mutator: a pair of input and output streams that together behave like a mutable variable.
 Mutating the value is accomplished by sending data to the output stream. Fetching the current
 value is done by reading the input stream.
 *)
val make_mutator : initial:'a -> 'a input * 'a output

val pure : 'a -> 'a input

(** Creates an input stream that enumerates the natural numbers. *)
val enumerate : unit -> int input

(** Gets the next value (or {i current} value if it's a behavior) from the input stream. *)
val next : 'a input -> 'a

(**
 Sends a single value to the output stream. Semantically equivalent to:
 {[Finite.Sync.pure value |> Finite.Sync.pipe output_stream]}
 But it's more efficient because it doesn't involve instatiating an input stream.
 *)
val send : 'a -> 'a output -> unit

(** Pipes an input stream into an output stream. *)
val pipe : 'a output -> 'a input -> unit

(** Like fold_left but for infinite streams, ending at signal n. *)
val accumulate : int -> ('b -> 'a -> 'b) -> 'b -> 'a input -> 'b

val map : ('a -> 'b) -> 'a input -> 'b input

(**
 Filters the input stream based on a predicate. This function returns a stream that, when invoked,
 will continue to invoke the original input stream until a value that satisfies the predicate is
 given, which is then returned. This means that an invocation of the returned stream may possibly
 never terminate.

 For example:

 {[enumerate () |> filter ((>=) 10)]}
 Will block and never terminate after [10] invocations, because after those invocations any future
 values of [enumerate ()] will never be less than [10].

 On the other hand,
 {[
  let is_even x = x mod 2 = 0 in
  enumerate () |> filter is_even
  ]}
 Will always return a value, because there will always be a successor to a natural number that is
 even (every other number).
 *)
val filter : ('a -> bool) -> 'a input -> 'a input

val scan : ('b -> 'a -> 'b) -> 'b -> 'a input -> 'b input
