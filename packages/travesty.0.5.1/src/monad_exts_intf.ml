(* This file is part of 'travesty'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [S] contains extensions for a monad.

    To create an instance of [S], use {{!Extend} Extend}. *)
module type S = sig
  (** The type of the extended monad. *)
  type 'a t

  (** {3 Haskell-style operators} *)

  val then_m : _ t -> 'a t -> 'a t
  (** [then_m x y] sequentially composes the actions [x] and [y] as with
      [>>=], but, rather than using the returned value of [x], it instead
      just returns [y]. *)

  val ( >> ) : _ t -> 'a t -> 'a t
  (** [x >> y] is [then_m x y]. *)

  val compose_m : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  (** [compose_m f g] is the Kleisli composition of [f] and [g]. *)

  val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
  (** [x >=> y] is [k_compose x y]. *)

  (** {3 Guarded monadic computations} *)

  val map_when_m :
    ?otherwise:('a -> 'a t) -> bool -> 'a -> f:('a -> 'a t) -> 'a t
  (** [map_when_m ?otherwise condition ~f a] is [f a] when [condition] is
      true, and [otherwise a] (by default, [return]) otherwise. *)

  val when_m :
    ?otherwise:(unit -> unit t) -> bool -> f:(unit -> unit t) -> unit t
  (** [when_m ?otherwise condition ~f] is [f ()] when [condition] is true,
      and [otherwise ()] (by default, [return]) otherwise. *)

  val map_unless_m :
    ?otherwise:('a -> 'a t) -> bool -> 'a -> f:('a -> 'a t) -> 'a t
  (** [map_unless_m ?otherwise condition ~f a] is [f a] when [condition] is
      false, and [otherwise a] (by default, [return]) otherwise. *)

  val unless_m :
    ?otherwise:(unit -> unit t) -> bool -> f:(unit -> unit t) -> unit t
  (** [unless_m ?otherwise condition ~f] is [f ()] when [condition] is
      false, and [otherwise ()] (by default, [return]) otherwise. *)

  (** {3 Executing monadic effects in the middle of pipelines} *)

  val tee_m : 'a -> f:('a -> unit t) -> 'a t
  (** [tee_m val ~f] executes [f val] for its monadic action, then returns
      [val].

      Example (using an {{!Travesty_base_exts.Or_error} extended Or_error}):

      {[
        let fail_if_negative x =
          On_error.when_m (Int.is_negative x)
            ~f:(fun () -> Or_error.error_string "value is negative!")
        in
        Or_error.(
          42 |> tee_m ~f:fail_if_negative >>| (fun x -> x * x)
        ) (* Ok (1764) *)
      ]} *)

  val tee : 'a -> f:('a -> unit) -> 'a t
  (** [tee val ~f] behaves as {{!tee_m} tee}, but takes a non-monadic [f].

      Example (using an {{!Travesty_base_exts.Or_error} extended Or_error}):

      {[
        let print_if_negative x =
          if Int.negative x then Stdio.print_string "value is negative!"
        in
        Or_error.(
          try_get_value ()
          >>= tee ~f:print_if_negative
          >>= try_use_value ()
        )
      ]} *)
end
