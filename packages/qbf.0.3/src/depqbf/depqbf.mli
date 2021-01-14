
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Bindings to DEPQBF} *)

type nesting = int
type var_id = Qbf.Lit.t (* unsigned *)
type lit_id = Qbf.Lit.t  (* signed *)
type constraint_id = int

type t
(** A QBF solver *)

val create : unit -> t
(** New solver *)

val configure : t -> string -> unit
(** Give CLI arguments to the solver *)

val max_scope_nesting : t -> nesting

val push : t -> int
(** Push a new frame index and returns it *)

val pop : t -> int
(** Pop back to the previous frame index and return it. All clauses added
    on top of the current frame are discarded *)

val gc : t -> unit
(** Deletes unused variables, quantifier blocks, etc. *)

val new_scope : t -> Qbf.quantifier -> nesting

val new_scope_at_nesting : t -> Qbf.quantifier -> nesting -> nesting

val get_value : t -> var_id -> Qbf.assignment

val add_var_to_scope : t -> var_id -> nesting -> unit

val add : t -> lit_id -> unit
(** Add a literal to the current clause or scope *)

val add0 : t -> unit
(** Add "literal" [0], to close a scope or a clause *)

val sat : t -> Qbf.result
(** Caution: call {!reset} between two calls to [sat]. Consider
    using {!check} instead. *)

val reset : t -> unit
(** Reset the current assignment. Careful. *)

val check : t -> Qbf.result
(** Higher-level interface to {!sat} *)

val assume : t -> lit_id -> unit
(** Add an assumption (assert a lit or its negation) *)

val get_relevant_assumptions : t -> lit_id list
(** List of assumptions used to prove last UNSAT *)


