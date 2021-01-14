
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

(** {1 Bindings to Quantor} *)

type lit = Qbf.Lit.t

(** {2 Direct Bindings} *)

module Raw : sig
  type t
  (** Encapsulated solver *)

  val create : unit -> t
  (** Allocate a new QBF solver *)

  val sat : t -> Qbf.result
  (** Current status of the solver *)

  val scope : t -> Qbf.quantifier -> unit
  (** Open a new scope with the given kind of quantifier *)

  val add : t -> lit -> unit
  (** Add a literal, or end the current clause/scope with [0] *)

  val deref : t -> lit -> Qbf.assignment
  (** Obtain the value of this literal in the current model *)
end

(** {2 Solver}

{[
  let cnf = Qbf.CNF.exists [1; 2] (Qbf.CNF.cnf [[1; ~-2]; [2; ~-3]]);;
  Quantor.solve cnf;;
]}
*)

val solve : Qbf.QCNF.t -> Qbf.result

val solver : Qbf.solver
