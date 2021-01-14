
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

type quantor
(** Abstract type of Quantor solver *)

type lit = Qbf.Lit.t

external quantor_create : unit -> quantor = "quantor_stub_create"

external quantor_delete : quantor -> unit = "quantor_stub_delete"

external quantor_sat : quantor -> int = "quantor_stub_sat"

external quantor_scope_exists : quantor -> unit = "quantor_stub_exists"

external quantor_scope_forall : quantor -> unit = "quantor_stub_forall"

external quantor_add : quantor -> int -> unit = "quantor_stub_add"

external quantor_deref : quantor -> int -> int = "quantor_stub_deref"

(** {2 Direct Bindings} *)

module Raw = struct
  type t = Quantor of quantor
  (* Put it in a block, so that it can be GC'd and so we can attach a
      finalizer to it. The finalizer will be responsible for free'ing
      the actual solver. *)

  let create () =
    let _q = quantor_create () in
    let q = Quantor _q in
    Gc.finalise (fun _ -> quantor_delete _q) q;
    q

  let deref (Quantor q) (i : lit) =
    match quantor_deref q (i:>int) with
    | 0 -> Qbf.False
    | 1 -> Qbf.True
    | -1 -> Qbf.Undef
    | n -> failwith ("unknown quantor_deref result: " ^ string_of_int n)

  let sat ((Quantor q) as solver) =
    let i = quantor_sat q in
    match i with
      | 0 -> Qbf.Unknown
      | 10 -> Qbf.Sat (fun i -> deref solver (Qbf.Lit.abs i))
      | 20 -> Qbf.Unsat
      | 30 -> Qbf.Timeout
      | 40 -> Qbf.Spaceout
      | _ -> failwith ("unknown quantor result: " ^string_of_int i)

  let scope (Quantor q) quant = match quant with
    | Qbf.Forall -> quantor_scope_forall q
    | Qbf.Exists -> quantor_scope_exists q

  let add_unsafe (Quantor q) i = quantor_add q i

  let add (Quantor q) i = quantor_add q (i:lit:>int)
end

let rec _add_cnf solver cnf = match cnf with
  | Qbf.QCNF.Quant (quant, lits, cnf') ->
      Raw.scope solver quant;
      List.iter (fun lit -> Raw.add solver lit) lits;
      Raw.add_unsafe solver 0;
      _add_cnf solver cnf'
  | Qbf.QCNF.Prop clauses ->
      List.iter
        (fun c ->
          List.iter (fun lit -> Raw.add solver lit) c;
          Raw.add_unsafe solver 0;
        ) clauses

let solve cnf =
  let quantor = Raw.create () in
  _add_cnf quantor cnf;
  Raw.sat quantor

let solver = {Qbf.solve=solve; Qbf.name="quantor";}
