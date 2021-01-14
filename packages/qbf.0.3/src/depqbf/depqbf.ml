
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

module C = Ctypes
module F = Foreign

type nesting = int
type var_id = Qbf.Lit.t (* unsigned *)
type lit_id = Qbf.Lit.t  (* signed *)
type constraint_id = int

(* main type *)
type qdpll
let qdpll : qdpll C.structure C.typ = C.structure "QDPLL"

let qdpll_delete = F.foreign "qdpll_delete" C.(ptr qdpll @-> returning void)

(** {2 Views} *)

(* wrap in a record so that a finalizer can be used *)
type t = {
  s : qdpll C.structure C.ptr;
}

(* use a half-view to access the single field of {!t} *)
let t = C.view
  ~write:(fun {s} -> s)
  ~read:(fun s ->
    let r = {s} in
    (*
    Obj.set_tag (Obj.repr r) Obj.no_scan_tag; (* no GC inside *)
    *)
    Gc.finalise (fun {s} -> qdpll_delete s) r;
    r
  ) (C.ptr qdpll)

let lit = C.view
  ~write:(fun (i:lit_id) -> (i:>int))
  ~read:(fun i -> Qbf.Lit.make i)
  C.int

let quant = C.view
  ~write:(function
    | Qbf.Exists -> -1
    | Qbf.Forall -> 1
  ) ~read:(function
    | -1 -> Qbf.Exists
    | 1 -> Qbf.Forall
    | 0 -> failwith "quantifier undefined"
    | _ -> assert false
  ) C.int

let assignment = C.view
  ~write:(function
    | Qbf.True -> 1
    | Qbf.False -> -1
    | Qbf.Undef -> 0
  ) ~read:(function
    | 1 -> Qbf.True
    | 0 -> Qbf.Undef
    | -1 -> Qbf.False
    | n -> failwith ("unknown assignment: " ^ string_of_int n)
  ) C.int

(** {2 API} *)

let create = F.foreign "qdpll_create" C.(void @-> returning t)

let configure =
  F.foreign "qdpll_configure" C.(t @-> string @-> returning void)

(* TODO: qdpll_adjust_vars *)

let max_scope_nesting =
  F.foreign "qdpll_get_max_scope_nesting" C.(t @-> returning int)

let push = F.foreign "qdpll_push" C.(t @-> returning int)

let pop = F.foreign "qdpll_pop" C.(t @-> returning int)

let gc = F.foreign "qdpll_gc" C.(t @-> returning void)

let new_scope = F.foreign "qdpll_new_scope"
  C.(t @-> quant @-> returning int)

let new_scope_at_nesting =
  F.foreign "qdpll_new_scope_at_nesting"
    C.(t @-> quant @-> int @-> returning int)

let get_value = F.foreign "qdpll_get_value"
  C.(t @-> lit @-> returning assignment)

let add_var_to_scope =
  F.foreign "qdpll_add_var_to_scope" C.(t @-> lit @-> int @-> returning void)

(* TODO: qdpll_has_var_active_occs *)

let add = F.foreign "qdpll_add" C.(t @-> lit @-> returning void)

let add0_ = F.foreign "qdpll_add" C.(t @-> int @-> returning void)
let add0 t = add0_ t 0

let qdpll_sat = F.foreign "qdpll_sat" C.(t @-> returning int)

let sat s = match qdpll_sat s with
  | 0 -> Qbf.Unknown
  | 10 -> Qbf.Sat (get_value s)
  | 20 -> Qbf.Unsat
  | n -> failwith ("unknown depqbf result: " ^ string_of_int n)

let reset = F.foreign "qdpll_reset" C.(t @-> returning void)

let check s =
  reset s;
  sat s

let assume = F.foreign "qdpll_assume" C.(t @-> lit @-> returning void)

(* we don't know the size, it's 0-terminated *)
let qdpll_get_relevant_assumptions =
  F.foreign "qdpll_get_relevant_assumptions" C.(t @-> returning (ptr int))

let yolo_free = F.foreign "free" C.(ptr int @-> returning void)

let get_relevant_assumptions s =
  let a = qdpll_get_relevant_assumptions s in
  (* find the length: last slot is 0 *)
  let l = ref [] in
  let i = ref 0 in
  while C.( !@ (a +@ !i)) <> 0 do
    l := Qbf.Lit.make C.( !@ (a +@ !i)) :: !l;
    incr i
  done;
  yolo_free a; (* XXX: unsafe, but nothing else would work *)
  !l

(* TODO: remaining funs *)
