
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

(** {1 Random Generators} *)

module RS = Random.State
module G = Random_generator

module F = Qbf.Formula

type quantifier_state = {
  max_lit : int;
  lits : Qbf.Lit.t list;
}

let empty_state = { max_lit=0; lits=[]; }

(* [[i, j)] (j-i elements) *)
let range i j =
  let rec up i j acc =
    if i=j then acc else up i (j-1) (j::acc)
  in up i j []

(* create [n] fresh literals *)
let _mk_lits n qst =
  let v = qst.max_lit + 1 in
  let lits = List.map Qbf.Lit.make (range v (v+n)) in
  {max_lit=qst.max_lit+n; lits=lits@qst.lits}, lits

(* add one layer of quantification to [f'], where [f'] is a generator*)
let _quantify qst (f':quantifier_state -> F.t G.gen): F.t G.gen =
  let open G in
  G.select [ Qbf.Exists; Qbf.Forall] >>= fun q ->
  G.make_int 2 10 >>= fun num_lits ->
  let qst, lits = _mk_lits num_lits qst in
  f' qst >>= fun f' ->
  G.return (F.quantify q lits f')

(* recursive building of a formula *)
let _mk_form : (quantifier_state -> F.form G.gen) G.fueled =
  let open G in
  Fuel.fix (fun self () ->
    Fuel.choose
      [ nullary (fun qst -> pure F.atom <$> select qst.lits)
      ; unary (self ()) (fun f qst -> pure F.neg <$> f qst)
      ; nary (self()) (fun l qst ->
        let l' = List.map (fun g -> g qst) l in
        pure F.and_l <$> join_list l')
      ; nary (self()) (fun l qst ->
        let l' = List.map (fun g -> g qst) l in
        pure F.or_l <$> join_list l')
      ; binary (self()) (self()) (fun f1 f2 qst -> pure F.imply <$> f1 qst <$> f2 qst)
      ]
  ) ()

(* fuel-based generator *)
let random_form_size size : F.t G.gen =
  assert (size>0);
  let open G in

  let recurse : (quantifier_state -> F.t gen) fueled =
    Fuel.fix
      (fun self () ->
        Fuel.choose (* TODO: heavier weight on quantifier *)
          [ unary (self ()) (fun f' qst -> _quantify qst f')
          ; unary _mk_form (fun f' qst -> map F.form (f' qst))
          ]
      ) ()
  in
  let subform = (recurse <$> pure (size-1) |> backtrack) in
  let f = pure (_quantify empty_state) <$> subform in
  join f

let random_form =
  G.(make_int 2 100 >>= random_form_size)
