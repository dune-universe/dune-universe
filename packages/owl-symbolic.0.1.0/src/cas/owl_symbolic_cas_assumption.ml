(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(*

Assumption rules :
    'integer        ->  rational',
    'rational       ->  real',
    'rational       ->  algebraic',
    'algebraic      ->  complex',
    'transcendental ==  complex & !algebraic',
    'real           ->  hermitian',
    'imaginary      ->  complex',
    'imaginary      ->  antihermitian',
    'extended_real  ->  commutative',
    'complex        ->  commutative',
    'complex        ->  finite',

    'odd            ==  integer & !even',
    'even           ==  integer & !odd',

    'real           ->  complex',
    'extended_real  ->  real | infinite',
    'real           ==  extended_real & finite',

    'extended_real        ==  extended_negative | zero | extended_positive',
    'extended_negative    ==  extended_nonpositive & extended_nonzero',
    'extended_positive    ==  extended_nonnegative & extended_nonzero',

    'extended_nonpositive ==  extended_real & !extended_positive',
    'extended_nonnegative ==  extended_real & !extended_negative',

    'real           ==  negative | zero | positive',
    'negative       ==  nonpositive & nonzero',
    'positive       ==  nonnegative & nonzero',

    'nonpositive    ==  real & !positive',
    'nonnegative    ==  real & !negative',

    'positive       ==  extended_positive & finite',
    'negative       ==  extended_negative & finite',
    'nonpositive    ==  extended_nonpositive & finite',
    'nonnegative    ==  extended_nonnegative & finite',
    'nonzero        ==  extended_nonzero & finite',

    'zero           ->  even & finite',
    'zero           ==  extended_nonnegative & extended_nonpositive',
    'zero           ==  nonnegative & nonpositive',
    'nonzero        ->  real',

    'prime          ->  integer & positive',
    'composite      ->  integer & positive & !prime',
    '!composite     ->  !positive | !even | prime',

    'irrational     ==  real & !rational',

    'imaginary      ->  !extended_real',

    'infinite       ->  !finite',
    'noninteger     ==  extended_real & !integer',
    'extended_nonzero == extended_real & !zero',

 *)

open Owl_symbolic_symbol
open Owl_symbolic_types

(* TODO: These temporary rules are only hacks tailored for the example *)

let is_rational n =
  match Owl_graph.attr n with
  | Int _ -> true
  | Div _ -> true
  | _     -> false


let is_zero n =
  match Owl_graph.attr n with
  | Int s     -> s.value = 0
  | Float s   -> s.value = 0.
  | Complex s -> s.real = 0. && s.img = 0.
  | _         -> false


let set_rational n =
  let sym = Owl_graph.attr n in
  let attr = Owl_symbolic_symbol.attrs sym in
  let a = "is_rational", ATTR_Bool true in
  let attr = Array.append attr [| a |] in
  Owl_symbolic_symbol.set_attrs sym attr;
  Owl_graph.set_attr n sym
