(* File: odepack.ml

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Bigarray

type vec = (float, float64_elt, fortran_layout) Array1.t
type mat = (float, float64_elt, fortran_layout) Array2.t
type int_vec = (int32, int32_elt, fortran_layout) Array1.t

(* specialize version to int (for speed) *)
let max i j = if (i:int) > j then i else j

type vec_field = float -> vec -> vec -> unit
(* [f t y y'] where y' must be used for the storage of the vector
   field at (t,y):  y' <- f(t,y). *)

type jacobian =
| Auto_full
| Auto_band of int * int
| Full of (float -> vec -> mat -> unit)
| Band of int * int * (float -> vec -> int -> mat -> unit)

type task = TOUT | One_step | First_msh_point | TOUT_TCRIT | One_step_TCRIT

type t = {
  f: vec_field;
  mutable t: float;
  y: vec;
  mutable state: int;
  mutable tout: float;
  mutable tout_next: float; (* time to reach after we are at [tout],
                               possibly stopping for roots.  *)
  rwork: vec;
  iwork: int_vec;
  jroot: int_vec;
  advance: float option -> unit;
}

let dummy_int_vec = Array1.create int32 fortran_layout 0

let time ode = ode.t
let vec ode = ode.y
let _hu ode = ode.rwork.{11}
let _hcur ode = ode.rwork.{12}
let _tcur ode = ode.rwork.{13}
let _tolsf ode = ode.rwork.{14}
let _tsw ode = ode.rwork.{15} (* only for lsoda *)
let _nst ode = ode.iwork.{11}
let _nfe ode = ode.iwork.{12}
let _nje ode = ode.iwork.{13}
let _nqu ode = ode.iwork.{14}
let _nqcur ode = ode.iwork.{15}
let _imxer ode = ode.iwork.{16} (* FIXME: fortran/C layout *)
let advance ?time ode = ode.advance time

let sol ode t =
  let tout = Some t in
  while ode.t <> t do ode.advance tout done;
  ode.y

let has_root t = t.state = 3
let root t i = t.jroot.{i} <> 0l

let roots t =
  let ng = Array1.dim t.jroot in
  if t.state <> 3 then Array.make ng false
  else Array.init ng (fun i -> t.jroot.{i} <> 0l)

let make_errors name =
  [| (* Alt for state=1, when the Jacobian is provided. *)
    Failure(name ^ ": excess work done on this call. Wrong Jacobian?");
    Failure(name ^ ": excess work done on this call.");
    Failure(name ^ ": excess accuracy requested (tolerances too small)");
    Invalid_argument(name ^ ": see message written on stdout");
    Failure(name ^ ": repeated error test failures (check all inputs)");
    Failure(name ^ ": repeated convergence failures, perhaps bad Jacobian \
                    or tolerances");
    Failure(name ^ ": error weight became zero during problem");
    Failure(name ^ ": work space insufficient to finish (see messages)");
    Failure(name ^ ": Unknown error (contact library author)") |]

let raise_error_of_state exn ~jac_given state =
  if state < 0 then (
    if state >= -7 then
      if jac_given && state = -1 then raise exn.(0)
      else raise exn.(-state)
    else
      raise exn.(8)
  )

(* The vector flield (type [vec_field]) and jacobian (type [float ->
   vec -> int -> mat -> unit]) functions must be registered.  The
   Jacobian must have Fortran layout as it must be presented in a
   columnwise manner. *)
external lsoda_ : vec_field -> vec -> float -> float ->
  itol:int -> rtol:vec -> atol:vec -> task -> state:int ->
  rwork:vec -> iwork:int_vec ->
  jac:(float -> vec -> int -> mat -> unit) -> jt:int ->
  ydot:vec -> pd:mat -> int
    = "ocaml_odepack_dlsoda_bc" "ocaml_odepack_dlsoda"

external set_iwork : int_vec -> ml:int -> mu:int -> ixpr:bool ->
  mxstep:int -> unit
  = "ocaml_odepack_set_iwork"

external xsetf : int -> unit = "ocaml_odepack_xsetf"


let tolerances name neq rtol rtol_vec atol atol_vec =
  let itol, rtol = match rtol_vec with
    | None ->
      let v = Array1.create float64 fortran_layout 1 in
      v.{1} <- rtol;
      1, v
    | Some v ->
      if Array1.dim v <> neq then
        invalid_arg(name ^ ": dim rtol_vec <> size ODE system");
      3, v  in
  let itol, atol = match atol_vec with
    | None ->
      let v = Array1.create float64 fortran_layout 1 in
      v.{1} <- atol;
      itol, v
    | Some v ->
      if Array1.dim v <> neq then
        invalid_arg(name ^ ": dim atol_vec <> size ODE system");
      itol + 1, v  in
  itol, rtol, atol

let dummy_jac _ _ _ _ = ()

let lsoda_errors = make_errors "Odepack.lsoda"
let lsoda_advance_errors = make_errors "Odepack.advance (lsoda)"

let lsoda ?(rtol=1e-6) ?rtol_vec ?(atol=1e-6) ?atol_vec ?(jac=Auto_full)
    ?(mxstep=500) ?(copy_y0=true) ?(debug=true) ?(debug_switches=false)
    f y0 t0 tout =
  let neq = Array1.dim y0 in
  let itol, rtol, atol =
    tolerances "Odepack.lsoda" neq rtol rtol_vec atol atol_vec in
  (* FIXME: int allocates "long" on the C side, hence too much is alloc?? *)
  let jac_given, jt, ml, mu, jac, dim1_jac, lrs = match jac with
    | Auto_full ->
       false, 2, 0, 0, dummy_jac, neq, 22 + (9 + neq) * neq
    | Auto_band(ml, mu) ->
       false, 5, ml, mu, dummy_jac, ml + mu + 1,
       22 + 10 * neq + (2 * ml + mu) * neq
    | Full jac ->
       true, 1, 0, 0, (fun t y _ pd -> jac t y pd), neq, 22 + (9 + neq) * neq
    | Band (ml, mu, jac) ->
       true, 4, ml, mu, jac, ml + mu + 1, 22 + 10 * neq + (2 * ml + mu) * neq in
  let lrn = 20 + 16 * neq in
  let rwork = Array1.create float64 fortran_layout (max lrs lrn) in
  (* Create bigarrays, proxy for rwork, that will encapsulate the
     array of devivatives or the jacobian for OCaml.  The part of
     [rwork] they will point too will be changed by the C code. *)
  let ydot = Array1.sub rwork 1 neq in
  let pd = genarray_of_array1 (Array1.sub rwork 1 (dim1_jac * neq)) in
  let pd = reshape_2 pd dim1_jac neq in
  (* Optional inputs. 0 = default value. *)
  rwork.{5} <- 0.; (* H0 *)
  rwork.{6} <- 0.; (* HMAX *)
  rwork.{7} <- 0.; (* HMIN *)
  let iwork = Array1.create int32 fortran_layout (20 + neq) in
  set_iwork iwork ~ml ~mu ~ixpr:debug_switches ~mxstep;
  xsetf (if debug then 1 else 0);
  let y0 =
    if copy_y0 then
      let y = Array1.create float64 fortran_layout (Array1.dim y0) in
      Array1.blit y0 y;
      y
    else y0 in
  let state = lsoda_ f y0 t0 tout ~itol ~rtol ~atol TOUT ~state:1
    ~rwork ~iwork ~jac ~jt  ~ydot ~pd in
  raise_error_of_state lsoda_errors state ~jac_given;

  let rec advance = function
    | None -> ()
    | Some t ->
       xsetf (if debug then 1 else 0); (* FIXME: ~ costs more than desired? *)
       let state = lsoda_ f ode.y t0 t ~itol ~rtol ~atol TOUT ~state:ode.state
                          ~rwork ~iwork ~jac ~jt ~ydot ~pd in
       raise_error_of_state lsoda_advance_errors state ~jac_given;
       ode.t <- t;
       ode.state <- state
  and ode = { f = f;  t = tout; y = y0;
              tout = tout;  tout_next = tout; (* not used for lsoda *)
              state = state;  rwork = rwork;  iwork = iwork;
              jroot = dummy_int_vec;
              advance = advance } in
  ode


external lsodar_ :
  vec_field -> vec -> float -> float ->
  itol:int -> rtol:vec -> atol:vec -> task -> state:int ->
  rwork:vec -> iwork:int_vec ->
  jac:(float -> vec -> int -> mat -> unit) -> jt:int ->
  ydot:vec -> pd:mat ->
  g:vec_field -> gout:vec -> jroot:int_vec
  -> int * float
  = "ocaml_odepack_dlsodar_bc" "ocaml_odepack_dlsodar"

let lsodar_errors = make_errors "Odepack.lsodar"
let lsodar_advance_errors = make_errors "Odepack.advance (lsodar)"

let lsodar ?(rtol=1e-6) ?rtol_vec ?(atol=1e-6) ?atol_vec ?(jac=Auto_full)
           ?(mxstep=500) ?(copy_y0=true) ?(debug=true) ?(debug_switches=false)
           ~g ~ng  f y0 t0 tout =
  let neq = Array1.dim y0 in
  let itol, rtol, atol =
    tolerances "Odepack.lsodar" neq rtol rtol_vec atol atol_vec in
  let jac_given, jt, ml, mu, jac, dim1_jac, lrs = match jac with
    | Auto_full ->
       false, 2, 0, 0, dummy_jac, neq, 22 + (9 + neq) * neq
    | Auto_band(ml, mu) ->
       false, 5, ml, mu, dummy_jac, ml + mu + 1,
       22 + 10 * neq + (2 * ml + mu) * neq
    | Full jac ->
       true, 1, 0, 0, (fun t y _ pd -> jac t y pd), neq, 22 + (9 + neq) * neq
    | Band (ml, mu, jac) ->
       true, 4, ml, mu, jac, ml + mu + 1,
       22 + 10 * neq + (2 * ml + mu) * neq in
  let lrn = 20 + 16 * neq + 3 * ng in
  let rwork = Array1.create float64 fortran_layout (max lrs lrn) in
  (* Create bigarrays, proxy for rwork, that will encapsulate the
     array of devivatives or the jacobian for OCaml.  The part of
     [rwork] they will point too will be changed by the C code. *)
  let ydot = Array1.sub rwork 1 neq in
  let pd = genarray_of_array1 (Array1.sub rwork 1 (dim1_jac * neq)) in
  let pd = reshape_2 pd dim1_jac neq in
  (* Optional inputs. 0 = default value. *)
  rwork.{5} <- 0.; (* H0 *)
  rwork.{6} <- 0.; (* HMAX *)
  rwork.{7} <- 0.; (* HMIN *)
  let iwork = Array1.create int32 fortran_layout (20 + neq) in
  set_iwork iwork ~ml ~mu ~ixpr:debug_switches ~mxstep;
  if ng < 0 then invalid_arg "Odepack.lsodar: ng < 0";
  let gout = Array1.sub rwork 1 ng in (* correct loc → C code *)
  let jroot = Array1.create int32 fortran_layout ng in
  let y0 =
    if copy_y0 then
      let y = Array1.create float64 fortran_layout (Array1.dim y0) in
      Array1.blit y0 y;
      y
    else y0 in
  xsetf (if debug then 1 else 0);
  let state, t = lsodar_ f y0 t0 tout ~itol ~rtol ~atol TOUT ~state:1
                         ~rwork ~iwork ~jac ~jt  ~ydot ~pd ~g ~gout ~jroot in
  raise_error_of_state lsodar_errors state ~jac_given;

  let rec ode = { f = f;  t = t;  y = y0;
                  state = state;  tout = tout;  tout_next = tout;
                  rwork = rwork;  iwork = iwork;
                  jroot = jroot;
                  advance = advance }
  and call_lsodar t =
    let state, t = lsodar_ f ode.y t0 t ~itol ~rtol ~atol TOUT ~state:ode.state
                           ~rwork ~iwork ~jac ~jt ~ydot ~pd ~g ~gout ~jroot in
    raise_error_of_state lsodar_advance_errors state ~jac_given;
    if state = 2 && ode.tout <> ode.tout_next then (
      (* [t = tout] but it is an old final time that was desired.  No
         need to stop there now. *)
      ode.tout <- ode.tout_next;
      let state, t =
        lsodar_ f ode.y t0 ode.tout ~itol ~rtol ~atol TOUT ~state:2
                ~rwork ~iwork ~jac ~jt ~ydot ~pd ~g ~gout ~jroot in
      raise_error_of_state lsodar_advance_errors state ~jac_given;
      ode.state <- state;
      ode.t <- t;
    )
    else (
      ode.state <- state;
      ode.t <- t;
    )
  and advance t =
    xsetf (if debug then 1 else 0); (* FIXME: ~ costs more than desired? *)
    match t with
    | None ->
       if ode.t <> ode.tout then call_lsodar ode.tout
    | Some t ->
       ode.tout_next <- t;
       if ode.t <> ode.tout then call_lsodar ode.tout (* state = 3 *)
       else (* ode.t = ode.tout, possible root at tout *)
         if t <> ode.tout then (
           ode.tout <- t;
           call_lsodar t;
         ) in
  ode
