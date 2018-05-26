(* File: lbfgs_FC.ml

   Copyright (C) 2011

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* FORTRAN/C code *)

let empty_vec = Array1.create float64 layout 0

let nbd_of_lu n c_ofsl (lopt: vec option) c_ofsu (uopt: vec option) =
  let nbd = Array1.create int32 layout n in
  match lopt, uopt with
  | None, None -> Array1.fill nbd 0l; (empty_vec, empty_vec, nbd)
  | Some l, None ->
    let min_diml = c_ofsl + n in
    if Array1.dim l < min_diml then
      invalid_arg(sprintf "%s.min: dim(l): valid=[%i..[ got=%i"
                    MOD min_diml (Array1.dim l));
    for i = FIRST to LAST(n) do
      if is_nan l.{i} || l.{i} = infinity then
        invalid_arg(sprintf "%s.min: l.{%i} = %f => empty domain" MOD i l.{i});
      nbd.{i} <- if l.{i} = neg_infinity then 0l else 1l
    done;
    (l, empty_vec, nbd)
  | None, Some u ->
    let min_dimu = c_ofsu + n in
    if Array1.dim u < min_dimu then
      invalid_arg(sprintf "%s.min: dim(u): valid=[%i..[ got=%i"
                    MOD min_dimu (Array1.dim u));
    for i = FIRST to LAST(n) do
      if is_nan u.{i} || u.{i} = neg_infinity then
        invalid_arg(sprintf "%s.min: u.{%i} = %f => empty domain" MOD i u.{i});
      nbd.{i} <- if u.{i} = infinity then 0l else 3l
    done;
    (empty_vec, u, nbd)
  | Some l, Some u ->
    let min_diml = c_ofsl + n in
    if Array1.dim l < min_diml then
      invalid_arg(sprintf "%s.min: dim(l): valid=[%i..[ got=%i"
                    MOD min_diml (Array1.dim l));
    let min_dimu = c_ofsu + n in
    if Array1.dim u < min_dimu then
      invalid_arg(sprintf "%s.min: dim(u): valid=[%i..[ got=%i"
                    MOD min_dimu (Array1.dim u));
    for i = FIRST to LAST(n) do
      if is_nan l.{i} || l.{i} = infinity then
        invalid_arg(sprintf "%s.min: l.{%i} = %f => empty domain" MOD i l.{i});
      if is_nan u.{i} || u.{i} = neg_infinity then
        invalid_arg(sprintf "%s.min: u.{%i} = %f => empty domain" MOD i u.{i});
      nbd.{i} <-
        if l.{i} = neg_infinity then (if u.{i} = infinity then 0l else 3l)
        else (if u.{i} = infinity then 1l else 2l)
    done;
    (l, u, nbd)

let min ?(print=No) ?work ?nsteps ?stop
    ?(corrections=10) ?(factr=1e7) ?(pgtol=1e-5)
    ?n ?(ofsl=FIRST) ?l ?(ofsu=FIRST) ?u f_df ?(ofsx=FIRST) (x: vec) =
  if ofsl < FIRST then invalid_arg(sprintf "%s.min: ofsl < %i" MOD FIRST);
  if ofsu < FIRST then invalid_arg(sprintf "%s.min: ofsu < %i" MOD FIRST);
  if ofsx < FIRST then invalid_arg(sprintf "%s.min: ofsx < %i" MOD FIRST);
  (* Convert to C-style offsets *)
  let c_ofsx = ofsx - FIRST
  and c_ofsl = ofsl - FIRST
  and c_ofsu = ofsu - FIRST in
  let n = match n with
    | None ->
      let max_ofsx = LAST(Array1.dim x) in
      if ofsx > max_ofsx then
        invalid_arg(sprintf "%s.min: ofsx: valid=[%i..%i] got=%i"
                      MOD FIRST max_ofsx ofsx);
      Array1.dim x - c_ofsx
    | Some n ->
      if n <= 0 then invalid_arg(MOD ^ ".min: n <= 0");
      let min_dim = c_ofsx + n in
      if Array1.dim x < min_dim then
        invalid_arg(sprintf "%s.min: dim(x): valid=[%i..[ got=%i"
                      MOD min_dim (Array1.dim x));
      n in
  if corrections <= 0 then failwith(MOD ^ ".min: corrections <= 0");
  let l, u, nbd = nbd_of_lu n c_ofsl l c_ofsu u in
  let w = match work with
    | None -> unsafe_work n corrections
    | Some w -> check_work n corrections w; w in
  set_start w.task; (* task = "START" *)
  let continue = ref true in
  let f = ref nan
  and g = Array1.create float64 layout n in
  let stop_at_x = match nsteps, stop with
    | None, None -> (fun w -> false)
    | Some n, None -> (fun w -> Int32.to_int w.isave.{30} > n)
    | None, Some f -> f
    | Some n, Some f -> (fun w -> Int32.to_int w.isave.{30} > n || f w) in
  while !continue do
    f := setulb ~n ~m:corrections ~c_ofsx ~x ~c_ofsl ~l ~c_ofsu ~u ~nbd
      ~f:!f ~g ~factr ~pgtol
      ~wa:w.wa ~iwa:w.iwa ~task:w.task ~iprint:(int_of_print print)
      ~csave:w.csave ~lsave:w.lsave ~isave:w.isave ~dsave:w.dsave;
    match Bytes.get w.task 0 with
    | 'F' (* FG *) -> f := f_df x g
    | 'C' (* CONV *) ->
      (* the termination test in L-BFGS-B has been satisfied. *)
      continue := false
    | 'A' (* ABNO *) -> raise(Abnormal(!f, extract_c_string w.task))
    | 'E' (* ERROR *) -> invalid_arg (extract_c_string w.task)
    | 'N' (* NEW_X *) -> if stop_at_x w then continue := false
    | _ -> assert false
  done;
  1. *. !f (* unbox f *)


let max ?print ?work ?nsteps ?stop ?corrections ?factr ?pgtol
    ?n ?ofsl ?l ?ofsu ?u f_df ?ofsx (x: vec) =
  (* Play with -f *)
  let neg_f_df (x: vec) (g: vec) =
    let v = f_df x g in
    for i = FIRST to LAST(Array1.dim g) do
      g.{i} <- -. g.{i}
    done;
    -. v in
  let v = min ?print ?work ?nsteps ?stop ?corrections ?factr ?pgtol
    ?n ?ofsl ?l ?ofsu ?u neg_f_df ?ofsx x in
  -. v


(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
