let rec sort_add_poly ((exp, x) as arg) = function
  | [] -> [ arg ]
  | (exp2, x2) :: q when exp2 = exp -> (exp, x +. x2) :: q
  | ((exp2, _) as arg2) :: q when exp < exp2 -> arg :: arg2 :: q
  | arg2 :: q -> arg2 :: sort_add_poly arg q

let mult_poly c p = List.map (fun (exp, x) -> (exp, c *. x)) p

let add_poly p1 p2 = List.fold_left (fun acc p -> sort_add_poly p acc) p1 p2

let eval_poly x =
  List.fold_left
    (fun (acc, dacc) (n, a) ->
      ( acc +. (a *. (x ** float n)),
        dacc +. (a *. float n *. (x ** float (n - 1))) ))
    (0.0, 0.0)

(*let diff_in_z b =
  List.fold_left
    (fun (acc, dacc) (n, a, da) ->
      ( acc +. (da *. (b ** float n)),
        dacc +. (a *. float n *. (b ** float (n - 1))) ))
    (0.0, 0.0)*)

let print_poly f =
  List.iter (fun (n, a) ->
      if n = 0 then Printf.fprintf f "%g" a
      else if n = 1 then Printf.fprintf f "+%gB" a
      else Printf.fprintf f "+%gB^%i" a n)

let rec dichotomie ?(verbose = 0) ?(factor = 1e-10) ?(relax = 0.0) ?low
    ?(up_bound = true) (bmin, bmax) f_to_evaluate target =
  (* fmin use to detect vertical asymptote *)
  let xfmin, yfmin =
    match low with None -> (bmin, fst @@ f_to_evaluate bmin) | Some v -> v
  in

  (*Format.printf "bisect [%g; %g] low:%g, %g @?" bmin bmax xfmin yfmin;*)
  if up_bound then (
    if bmax -. bmin < factor then bmin
    else
      let m = 0.5 *. (bmin +. bmax) in

      (*if bmax -. bmin <= factor then m*)
      let fx, _ = f_to_evaluate m in
      if verbose > 2 then
        Printf.printf "dicho min:%g; max :%g x:%g f:%g\n" bmin bmax m
          (fx -. target);

      (*Format.printf "eval f(%g)=%g @?" m fx;*)
      (*Printf.printf "dicho min:%g; max :%g f:%g\n" bmin bmax fx;*)
      (*Detect asymtote *)
      let detect_asymptote =
        if m < xfmin then false
        else
          let cf = classify_float (fx *. yfmin) in
          cf = FP_nan || cf = FP_infinite || fx < yfmin
      in
      let width = bmax -. bmin in
      if abs_float fx < factor || width < factor /. 10. then (*stop *) bmin
      else if detect_asymptote || fx -. target > 0.0 then
        (* go left *)
        dichotomie ~relax ~factor ?low
          (bmin -. (relax *. width), m +. (relax *. width))
          f_to_evaluate target
      else
        (* go right *)
        dichotomie ~relax ~factor ~low:(m, fx)
          (m -. (relax *. width), bmax +. (relax *. width))
          f_to_evaluate target )
  else
    let fx, _ = f_to_evaluate bmax in
    if classify_float fx = FP_nan || fx -. target > 0.0 || fx < yfmin then
      (* Go left true upperbound *)
      dichotomie ~factor ~relax ~up_bound:true ?low (bmin, bmax) f_to_evaluate
        target
    else if bmax -. bmin < 1.0 /. factor then
      (* Go right extend upperbound *)
      dichotomie ~factor ~relax ~up_bound:false ?low
        (bmin, 2.0 *. bmax)
        f_to_evaluate target
    else 0.0

(*let rec dichotomie ?(factor = 1e-5) ?(up_bound = true) (bmin, bmax)
    f_to_evaluate target =
  if up_bound then (
    let m = 0.5 *. (bmin +. bmax) in
    (*if bmax -. bmin <= factor then m*)
    let fx, _ = f_to_evaluate m in
    if !verbose > 2 then
      Printf.printf "dicho min:%g; max :%g f:%g\n" bmin bmax fx;
    if abs_float (fx -. target) < factor || bmax -. bmin < factor /. 10. then m
    else if classify_float fx = FP_nan || fx < 0.0 || fx -. target > 0.0 then
      dichotomie ~factor (bmin, m) f_to_evaluate target
    else dichotomie ~factor (m, bmax) f_to_evaluate target )
  else
    let fx, _ = f_to_evaluate bmax in
    if classify_float fx = FP_nan || fx < 0.0 || fx -. target > 0.0 then
      dichotomie ~factor ~up_bound:true (bmin, bmax) f_to_evaluate target
    else
      dichotomie ~factor ~up_bound:false
        (bmin, 2.0 *. bmax)
        f_to_evaluate target*)

exception Zero_derivative

(* Copy from Boost/math/tools/roots.hpp *)

let newton_raphson_iterate ?(factor = 1e-10) ?(max_iter = 20) ?bound f guess_p =
  let min, max =
    match bound with
    | None -> (ref (-.max_float), ref max_float)
    | Some (x, y) -> (ref x, ref y)
  in

  let guess = ref guess_p in
  let result = ref (if guess_p = infinity then 1.0 else guess_p) in
  let delta = ref max_float
  and delta1 = ref max_float
  and delta2 = ref max_float in
  try
    for _ = 0 to max_iter do
      delta2 := !delta1;
      delta1 := !delta;
      let f0, f1 = f !result in
      (*Format.printf "nr x:%g f:%g df:%g minmax: %g;%g@." !result f0 f1 !min !max;*)
      if f0 = 0.0 then raise Exit;
      if f1 = 0.0 then raise Zero_derivative;
      delta := f0 /. f1;
      (*Format.printf "delta: %g delta2:%g@." !delta !delta2;*)
      if abs_float (!delta *. 2.0) > abs_float !delta2 then (
        (*Format.printf "test@.";*)
        let shift =
          if !delta > 0.0 then (!result -. !min) /. 2.0
          else (!result -. !max) /. 2.0
        in
        if !result <> 0.0 && abs_float shift > abs_float !result then
          delta := copysign (0.9 *. abs_float !result) !delta
        else delta := shift;
        delta1 := 3.0 *. !delta;
        delta2 := 3.0 *. !delta );
      (*Format.printf "test2@.";*)
      guess := !result;
      result := !result -. !delta;
      if !result <= !min then (
        delta := 0.5 *. (!guess -. !min);
        result := !guess -. !delta;
        if !result = !min || !result = !max then raise Exit )
      else if !result >= !max then (
        delta := 0.5 *. (!guess -. !max);
        result := !guess -. !delta;
        if !result = !min || !result = !max then raise Exit );
      if !delta > 0.0 then max := !guess else min := !guess;
      if abs_float (!result *. factor) >= abs_float !delta then raise Exit
    done;
    Format.eprintf "Alert Not converging %g@." !result;
    !result
  with Exit -> !result

let newton_raphson_multivariate ?(factor = 1e-10) ?(max_iter = 20) f guess_p =
  let n = Array.length guess_p in

  let norm = Array.fold_left (fun acc x -> acc +. abs_float x) 0.0 in

  let guess = ref guess_p in
  let result = ref guess_p in
  let delta = ref (Array.make n max_float)
  and delta1 = ref (Array.make n max_float)
  and delta2 = ref (Array.make n max_float) in
  try
    for _ = 0 to max_iter do
      delta2 := !delta1;
      delta1 := !delta;
      let f0, dx = f !result in
      (*Format.printf "nr x:%g f:%g df:%g minmax: %g;%g@." !result f0 f1 !min !max;*)
      if norm f0 = 0.0 then raise Exit;
      (*if f1 = 0.0 then raise Zero_derivative;*)
      delta := dx;
      (*Format.printf "delta: %g delta2:%g@." !delta !delta2;*)
      (*Format.printf "test2@.";*)
      guess := !result;
      result := Array.map2 (fun a b -> a +. b) !result !delta;
      if factor *. norm !result >= norm !delta then raise Exit
    done;
    (*Format.eprintf "Alert Not converging @.";*)
    !result
  with Exit -> !result

let geom_law p u =
  if u < p then 0
  else 0 + (int_of_float @@ ceil @@ ((log (1.0 -. u) /. log (1.0 -. p)) -. 1.0))

module Array = struct
  include Array

  (* Computes: f a.(0) + f a.(1) + ... where + is 'g'. *)
  let foldmap g f a =
    let n = Array.length a in
    let rec aux acc i =
      if i >= n then acc else aux (g acc (f a.(i))) (succ i)
    in
    aux (f a.(0)) 1
end

let foldmap_range g f (a, b) =
  let rec aux acc n =
    let n = succ n in
    if n > b then acc else aux (g acc (f n)) n
  in
  aux (f a) a

let fold_range f init (a, b) =
  let rec aux acc n = if n > b then acc else aux (f acc n) (succ n) in
  aux init a

(* Some less-general support functions for 'solve'. *)
let swap_elem m i j =
  let x = m.(i) in
  m.(i) <- m.(j);
  m.(j) <- x

let maxtup a b = if snd a > snd b then a else b

let augmented_matrix m b =
  Array.(init (length m) (fun i -> append m.(i) [| b.(i) |]))

let pf f x =
  if abs_float x < 1e-10 then Printf.fprintf f "0" else Printf.fprintf f "%g" x

let print_mat f x =
  Array.iter
    (fun a ->
      Array.iter (fun b -> Printf.fprintf f "\t%a" pf b) a;
      Printf.fprintf f "\n")
    x

let print_vec f x = Array.iter (fun a -> Printf.fprintf f "\t%a\n" pf a) x

let print_vec_list f = function
  | [] -> ()
  | t :: q ->
      Array.iteri
        (fun i a ->
          Printf.fprintf f "%g%a\n" a
            (fun f () ->
              List.iter (fun x -> Printf.fprintf f "\t%a" pf x.(i)) q)
            ())
        t

(* Solve Ax=b for x, using gaussian elimination with scaled partial pivot,
 * and then back-substitution of the resulting row-echelon matrix.
Taken
   * from rosetta stone project. *)
exception Matrix_singular

let solve m b =
  let n = Array.length m in
  let n' = pred n in
  (* last index = n-1 *)
  let s = Array.(map (foldmap max abs_float) m) in
  (* scaling vector *)
  let a = augmented_matrix m b in

  for k = 0 to pred n' do
    (* Scaled partial pivot, to preserve precision *)
    let pair i = (i, abs_float a.(i).(k) /. s.(i)) in
    let i_max, v = foldmap_range maxtup pair (k, n') in
    if v < epsilon_float then raise Matrix_singular;
    swap_elem a k i_max;
    swap_elem s k i_max;

    (* Eliminate one column *)
    for i = succ k to n' do
      let tmp = a.(i).(k) /. a.(k).(k) in
      for j = succ k to n do
        a.(i).(j) <- a.(i).(j) -. (tmp *. a.(k).(j))
      done
    done
  done;

  (* Backward substitution; 'b' is in the 'nth' column of 'a' *)
  let x = Array.copy b in
  (* just a fresh array of the right size and type *)
  for i = n' downto 0 do
    let minus_dprod t j = t -. (x.(j) *. a.(i).(j)) in
    x.(i) <- fold_range minus_dprod a.(i).(n) (i + 1, n') /. a.(i).(i)
  done;
  x
