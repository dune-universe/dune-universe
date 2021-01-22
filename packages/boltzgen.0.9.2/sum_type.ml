open Type

let rec sort_add_poly ((exp, x, dx) as arg) = function
  | [] -> [ arg ]
  | (exp2, x2, dx2) :: q when exp2 = exp -> (exp, x +. x2, dx +. dx2) :: q
  | ((exp2, _, _) as arg2) :: q when exp < exp2 -> arg :: arg2 :: q
  | arg2 :: q -> arg2 :: sort_add_poly arg q

let eval_poly x =
  List.fold_left
    (fun (acc, dacc) (n, a, _) ->
      ( acc +. (a *. (x ** float n)),
        dacc +. (a *. float n *. (x ** float (n - 1))) ))
    (0.0, 0.0)

let diff_in_z b =
  List.fold_left
    (fun (acc, dacc) (n, a, da) ->
      ( acc +. (da *. (b ** float n)),
        dacc +. (a *. float n *. (b ** float (n - 1))) ))
    (0.0, 0.0)

let print_poly f =
  List.iter (fun (n, a, _) ->
      if n = 0 then Printf.fprintf f "%g" a
      else if n = 1 then Printf.fprintf f "+%gB" a
      else Printf.fprintf f "+%gB^%i" a n)

let rec dichotomie ?(factor = 1e-5) ?(up_bound = true) (bmin, bmax)
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
        f_to_evaluate target

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

let named_of_sum =
  let count = ref 1 in
  fun ((name, args), (ts_abstr : sum_type_def)) ->
    (* Instantiate abstract type to concrete one *)
    let instconstr_constr constr_content = function
      | Name (name_concrete, param) when name_concrete = name -> (
          match constr_content with
          | None -> None
          | Some v ->
              Some
                (List.fold_left2
                   (fun vit arg par -> instantiate arg par vit)
                   v args param) )
      | Name (n, _) ->
          print_endline ("fail to deconstruct " ^ name ^ " received " ^ n);
          assert false
      | _ ->
          print_endline ("fail to deconstruct " ^ name);
          assert false
    in

    let compute_boltz_constr f bt z (_, ct2, w2) =
      let w = match w2 with Some x -> x | _ -> 1.0 in
      let ct = instconstr_constr ct2 bt in
      match ct with
      | None -> (0, w *. z, w)
      | Some ti when ti = bt -> (1, w *. z, w)
      | Some (Prod tl) ->
          List.fold_left
            (fun (i, fx, dx) tin ->
              if tin = bt then (i + 1, fx, dx)
              else
                let fx2, dx2 = f tin z in
                (i, fx *. fx2, (fx2 *. dx) +. (fx *. dx2)))
            (0, w *. z, w)
            tl
      | Some ti ->
          let fx, dx = f ti z in
          (0, w *. fx *. z, (w *. z *. dx) +. fx)
    in

    let eval_boltz btotal (i, x, _) = (btotal ** float i) *. x in

    {
      identifier = name;
      arguments = List.length args;
      gen_fun =
        (fun rs m f boltz bt z ->
          let p = Random.State.float rs 1.0 in
          let btotal, _ = boltz bt z in
          let const_weight =
            List.map
              (fun av ->
                let eb =
                  eval_boltz btotal (compute_boltz_constr boltz bt z av)
                in
                (av, eb /. btotal))
              ts_abstr
          in

          let rec findp pa = function
            | [] -> assert false
            | [ (t, _) ] -> t
            | (t, w) :: _ when pa +. w > p -> t
            | (_, w) :: q -> findp (pa +. w) q
          in

          let constr, ct, _ = findp 0.0 const_weight in
          let v, size =
            match instconstr_constr ct bt with
            | None -> ((constr, None), 1)
            | Some ti ->
                let vc, s = f rs m ti z in
                ((constr, Some vc), s)
          in
          (hide v bt, size));
      to_string =
        (fun f bt o ->
          match reveal o bt with
          | constr, None -> constr
          | constr, Some v -> (
              let type_content =
                instconstr_constr
                  (let _, x, _ =
                     List.find (fun (x, _, _) -> x = constr) ts_abstr
                   in
                   x)
                  bt
              in
              match type_content with
              | Some tl -> "(" ^ constr ^ "(" ^ f tl v ^ "))"
              | _ -> assert false ));
      string_of_named =
        (fun meml f bt ->
          match List.assoc_opt bt meml with
          | None ->
              let j = Printf.sprintf "p%i" !count in
              incr count;
              let s =
                List.fold_left
                  (fun acc (cn, cc, _) ->
                    match instconstr_constr cc bt with
                    | None -> Printf.sprintf "%s | %s -> \"%s\"" acc cn cn
                    | Some (Prod x) ->
                        let def, body =
                          print_prod_aux (f ((bt, j) :: meml)) 1 x
                        in
                        Printf.sprintf "%s | %s (%s) -> \"%s(\"^(%s)^\")\"" acc
                          cn def cn body
                    | Some x ->
                        Printf.sprintf "%s | %s ( x) -> \"%s(\"^(%s x)^\")\""
                          acc cn cn
                          (f ((bt, j) :: meml) x))
                  (Printf.sprintf "(fun v -> let rec %s = function " j)
                  ts_abstr
              in
              s ^ Printf.sprintf " in %s v)" j
          | Some recf -> recf);
      boltzman_fun =
        (fun f bt z ->
          let polysum =
            List.fold_left
              (fun acc vt -> sort_add_poly (compute_boltz_constr f bt z vt) acc)
              [ (0, 0.0, 0.0); (1, 0.0, 0.0) ]
              ts_abstr
          in
          let b, db =
            match polysum with
            | [] -> (0.0, 0.0) (*strange perhaps impossible *)
            | [ (0, fx, dx) ] -> (fx, dx)
            | [ (0, bx, dbx); (1, ax, dax) ] ->
                let bz = bx /. (1.0 -. ax) in
                (bz, ((dax *. bz) +. dbx) /. (1.0 -. ax))
            | [ (0, xc, dxc); (1, xbp, dxb); (2, xa, dxa) ] ->
                let xb = xbp -. 1.0 in
                let discr = (xb *. xb) -. (4.0 *. xa *. xc) in
                if discr < 0.0 then (max_float, max_float)
                else
                  let delta = sqrt discr in
                  let fx = (-.xb -. delta) /. (2.0 *. xa)
                  (* formal derivation by wolfram alpha *)
                  and dfx =
                    (((xb *. dxa) -. (xa *. dxb)) /. (2.0 *. xa *. xa))
                    +. (dxa *. delta /. (2.0 *. xa *. xa))
                    -. ( (-4.0 *. xc *. dxa)
                       -. (4.0 *. xa *. dxc)
                       +. (2.0 *. xb *. dxb) )
                       /. (4.0 *. xa *. delta)
                  in
                  (*Printf.eprintf "z:%g; poly:(%a) -> fx:%g dfx:%g\n" z print_poly polysum fx dfx; *)
                  (fx, dfx)
            | _ ->
                let ps2 = sort_add_poly (1, -1.0, 0.0) polysum in
                let f x = eval_poly x ps2 in
                (*let f x = xa*.x*.x +. xb*.x +.xc, 2.0*.xa*.x +. xb in *)
                let fxnewton =
                  newton_raphson_iterate ~bound:(0.0, max_float) f 1.0
                in
                (*let fxdicho = dichotomie (0.0,10000.0) f in *)
                let ct, multt = diff_in_z fxnewton polysum in
                let dfxnewton = ct /. (1.0 -. multt) in
                (*Printf.eprintf "z:%g; poly:(%a) -> fx:%g dfx:%g\n" z print_poly ps2 fxnewton dfxnewton;*)
                (fxnewton, dfxnewton)
          in
          if !verbose > 1 then
            Printf.printf
              "Computing Boltzman equation for '%s': 'B=%a' z:%g -> B=%g\n"
              (string_of_compo bt) print_poly polysum z b;
          if !verbose > 2 then
            List.iter
              (fun ((constr, _, _) as av) ->
                let eb = eval_boltz b (compute_boltz_constr f bt z av) in
                Printf.printf "Constr: %s G:%g P:%g\n" constr eb (eb /. b))
              ts_abstr;
          (b, db));
    }

let geom_law p u =
  if u < p then 0
  else 0 + (int_of_float @@ ceil @@ ((log (1.0 -. u) /. log (1.0 -. p)) -. 1.0))
