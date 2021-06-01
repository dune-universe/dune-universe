open Type
open Math

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
      | None -> (0, w *. z)
      (* | Some (Prod tl) ->
           List.fold_left
             (fun (i, fx) tin ->
               if tin = bt then (i + 1, fx)
               else
                 let fx2, _ = f tin z in
                 (i, fx *. fx2))
             (0, w *. z)
             tl*)
      | Some ti ->
          let fx, _ = f ti z in
          (0, w *. fx *. z)
    in

    (* hack for list *)
    let rename_type = function
      | Name ("list", [ arg ]) -> Name ("m__list", [ arg ])
      | x -> x
    in

    let compute_boltz_constr_eq f bt acc (_, ct2, w2) =
      let w = match w2 with Some x -> x | _ -> 1.0 in
      let ct = instconstr_constr ct2 bt in
      match ct with
      | None -> ((([], 1), w), acc)
      | Some ti when ti = bt -> ((([ rename_type ti ], 1), w), acc)
      | Some (Prod tl) ->
          let tl2 = flatten_prod tl in
          List.fold_left
            (fun (((i, n), fx), acc2) tin ->
              if tin = bt then (((tin :: i, n), fx), acc2)
              else
                let acc3 = f tin acc2 in
                (((rename_type tin :: i, n), fx), acc3))
            ((([], 1), w), acc)
            tl2
      | Some ti ->
          let acc2 = f ti acc in
          ((([ rename_type ti ], 1), w), acc2)
    in

    let eval_boltz btotal (i, x) = (btotal ** float i) *. x in

    {
      identifier = name;
      boltz_identifier = name;
      arguments = List.length args;
      is_simple = false;
      get_equ =
        (fun f accrec bt ->
          if List.exists (fun (x, _) -> x = bt) accrec then accrec
          else
            let accrec2 = (bt, []) :: accrec in
            let polysum, acc2 =
              List.fold_left
                (fun (acc_poly, acc_other) vt ->
                  let p, acc_other2 =
                    compute_boltz_constr_eq f bt acc_other vt
                  in
                  (sort_add_poly p acc_poly, acc_other2))
                ([], accrec2) ts_abstr
            in
            List.map
              (fun (x, v) -> if x = bt then (bt, polysum) else (x, v))
              acc2);
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
      print =
        (fun f bt form o ->
          match reveal o bt with
          | constr, None -> Format.pp_print_string form constr
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
              | Some tl -> Format.fprintf form "(%s(%a))" constr (f tl) v
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
          match bt with
          | Name (_, l) ->
              let bt2 = Name (name, l) in
              f bt2 z
          (*| Name ("list", l) ->
              Printf.printf "translate\n";
              f (Name ("m__list", l)) z*)
          | _ ->
              failwith
                (Format.sprintf "%f -> %i should be memoize" z
                   (Hashtbl.hash (bt, z))))
        (*let polysum =
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
          (b, db));*);
    }

let print_t f t = Format.fprintf f "B(%a)" (fun f _ -> pp_compo f t) t

let print_poly_b f l =
  let open Format in
  if l = [] then fprintf f "0"
  else
    List.iter
      (fun ((tl, n), a) ->
        fprintf f "+";
        if a <> 1.0 then fprintf f "%g" a;
        if n = 1 then fprintf f "z";
        if n > 1 then fprintf f "z^%i" n;
        match tl with
        | [] -> ()
        | [ t ] -> fprintf f "%a" print_t t
        | l -> List.iter (fun t -> print_t f t) l)
      l

let print_equations f eqs =
  Array.iter
    (fun (t, p) ->
      Format.fprintf f "%a = " print_t t;
      print_poly_b f p;
      print_newline ())
    eqs

let instantiate_in_z f z eqs =
  (*iter on equation*)
  List.map
    (fun (bt, p) ->
      (* iter on monome *)
      let p2 =
        List.fold_left
          (fun acc ((tl, n), x) ->
            (* iter on type *)
            let m =
              List.fold_left
                (fun (acc2, x2) t ->
                  match f t z with
                  | None -> (t :: acc2, x2)
                  | Some (tx, _) ->
                      (* need to derive the product *)
                      (acc2, x2 *. tx))
                ([], x *. (z ** float n))
                tl
            in
            Math.sort_add_poly m acc)
          [] p
      in
      (bt, p2))
    eqs

let diff_z_product f z xB (tl, n) w =
  let nf = float n in
  List.fold_left
    (fun (pacc, ca) t ->
      match f t z with
      | None ->
          (* the type is a sum type to be solve *)
          let c = List.assoc t xB in
          (sort_add_poly ([ t ], ca) (mult_poly c pacc), c *. ca)
      | Some (x, dx) ->
          (* the type is a simple type of weight x *)
          (sort_add_poly ([], dx *. ca) (mult_poly x pacc), x *. ca))
    ([ ([], w *. nf *. (z ** (nf -. 1.0))) ], w *. (z ** nf))
    tl

let instantiate_in_diff_z f z eqs xB =
  (*iter on equation*)
  List.map
    (fun (bt, p) ->
      (* iter on monome *)
      let p2 =
        List.fold_left
          (fun acc (term, p) ->
            (* iter on type *)
            let df, _ = diff_z_product f z xB term p in
            add_poly acc df)
          [] p
      in
      (bt, p2))
    eqs

let diff_in_type p t =
  p
  |> List.map (fun (tl, c) ->
         ( List.fold_left
             (fun (n, acc) t2 ->
               if t2 = t && n = 0 then (1, acc)
               else if t2 = t then (n + 1, t :: acc)
               else (n, t2 :: acc))
             (0, []) tl,
           c ))
  |> List.filter (fun ((n, _), _) -> n > 0)
  |> List.map (fun ((n, tl), c) -> (tl, float n *. c))

let map_of_equations eqs =
  snd
  @@ Array.fold_left (fun (i, l) (eq, _) -> (i + 1, (eq, i) :: l)) (0, []) eqs

let ap_poly map p x =
  List.fold_left
    (fun acc (tl, v) ->
      acc
      +. v
         *. List.fold_left (fun accm ct -> accm *. x.(List.assoc ct map)) 1.0 tl)
    0.0 p

let compute_jacobian eqs =
  let n = Array.length eqs in
  let jac = Array.make_matrix n n [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let td = fst eqs.(j) in
      jac.(i).(j) <- diff_in_type (snd eqs.(i)) td
      (*if !verbose > 4 then
        Printf.printf "d%a/d%a = %a\n" print_t
          (fst eqs.(i))
          print_t td print_poly_b
          jac.(i).(j)*)
    done
  done;
  jac

let comp_dx eqs jac map x =
  let jacx = Array.map (fun a -> Array.map (fun b -> ap_poly map b x) a) jac in
  let fx = Array.map (fun (_, p) -> ap_poly map p x) eqs in
  let dx = Math.solve jacx (Array.map ( ~-. ) fx) in
  if !verbose > 4 then (
    Printf.printf "jac:[%a]\n" Math.print_mat jacx;
    (*print_equations stdout eqs;*)
    Printf.printf "x\tfx\tdx[\n%a]\n" Math.print_vec_list [ x; fx; dx ] );
  (fx, dx)

let solve_eq equations z =
  (*if !verbose > 4 then print_equations stdout equations;*)
  let jac = compute_jacobian equations in
  let map = map_of_equations equations in
  let xB =
    Math.newton_raphson_multivariate
      (comp_dx equations jac map)
      (Array.make (Array.length jac) z)
  in
  List.map (fun (bt, i) -> (bt, xB.(i))) map

let compute_b_sum f eqrec z =
  let equations =
    instantiate_in_z f z eqrec
    (*|> List.fold_left
         (fun acc (bt, p) -> ((bt, false), p) :: ((bt, true), p) :: acc)
         []*)
    |> List.map (fun (bt, p) -> (bt, ([ bt ], -1.0) :: p))
    |> Array.of_list
  in
  solve_eq equations z

let compute_b_diff f eqrec z xB =
  let equations =
    instantiate_in_diff_z f z eqrec xB
    (*|> List.fold_left
      (fun acc (bt, p) -> ((bt, false), p) :: ((bt, true), p) :: acc)
      []*)
    |> List.map (fun (bt, p) -> (bt, ([ bt ], -1.0) :: p))
    |> Array.of_list
  in
  solve_eq equations z

let compute_size f eq z =
  try
    let xB = compute_b_sum f eq z in
    if !verbose > 2 then
      List.iter (fun (ct, x) -> Format.printf "%a = %g@." print_t ct x) xB;

    (*let x, dx = boltzman_from_compo intype z in
      if x > 1.0e+100 then (nan, 0.0) else (z *. dx /. x, 0.0)*)
    if List.exists (fun (_, v) -> v <= 0.0 || classify_float v <> FP_normal) xB
    then None
    else
      let dxB = compute_b_diff f eq z xB in
      Some
        (List.map2
           (fun (t, v) (t2, v2) ->
             assert (t = t2);
             (t, v, v2))
           xB dxB)
  with Math.Matrix_singular -> None
