(* Test conversions from and to sequences. *)

let point x y = Curve_sampling.P2.Point (Gg.P2.v x y)

let () =
  let s = Curve_sampling.of_path [(1.,1.); (2.,2.); (3., nan); (4.,4.)] in
  let out = [[(1.,1.); (2.,2.)]; [(4.,4.)]] in
  let out_p2 = [point 1. 1.; point 2. 2.; Curve_sampling.P2.Cut;
                point 4. 4.] in
  assert(Curve_sampling.to_list s = out);
  assert(Curve_sampling.P2.to_list s = out_p2);
#if OCAML_VERSION >= (4, 7, 0)
  assert(let l = List.of_seq (Curve_sampling.to_seq s) in
         List.map List.of_seq l = out);
  assert(List.of_seq (Curve_sampling.P2.to_seq s) = out_p2)
#endif
;;

let () =
  let s = Curve_sampling.of_path [(1.,nan); (2.,2.); (3., 3.); (nan,4.)] in
  let out = [ [(2.,2.); (3.,3.)] ] in
  let out_p2 = [point 2. 2.; point 3. 3.] in
  assert(Curve_sampling.to_list s = out);
  assert(Curve_sampling.P2.to_list s = out_p2);
#if OCAML_VERSION >= (4, 7, 0)
  assert(let l = List.of_seq (Curve_sampling.to_seq s) in
         List.map List.of_seq l = out);
  assert(List.of_seq (Curve_sampling.P2.to_seq s) = out_p2)
#endif
;;

let () =
  let s = Curve_sampling.of_path
            [(1.,nan); (2.,2.); (3., 3.); (nan,4.); (5., nan);
             (6., 6.); (nan, 7.)] in
  let out = [ [(2.,2.); (3.,3.)]; [(6., 6.)] ] in
  let out_p2 = [point 2. 2.; point 3. 3.; Curve_sampling.P2.Cut;
                point 6. 6.] in
  assert(Curve_sampling.to_list s = out);
  assert(Curve_sampling.P2.to_list s = out_p2);
#if OCAML_VERSION >= (4, 7, 0)
  assert(let l = List.of_seq (Curve_sampling.to_seq s) in
         List.map List.of_seq l = out);
  assert(List.of_seq (Curve_sampling.P2.to_seq s) = out_p2)
#endif
;;
