let id x = x
let fix = GT.fix0;;

(* The first example: decorated expressions *)
@type 'expr a_expr =
| Const of GT.int
| Add   of 'expr * 'expr with show, gmap, foldl

@type expr = expr a_expr with show;;
@type 'a decorated = ('a decorated a_expr, 'a) GT.pair with show;;

let show_decorated e = GT.show(decorated) e

let fix f inh t =
  let knot = ref (fun _ -> assert false) in
  let recurse inh t = f !knot inh t in
  knot := recurse;
  recurse inh t

(* Simple --- one-level --- decoration *)
module Simple =
  struct
    (* let (_:int) = GT.transform_gc gcata_a_expr (new gmap_a_expr_t (GT.lift fst)) () *)

    (* Stripping *)
    let strip :  'a decorated -> expr = fun e ->
      fix (fun fself () e -> GT.gmap(a_expr) fself (fst e)) () e

    (* Decoration *)
    let decorate : (unit -> 'a) -> expr -> 'a decorated = fun fd e ->
      fix (fun fself () e -> let d = fd () in GT.gmap(a_expr) fself e, d) () e


    (* Testing *)
    let _ =
      Printf.printf "\n";
      let e = Add (Add (Const 1, Const 2), Const 3) in
      Printf.printf "%s\n" (GT.show expr e);
      let e = decorate (let i = ref 0 in fun () -> let n = !i in incr i; n) e in
      Printf.printf "%s\n" (show_decorated (GT.lift string_of_int) e);
      Printf.printf "%s\n" (GT.show expr @@ strip e)

  end

(* Advanced --- multilayer decorations *)
module Advanced =
  struct

    (* Lifting --- attaching unit decoration *)
    let lift = Simple.decorate (fun _ -> ())

    (* Redecorate *)
    let redecorate : ('a -> 'b) -> 'a decorated -> 'b decorated = fun ab e ->
      fix (fun s () (e, a) -> GT.gmap(a_expr) s e, ab a) () e

    (* Testing *)
    let _ =

      let e = Add (Add (Const 1, Const 2), Const 3) in
      Printf.printf "%s\n" (show_expr e);
      let e = lift e in
      Printf.printf "%s\n" (show_decorated (fun () _ -> "()") e);
      let e = redecorate (let i = ref 0 in fun () -> let n = !i in incr i; n) e in
      Printf.printf "%s\n" (show_decorated (GT.lift @@ GT.show GT.int) e);
      let e = redecorate (let i = ref 0 in fun a -> let n = !i in incr i; (a, n)) e in
      Printf.printf "%s\n" @@
      show_decorated GT.(lift @@ show(pair) (lift @@ show int) (lift @@ show int)) e;
      let e = redecorate snd e in
      Printf.printf "%s\n" (show_decorated GT.(lift @@ show int) e)

  end

(* Custom --- no use of gmap *)
module Custom = struct
  let redecorate : 'inh -> ('a -> 'inh -> 'c * 'inh) -> 'a decorated -> 'c decorated =
    fun init f expr ->
      (* TODO: we have issues here because we can't create proper*)
      let tr = object(self)
        inherit [ 'inh, 'a decorated a_expr, 'c decorated a_expr * 'inh
                , 'inh,                  'a,                         'c
                , 'inh, 'c decorated * 'inh, 'c decorated * 'inh
                ] GT.pair_t

        method c_Pair c0 _ l r =
          let (l, c1) =
            GT.transform a_expr
              (fun _ -> object
                method c_Const c0 _ n : ('c decorated a_expr * 'inh) = (Const n, c0)
                method c_Add   c0 _ l r =
                  let (l2, c1) = decorated.GT.gcata self c0 l in
                  let (r2, c2) = decorated.GT.gcata self c1 r in
                  (Add (l2,r2), c2)
              end)
              c0
              l
          in
          let (new_decor,new_inh) = f r c1 in
          ( (l, new_decor), new_inh)
      end in
      decorated.GT.gcata tr init expr |> fst

    let () =
      Printf.printf "\n";
      let e = Add (Add (Const 1, Const 2), Const 3) in
      Printf.printf "%s\n" (show_expr e);
      let e2 = Simple.decorate (fun () -> ()) e in
      Printf.printf "%s\n" (show_decorated (fun () _ -> "<unit>") e2);

      let e3 = redecorate 10 (fun () n -> n,n+1) e2 in
      Printf.printf "%s\n" (show_decorated GT.(lift @@ show int) e3);
      ()

  end
