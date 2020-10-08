(* Opening GT *)
(* open GT *)

(* Some useful idioms: identity, fixpoint *)
let id x = x
let fix = GT.fix0;;

(* The first example: decorated expressions *)
@type 'expr a_expr =
| Const of GT.int
| Add   of 'expr * 'expr with show, gmap, foldl

(* Must replace with @type expr = expr a_expr with ... *)
@type expr = expr a_expr with show;;

@type 'a decorated = ('a decorated a_expr, 'a) GT.pair with show;;

let show_decorated e = GT.show(decorated) e

(* Simple --- one-level --- decoration *)
module Simple =
  struct

    (* Decoration *)
    let decorate : (unit -> 'a) -> expr -> 'a decorated = fun fd e ->
      fix (fun s e -> let d = fd () in GT.gmap(a_expr) s e, d) e

    (* Stripping *)
    let strip : 'a decorated -> expr = fun e ->
      fix (fun s e -> GT.gmap(a_expr) s (fst e)) e

    (* Testing *)
    let _ =
      Printf.printf "\n";
      let e = Add (Add (Const 1, Const 2), Const 3) in
      Printf.printf "%s\n" (show_expr e);
      let e = decorate (let i = ref 0 in fun () -> let n = !i in incr i; n) e in
      Printf.printf "%s\n" (show_decorated string_of_int e);
      Printf.printf "%s\n" (show_expr @@ strip e)

  end

(* Advanced --- multilayer decorations *)
module Advanced =
  struct

    (* Lifting --- attaching unit decoration *)
    let lift = Simple.decorate (fun _ -> ())

    (* Redecorate *)
    let redecorate : ('a -> 'b) -> 'a decorated -> 'b decorated = fun ab e ->
      fix (fun s (e, a) -> GT.gmap(a_expr) s e, ab a) e

    (* Testing *)
    let _ =

      let e = Add (Add (Const 1, Const 2), Const 3) in
      Printf.printf "%s\n" (show_expr e);
      let e = lift e in
      Printf.printf "%s\n" (show_decorated (fun _ -> "()") e);
      let e = redecorate (let i = ref 0 in fun () -> let n = !i in incr i; n) e in
      Printf.printf "%s\n" (show_decorated (GT.show GT.int) e);
      let e = redecorate (let i = ref 0 in fun a -> let n = !i in incr i; (a, n)) e in
      Printf.printf "%s\n" @@
      show_decorated (GT.show(GT.pair) (GT.show GT.int) (GT.show GT.int)) e;
      let e = redecorate snd e in
      Printf.printf "%s\n" (show_decorated (GT.show GT.int) e)

  end

(* Custom --- no use of gmap *)
module Custom =
  struct
    let redecorate : 'inh -> ('a -> 'inh -> 'c * 'inh) -> 'a decorated -> 'c decorated =
      fun init f expr ->
      let tr = object(self_p)
        inherit [ 'a decorated a_expr, 'inh, 'c decorated a_expr * 'inh
                , 'a, 'inh, 'c
                , 'inh, 'c decorated * 'inh, _
                ] GT.class_pair

        method c_Pair c0 l r =
          let (l, c1) = GT.transform a_expr (object
              method c_Const c0 n : ('c decorated a_expr * 'inh) = (Const n, c0)
              method c_Add   c0 l r =
                let (l, c1) = GT.transform (decorated) self_p c0 l in
                let (r, c2) = GT.transform (decorated) self_p c1 r in
                (Add (l,r), c2)
            end)
              c0
              l
          in
          let (new_decor,new_inh) = f r c1 in
          ( (l, new_decor), new_inh)
      end in
      GT.transform (decorated) tr init expr |> fst


    (* let (_:int) = redecorate *)
    let () =
      Printf.printf "\n";
      let e = Add (Add (Const 1, Const 2), Const 3) in
      Printf.printf "%s\n" (show_expr e);
      let e2 = Simple.decorate (fun () -> ()) e in
      Printf.printf "%s\n" (show_decorated (fun _ -> "<unit>") e2);

      let e3 = redecorate 10 (fun () n -> n,n+1) e2 in
      Printf.printf "%s\n" (show_decorated (GT.show GT.int) e3);
      ()

  end
