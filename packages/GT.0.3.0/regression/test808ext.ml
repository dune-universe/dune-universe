let unused _ _ = failwith "*** Using the unused ***"

module E = struct
  type ('a,'b,'c) expr = ..
  [@@deriving gt ~options:{show; eval}]
  (* TODO: support whildcards here *)

    (* class virtual ['ia,'a,'sa,'ib,'b,'sb,'ic,'c,'sc,'inh,'self,'syn] expr_t =
     *   object  end
     * let gcata_expr tr inh subj = assert false *)

end

module Lam = struct
  type ('name_abs, 'name, 'lam) E.expr += App of 'lam * 'lam | Var of 'name
  [@@deriving gt ~options:{show; eval}]

  class ['me, 'me2] de_bruijn fuse_var fterm = object
    inherit [string, unit, string, int, 'me, 'me2, string list, 'me2] eval_expr_t
        unused (fun _ (_: string) -> ()) fuse_var fterm
  end
end

module Abs = struct
  type ('name_a, 'name, 'lam) E.expr += Abs of 'name_a * 'lam
  [@@deriving gt ~options:{show; eval}]

  class ['me, 'me2] de_bruijn fuse_var ft = object
    inherit [ 'inh, 'name_a, 'name_a2
            , 'inh, 'name, 'name2
            , 'inh, 'lam, 'lam2
            , 'inh, 'self, ('name_a2, 'name2, 'lam2) E.expr
            ] expr_t
    inherit ['me, 'me2] Lam.de_bruijn fuse_var ft
    constraint 'name2 = int
    method c_Abs env name term = Abs ((), ft (name :: env) term)
  end
end

module Let = struct
  type ('name_a, 'name, 'lam) E.expr += Let of 'name_a * 'lam * 'lam
  [@@deriving gt ~options:{show; eval}]

  class ['me, 'me'] de_bruijn fuse_var ft = object
    inherit [ string list, string, unit
            , string list, string, int
            , string list, 'me, 'me'
            , string list, 'me, 'me'] expr_t
    inherit ['me, 'me'] Abs.de_bruijn fuse_var ft
    method c_Let env (name: string) bnd term =
      Let ((), ft env bnd, ft (name :: env) term)
  end
end

module LetRec = struct
  type ('name_a, 'name, 'lam) E.expr += LetRec of 'name_a * 'lam * 'lam
  [@@deriving gt ~options:{show; eval}]

  class ['me, 'me'] de_bruijn fuse_var ft = object
    inherit [ string list, string, unit
            , string list, string, int
            , string list, 'me, 'me'
            , string list, 'me, 'me'] expr_t
    inherit ['me, 'me'] Let.de_bruijn fuse_var ft
    method c_LetRec env name bnd term =
      let env2 = name::env in
      LetRec ((), ft env2 bnd, ft env2 term)
  end
end


(* *)
let fix f inh t =
  let knot = ref (fun _ -> assert false) in
  let recurse inh t = f !knot inh t in
  knot := recurse;
  recurse inh t

let ith m n = fix (fun me i -> function
                   | []                 -> raise Not_found
                   | x :: tl when x = n -> i
                   | _ :: tl            -> me (i+1) tl
  ) 0 m


type ('var_abs, 'n, 'b) t = ('var_abs, 'n, ('var_abs, 'n, 'b) t) E.expr
let show_t fname fterm lam =
  let rec helper lam =
    Let.gcata_expr (new Let.show_expr_t helper fname fname helper) () lam
  in
  helper lam

type named = (string, string, string) t
type nameless = (unit, int, unit) t

let show_string = Printf.sprintf "%S"
let show_int = Printf.sprintf "%d"
let show_unit _ = Printf.sprintf "()"

let show_named (t: named) =
  GT.fix0 (fun fself ->
      LetRec.gcata_expr (new LetRec.show_expr_t fself show_string show_string fself) ()
    ) t
let show_nameless (t: nameless) =
  GT.fix0 (fun fself ->
      LetRec.gcata_expr (new LetRec.show_expr_t fself show_unit show_int fself) ()
    ) t

(* to de Bruijn *)
let convert (lam: named) : nameless =
  let rec helper env lam =
      LetRec.gcata_expr (new LetRec.de_bruijn ith helper) env lam
  in
  helper [] lam

let () =
  let l : named =
    let open Lam in let open Abs in
    App (Abs ("x", Var "x"), Abs ("y", Var "y"))
  in
  let l2 : named =
    let open Lam in let open Abs in let open Let in
    Let ("z", Abs ("x", Var "x"), Abs ("x", Abs ("y", App (Var "x", Var "z"))))
  in
  let l3 =
    let open Lam in let open Abs in let open Let in  let open LetRec in
    LetRec ("z", App (Abs ("x", Var "x"), Var "z"), Abs ("x", Abs ("y", App (Var "x", Var "z"))))
  in
  (* Original: App (Abs ("x", Var ("x")), Abs ("y", Var ("y"))) *)
  Printf.printf "Original: %s\n" (show_named l);
  (* Converted: App (Abs ((), Var (0)), Abs ((), Var (0))) *)
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l);
  (* Converted: Let ((), Abs ((), Var (0)), Abs ((), Abs ((), App (Var (1), Var (2))))) *)
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l2);
  (* Converted: Let ((), App (Abs ((), Var (0)), Var (0)), Abs ((), Abs ((), App (Var (1), Var (2))))) *)
  Printf.printf "Converted: %s\n" (show_nameless @@ convert l3);
  ()
