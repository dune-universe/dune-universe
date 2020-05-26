open Migrate_parsetree
open Ast_405

open Ppxx.Compilerlib
open Parsetree
open Asttypes
open Ast_mapper
open Longident

open Ppxx.Helper
open Ppxx.Utils
module Ppx = Ppxx.Ppx

let ($.) l s = Ldot(l,s)

let exp_of_position p =
  let open Lexing in
  let mklid s = at (Lident "Lexing" $. s)
  in
  let open Exp in
  record [
    mklid "pos_fname", string p.pos_fname;
    mklid "pos_lnum",  int p.pos_lnum;
    mklid "pos_bol",   int p.pos_bol;
    mklid "pos_cnum",  int p.pos_cnum
  ] None

let exp_of_location l =
  let open Location in
  let mklid s = at ~loc:(ghost l) (Lident "Ppx_test" $. "Location" $. s) in
  let open Exp in
  record ~loc:l [
    mklid "loc_start", exp_of_position l.loc_start;
    mklid "loc_end",   exp_of_position l.loc_end;
    mklid "loc_ghost", bool l.loc_ghost
  ] None

let rec exp_of_longident = 
  let open Exp in
  let mklid s = at (Lident "Ppx_test" $. "Longident" $. s) in
  function 
    | Lident s -> 
        construct (mklid "Lident") (Some (string s))
    | Ldot (t, s) ->
        construct (mklid "Ldot") (Some (tuple [exp_of_longident t; string s]))
    | Lapply (t1, t2) ->
        construct (mklid "Lapply") (Some (tuple [exp_of_longident t1; exp_of_longident t2]))



(* __FOR_PACKAGE__ special value *)
          
let extend_package super =
  let expr self e = match e with
    | { pexp_desc = Pexp_ident { txt= Lident "__FOR_PACKAGE__"; loc } } ->
        begin match !Clflags.for_package with
        | None ->
            let open Exp in
            constraint_ ~loc 
              (construct ~loc { txt = Lident "None"; loc } None)
              Typ.(constr { txt = Lident "option"; loc } [ constr { txt = Lident "string"; loc } [] ])
        | Some s ->
            Exp.construct ~loc {txt = Lident "Some"; loc } & Some (Exp.string s)
        end
    | _ -> super.expr self e
  in
  { super with expr }
    
let with_ref ref update f =
  let back = !ref in
  update ();
  let res = f () in
  ref := back;
  res
    
let current_structure_or_signature : [ `Sig of signature | `Str of structure ] option ref = ref None

let _with_current_structure_or_signature x =
  with_ref
    current_structure_or_signature
    (fun () -> current_structure_or_signature := Some x)
    
(* It lacks the top module name, which should be obtained from Location *)

module Current_module_path = struct
  let x = ref None
    
  let top_module () = 
    match !Location.input_name with
    | "//toplevel//" as s -> Lident s
    | fname ->
        let base = Filename.basename fname in
        let chopped = try Filename.chop_extension base with _ -> base in
        let mn = String.capitalize_ascii chopped in
        match !Clflags.for_package with
        | None -> Lident mn
        | Some p -> Ldot (Lident p, mn) 

  let set y = x := Some y
    
  let get () = match !x with
    | None ->
        let y = top_module () in
        x := Some y;
        y
    | Some y -> y

  let with_ x f =
    let back = get () in
    set x;
    let res = f () in
    set back;
    res
end

(* This may creates an attribute with illegal expression ex.:
    [@ppx_test_module_path Test.F(A)]
   where Text.F(A) : Path.t is illegal for an exp.

   If we keep it in the PPX output, it is rejected by OCaml 4.03, 
   so we must clean it.
*)
let attr_module_path mp =
  (at "ppx_test_module_path",
   PStr [ Str.mk (Pstr_eval (Exp.ident (at mp), [])) ]) 

let get_module_path_from_attr = function
  | ({ txt="ppx_test_module_path" }, 
     PStr [ {pstr_desc= Pstr_eval ({pexp_desc= Pexp_ident {txt}}, [])}]) -> Some txt
  | _ -> None

let get_module_path_from_attrs ats = 
  match
    List.partition_map (fun a -> 
      match get_module_path_from_attr a with
      | Some txt -> `Right txt
      | None -> `Left a) ats
  with
  | (rest, [x]) -> Some x, rest
  | (rest, [] ) -> None, rest
  | _ -> assert false

let rec annotate_module_expr mp mexp = 
  let add mexp = 
    with_loc mexp.pmod_loc & fun () ->
      { mexp with pmod_attributes = attr_module_path mp :: mexp.pmod_attributes } in
  match mexp.pmod_desc with
  | Pmod_structure _ -> add mexp
  | Pmod_constraint (mexp, mty) ->
      { mexp with pmod_desc = Pmod_constraint (annotate_module_expr mp mexp,
                                               annotate_module_type mp mty) }
  | Pmod_functor (({txt} as n), mtyo, mexp') -> 
      let mp = Lapply(mp, Lident txt) in
      { mexp with pmod_desc = Pmod_functor (n, mtyo, annotate_module_expr mp mexp') }
  | _ -> mexp

and annotate_module_type mp mty = 
  let add mty = { mty with pmty_attributes = attr_module_path mp :: mty.pmty_attributes } in
  match mty.pmty_desc with
  | Pmty_signature _ -> add mty
  | _ -> mty

let extend_module_path_tracking super =

  (* just list up named structures and signatures *)
  
  let module_binding self mb =
    let mp = Ldot (Current_module_path.get (), mb.pmb_name.txt) in
    let mb = { mb with pmb_expr = annotate_module_expr mp mb.pmb_expr } in
    Current_module_path.with_ mp & fun () ->
      super.module_binding self mb
  in
  let module_declaration self md =
    let mp = Ldot (Current_module_path.get (), md.pmd_name.txt) in
    let md = { md with pmd_type = annotate_module_type mp md.pmd_type } in
    Current_module_path.with_ mp & fun () ->
      super.module_declaration self md
  in

  let module_expr self mexp =
    match get_module_path_from_attrs mexp.pmod_attributes with
    | Some mp, rest -> 
        Current_module_path.with_ mp & fun () ->
          super.module_expr self {mexp with pmod_attributes = rest}
    | None, rest -> 
        let mp = Ldot (Current_module_path.get (), "_") in
        Current_module_path.with_ mp & fun () ->
          super.module_expr self {mexp with pmod_attributes = rest}
  in
  let module_type self mty =
    match get_module_path_from_attrs mty.pmty_attributes with
    | Some mp, rest -> 
        Current_module_path.with_ mp & fun () ->
          super.module_type self { mty with pmty_attributes = rest }
    | None, rest -> 
        let mp = Ldot (Current_module_path.get (), "_") in
        Current_module_path.with_ mp & fun () ->
          super.module_type self { mty with pmty_attributes = rest }
  in
  { super with module_binding; module_declaration; module_expr; module_type }

    
(*    [let %TEST name = e]    *)

type test_type = Unit | Bool | Fail

let drop_tests = ref false
let warn_dupes = ref false
let top_name = ref None

let rec lident_concat l1 = function
  | Lident s -> Ldot (l1, s)
  | Ldot (t, s) -> Ldot (lident_concat l1 t, s)
  | Lapply (t1, t2) -> Lapply (lident_concat l1 t1, t2)

let return_type lid =
  match lid with
  | Lident s | Ldot (_, s) ->
      if s.[String.length s - 1] = '_' then Unit
      else if String.(length s > 5 && sub s (length s - 5) 5 = "_fail") then Fail
      else Bool
  | _ -> Bool

let add_top_name lid =
  match !top_name with
  | None -> lid
  | Some top_lid -> lident_concat top_lid lid

module Tests = Set.Make(struct type t = Longident.t let compare = compare end)

let tests = ref Tests.empty

let add_test loc lid =
  (* Reject registeration of duped name *)
  let anon = function
    | Lident "_" -> true
    | Lident _ -> false
    | Ldot(_, "_") -> true
    | Ldot(_, _) -> false
    | Lapply _ -> assert false
  in
  if anon lid then ()
  else
    if Tests.mem lid !tests then begin
      if !warn_dupes then
        Ppxx.Utils.warnf "%a: ppx_test: Test name %s is already defined"
          Location.format loc
          (Format.sprintf "%a" Longident.format lid)
    end else tests := Tests.add lid !tests
    
let test_item self test = function
  | Pstr_eval (e, _attr) -> (* unnamed boolean test *)
      let name =
        let lid = Current_module_path.get () in
        let mpath = Ldot (lid, "_") in
        add_top_name & mpath
      in
      let test = "test" in
      (* Build [Ppx_test.test/test_ ~loc name (fun () -> e)] *)
      let open Exp in
      Str.value Nonrecursive [
        Vb.mk (Pat.unit ())
        & open_ (lid "PTest.TestTool")
        & apply 
          (ident & at & Lident "PTest" $. test)
          [ Nolabel, exp_of_location e.pexp_loc;
            Nolabel, exp_of_longident name;
            Nolabel, fun_ Nolabel None (Pat.unit ()) (self.expr self e) ]
      ]
      
  | Pstr_value (Nonrecursive, vbs) ->
      let f vb =
        let loc = vb.pvb_loc in
        let name = match vb.pvb_pat with
          | { ppat_desc = Ppat_any } -> 
              let lid = Current_module_path.get () in
              let mpath = Ldot (lid, "_") in
              add_top_name & mpath
          | { ppat_desc = Ppat_constant (Pconst_string (name, _)) } -> 
              let mpath = Current_module_path.get () in
              add_top_name & lident_concat mpath (Lident name)
          | { ppat_desc = Ppat_var {txt=name} } -> 
              let lid = Current_module_path.get () in
              let mpath = Ldot (lid, name) in
              add_top_name & mpath
          | { ppat_desc = Ppat_construct ({ txt= lid }, None) } ->
              let mpath = Current_module_path.get () in
              add_top_name & lident_concat mpath lid
          | _ -> assert false
        in
        (* Check duped test names *)
        add_test loc name;
        let return_type = match test with
          | "TEST_UNIT" -> Unit
          | "TEST" -> return_type name
          | "TEST_FAIL" -> Fail
          | _ -> assert false
        in
        let test = match return_type with
          | Unit -> "test_unit"
          | Bool -> "test"
          | Fail -> "test_fail"
        in
        (* Build [Ppx_test.test/test_ ~loc name (fun () -> e)] *)
        let open Exp in
        Vb.mk (Pat.unit ())
          (apply 
             (ident & at & Lident "PTest" $. test)
             [ Nolabel, exp_of_location loc;
               Nolabel, exp_of_longident name;
               Nolabel, fun_ Nolabel None (Pat.unit ()) (self.expr self vb.pvb_expr) ])
      in
      Str.value Nonrecursive (List.map f vbs)
  | _ -> assert false

let extend_let_test super =
  let structure_item self = function
    | { pstr_desc= Pstr_extension ( ( { txt = ("TEST" | "TEST_UNIT" | "TEST_FAIL") }, _ ), _) } when !drop_tests -> []

    (* let %TEST p = e   or   [%%TEST ...] *)           
    | { pstr_desc= 
          Pstr_extension (
            ( { txt = ("TEST" | "TEST_UNIT" | "TEST_FAIL") }, _ ),
            _) } as sitem when !drop_tests ->
        [ { sitem with pstr_desc= Pstr_eval (Exp.unit (), []) } ]

    (* let %TEST p = e 
       or [%%TEST let p = e] 
    *)           
    | { pstr_desc= 
          Pstr_extension (
            ( { txt = ("TEST" | "TEST_UNIT" | "TEST_FAIL" as test) }, 
              PStr [ { pstr_desc= (Pstr_value (Nonrecursive, _vbs) as desc)} ]),
            _) } ->
        [ test_item self test desc ]
          
    (* [%%TEST ...], more general case 
    *)           
    | { pstr_desc= 
          Pstr_extension (
            ( { txt = ("TEST" as test) },
              PStr sitems),
            _) } ->
        List.map (fun sitem -> test_item self test sitem.pstr_desc) sitems

    | x -> [ super.structure_item self x ]
  in
  let structure self sitems = List.concat & List.map (structure_item self) sitems
(*
    let cp = Current_module_path.get () in
    (* CR jfuruse: BUG: This is not good. Internal anonymous structure consume the doctest. Actually doctest comment is PStr therefore it consumes itself! *)
    let matched, non_matched = List.partition (fun (cp',_tests) -> cp = cp') !doctests in
    doctests := non_matched;
    let tests = List.(concat_map snd & rev matched) in
    let tests = List.map (fun (loc, s) ->
      (* CR jfuruse: need to incremtn loc_start *)
      let lexbuf = Doctest.Lexing.from_string_with_position s loc.Location.loc_start in
      Lexer.init ();
      let e = Parser.parse_expression Lexer.token lexbuf in
      let open Exp in
      let name = None in
      Str.value Nonrecursive [
        Vb.mk Pat.unit           
          (apply 
             (ident & at & Lident "PTest" $. "test")
             [ "", exp_of_location e.pexp_loc;
               "", option & Option.map exp_of_longident name;
               "", fun_ "" None Pat.unit (self.expr self e) ])
      ]
    ) tests
    in
    is @ tests
*)
  in

  { super with structure; (* signature; *) }

let make_mapper () =
  current_structure_or_signature := None;
  Current_module_path.x := None; (* filled lazily when Location.input_name is set *)
  tests := Tests.empty; (* no trans module border test name checks *)

  extend_let_test
  & extend_module_path_tracking
  & extend_package default_mapper

let parse_as_lident s = match Longident.parse s with
  | Lident "" -> None
  | l -> Some l

let opts =
  [ "-drop-tests", Arg.Set drop_tests, "Drop tests"
    
  ; "-warn-dupes", Arg.Set warn_dupes, "Warn tests with the same names"
  ; "-top-name",
    Arg.String (fun s ->
      match parse_as_lident s with
      | None -> raise_errorf "-top-name must take a valid module path"
      | Some l -> top_name := Some l),
    "Set the top module name" 
  ]

include Ppxx.Ppx.Make(struct
  let name = "ppx_test"
  let options = opts
  let make_mapper _config _cookies = make_mapper ()
end)

let () = register ()
