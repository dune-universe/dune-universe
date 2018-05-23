open Ast_405
open Ast_mapper
open Ppxx.Utils
open Ppxx.Helper
open Ppxx.Compilerlib
open Parsetree
open Longident
open Location

let tbl = Hashtbl.create 17

let mk_variable = Printf.sprintf "integer_%s%c"

let mk_declaration s c =
  Str.value Nonrecursive 
    [ Vb.mk (Pat.var' & mk_variable s c)
        (Exp.apply (Exp.id & Printf.sprintf "Ppx_integer._%c" c)
          [Nolabel, Exp.string s])
    ]

let structure_top = ref true

type error = string

exception Error of Location.t * string

let () =
  Location.register_error_of_exn & function
    | Error (loc, msg) ->
        Some {loc; msg; sub=[]; if_highlight= msg }
    | _ -> None

let insert_declarations sitems =
  let inserted = ref false in
  let sitems' = 
    List.concat (List.map (fun sitem ->
        match sitem.pstr_desc with
        | Pstr_extension (({txt="ppx_integer"},_), _) when !inserted ->
            raise (Error (sitem.pstr_loc, "[%%ppx_integer] can be used at most once."))
        | Pstr_extension (({txt="ppx_integer"}, _), _) ->
            inserted := true;
            Hashtbl.fold (fun _ v vs -> v::vs) tbl []
        | _ -> [sitem]) sitems)
  in
  if not !inserted then 
    Hashtbl.fold (fun _ v vs -> v::vs) tbl sitems'
  else sitems'

let reset () =
  structure_top := true;
  Hashtbl.clear tbl
  
let extend super =
  let expr self e = match e.pexp_desc with
    | Pexp_constant (Pconst_integer (_, Some ('l'|'L'|'n'))) -> super.expr self e
    | Pexp_constant (Pconst_integer (s, Some c)) -> 
        (* let integer_sx = Ppx_integer._x s *)
        with_gloc e.pexp_loc & fun () ->
          let v = mk_variable s c in
          if not & Hashtbl.mem tbl v then begin
            Hashtbl.add tbl v 
            & with_gloc e.pexp_loc & fun () -> mk_declaration s c
          end;
          Exp.var v    
    | _ -> super.expr self e
  in
  let structure self sitems =
    let do_insert = !structure_top in
    structure_top := false;
    let sitems = super.structure self sitems in
    let no_insert sitems = 
      List.iter (fun sitem -> match sitem.pstr_desc with
          | Pstr_extension (({txt="ppx_integer"},_), _) ->
              raise (Error (sitem.pstr_loc, "[%%ppx_integer] must appear at the toplevel."))
          | _ -> ()) sitems;
      sitems
    in
    if do_insert then insert_declarations sitems 
    else no_insert sitems
  in
  { super with expr; structure }

