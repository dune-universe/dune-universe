open Migrate_parsetree
open Ast_405

open Ppxx.Utils
open Ppxx.Compilerlib
open Ppxx.Helper
open Ast_mapper
open Parsetree
open Longident

let hash = Btype.hash_variant

let poly_record = ref []

let with_poly_record b f =
  poly_record := b :: !poly_record;
  let res = f () in
  poly_record := List.tl !poly_record;
  res

let is_poly_record () = match !poly_record with
  | [] -> false
  | b::_ -> b

let poly_record x = lid & "Ppx_poly_record.Poly_record." ^ x

let poly_record_internal x = lid & "Ppx_poly_record.Poly_record.Internal." ^ x

let error_field_with_module loc =
  raise_errorf ~loc "Polymorphic record fields cannot take module names"

let check_attrs_empty loc = function
  | [] -> ()
  | _ -> raise_errorf ~loc "This extension cannot have structure item attributes [@@...]"

let error_illegal_field loc =
  raise_errorf ~loc "Polymorphic record field must be a simple label"

let check_fields fs =
  let uniq_dup_by eq fs = 
    let rec loop firsts dups = function
      | [] -> List.rev firsts, List.rev dups
      | x::xs -> 
          let firsts', dups' = 
            match List.find (eq x) firsts with
            | found -> firsts, (x,found)::dups 
            | exception Not_found -> x::firsts, dups
          in
          loop firsts' dups' xs
    in
    loop [] [] fs
  in
  let _firsts, dups = uniq_dup_by (fun (_,_,l,_,_) (_,_,l',_,_) -> l = l') fs in
  begin match dups with
  | [] -> ()
  | ((_,_,l,_,e),_) :: _ -> raise_errorf ~loc:e.pexp_loc "Duplicated label %s" l
  end;
  let _firsts, dups = uniq_dup_by (fun (_,_,_,h,_) (_,_,_,h',_) -> h = h') fs in
  begin match dups with
  | [] -> ()
  | ((_,_,l,_,e),(_,_,l',_,_)) :: _ ->
      raise_errorf ~loc:e.pexp_loc "Label %s cannot coexist with %s sharing the same hash" l l'
  end

let handle_poly_pexp_record e fields eopt =
  (*
    { l1 = v1; l2 = v2 }

    (* binds *)
    let v1 = $e1 in
    let v2 = $e2 in

    (* enforce *)
    if false then 
      ignore (fun (o : 'a) -> 
        ignore (o#$l1 = v1);
        ignore (o#$l2 = v2)
      );
    
    (Poly_record.copy_with [| ($l1_hash, Obj.repr $v1); ... |] (None | Some (e : 'a Poly_record.t)) : 'a Poly_record.t)
  *)
  let open Exp in
  with_loc e.pexp_loc & fun () ->
    let fields = List.mapi (fun i ({txt; loc},e) ->
      match txt with
      | Longident.Lident l -> 
          let name = "v" ^ string_of_int i in
          (Pat.var' ~loc:e.pexp_loc name,
           Exp.var ~loc:e.pexp_loc name,
           l, 
           int ~loc:loc (hash l), 
           e)
      | _ -> error_field_with_module loc) fields
    in
    check_fields fields;
    let binds e =
      let_ Nonrecursive 
        (List.map (fun (p, _, _, _, e) -> Vb.mk p e) fields)
        e
    in
    let tvar = Typ.new_var & Some "object" in
    let tvar_poly_record_t = Typ.constr (poly_record "t") [tvar] in
    let enforce =
      (* if false then 
           (fun (o : 'a) -> 
             ignore (o#$l1 = v1);
             ignore (o#$l2 = v2)
           ) (assert false);
      *)
      ifthenelse
        (bool false) 
        (let ov,op = ExpPat.var "o" in
         apply
           (fun_ Nolabel None 
              (Pat.(constraint_ op tvar))
              (seqs 
               & List.map (fun (_, v, l, _, _) -> 
                 ignore_
                 & apply (id "=")
                   [ Nolabel, send ov !@l 
                   ; Nolabel, v ])
                 fields))
           [Nolabel, assert_false ()]
        )
        None
    in
    let array = 
      array ~loc:e.pexp_loc ~attrs:e.pexp_attributes
      & List.map (fun (_,_,_,hash,e) -> 
        tuple ~loc:hash.pexp_loc 
          [hash; 
           apply (id "Obj.repr") [Nolabel, e]]) fields 
    in
    let eopt' = match eopt with
      | None -> None
      | Some e -> Some (constraint_ e tvar_poly_record_t)
    in
    let copy_with =
      constraint_
        (apply (ident (poly_record_internal "copy_with"))
           [ Nolabel, array
           ; Nolabel, option eopt' ])
        tvar_poly_record_t
    in
    binds (sequence enforce copy_with)
    
let handle_poly_pexp_field e e' l loc' =
  (* 
     (Poly_record.get ($e' : < $l : 'a; ..> Poly_record.t) $l_hash : 'a)
  *)
  let open Exp in
  with_loc e.pexp_loc & fun () ->
    let hash = int ~loc:loc' (hash l) in
    let tvar = Typ.new_var & Some "object" in
    let obj_poly_record_t =
      Typ.(constr (poly_record "t") [ object_ [!@l, [], tvar] Open ])
    in
    constraint_
      (apply (ident (poly_record_internal "get"))
         [ Nolabel, constraint_ e' obj_poly_record_t
         ; Nolabel, hash])
      tvar

let handle_poly_pexp_setfield e e' l loc' e'' =
  (* 
     Poly_record.set ($e' : < $l : 'a ref; ..> Poly_record.t) $l_hash (e'' : 'a)
  *)
  let open Exp in
  with_loc e.pexp_loc & fun () ->
    let hash = int ~loc:loc' (hash l) in
    let tvar = Typ.new_var & Some "object" in
    let tvar_ref = Typ.ref_ tvar in
    let obj_poly_record_t =
      Typ.(constr (poly_record "t") [ object_ [!@l, [], tvar_ref] Open ])
    in
    apply (ident (poly_record_internal "set"))
      [ Nolabel, constraint_ e' obj_poly_record_t
      ; Nolabel, hash
      ; Nolabel, constraint_ e'' tvar
      ]

let extend super =
  let expr self e = match e.pexp_desc with
    | Pexp_extension ({txt="poly_record"}, PStr [ { pstr_desc= Pstr_eval (e, attrs) } ]) ->
        check_attrs_empty e.pexp_loc attrs;
        with_poly_record true & fun () -> self.expr self e
    | Pexp_extension ({txt="mono_record"}, PStr [ { pstr_desc= Pstr_eval (e, attrs) } ]) ->
        check_attrs_empty e.pexp_loc attrs;
        with_poly_record false & fun () -> self.expr self e
    | Pexp_extension ({txt=("poly_record" | "mono_record")}, _) ->
        raise_errorf ~loc:e.pexp_loc "This extension must take only one expression"
    | Pexp_apply( { pexp_desc= Pexp_ident {txt= Longident.Lident "!" } },
                  [Nolabel, ({ pexp_desc= Pexp_record (fields, eopt) })] ) ->
        (*  !{ ... } *)
        handle_poly_pexp_record e fields eopt
        
    | Pexp_apply( { pexp_desc= Pexp_ident {txt= Longident.Lident "#!" } },
                  [Nolabel, e';
                   Nolabel, { pexp_desc= Pexp_ident {txt= Longident.Lident l; loc= loc'} }]) ->
        (*  r#!l *)
        handle_poly_pexp_field e e' l loc'
        
    | Pexp_apply( { pexp_desc= Pexp_ident {txt= Longident.Lident "#!" } },
                  [Nolabel, _e';
                   Nolabel, e'']) ->
        (*  r#!(expr) is error *)
        error_illegal_field e''.pexp_loc
        
    | _ when not (is_poly_record ()) -> super.expr self e

    (* is_poly_record () *)

    | Pexp_record (fields, eopt) ->
        handle_poly_pexp_record e fields eopt

    | Pexp_field (e', {txt=Lident l; loc=loc'}) ->
        handle_poly_pexp_field e e' l loc'

    | Pexp_field (_, {txt=_lident; loc=f_loc}) ->
        (* [x.M.l] is error *)
        error_field_with_module f_loc
                  
    | Pexp_setfield (e', {txt=Lident l; loc=loc'}, e'') ->
        handle_poly_pexp_setfield e e' l loc' e''

    | Pexp_setfield (_, {txt=_lident; loc=f_loc}, _) ->
        (* [x.M.l <- e'] is error *)
        error_field_with_module f_loc
                  
    | _ -> super.expr self e
  in
  { super with expr }

include Ppxx.Ppx.Make(struct
    let make_mapper _ _ = extend default_mapper
    let name = "ppx_poly_record"
    let options = []
  end)

let () = register ()
