let build_pat pat : Ppxlib.expression =
  [%expr Ppxlib.Ast_helper.Pat.mk [%e pat]]

let build_pat_construct ctor arg : Ppxlib.expression =
  build_pat [%expr Ppat_construct (
    [%e Metaquot.Exp.loc Metaquot.Exp.longident ctor], [%e arg])]

let build_pat_tuple tuple : Ppxlib.expression =
  build_pat [%expr Ppat_tuple [%e Metapp.Exp.list tuple]]

let sub i = Printf.sprintf "sub%d" i

let quoted i = Printf.sprintf "quoted%d" i

let pat_var_of_string ~loc s =
  Ppxlib.Ast_helper.Pat.var { loc; txt = s }

let exp_var_of_string ~loc s =
  Ppxlib.Ast_helper.Exp.ident { loc; txt = Lident s }

let pat_tuple_or_value ~loc list : Ppxlib.pattern =
  match list with
  | [] -> [%pat? ()]
  | [value] -> value
  | _ -> Ppxlib.Ast_helper.Pat.tuple ~loc list

let exp_tuple_or_value ~loc list : Ppxlib.expression =
  match list with
  | [] -> [%expr ()]
  | [value] -> value
  | _ -> Ppxlib.Ast_helper.Exp.tuple ~loc list

let mismatch ~loc pat : Ppxlib.expression =
  [%expr
     __mismatch_count_ref := index + 1;
   let ident = Printf.sprintf "@%d" index in
   let loc = Location.none in
   Error {
   common = Ppxlib.Ast_helper.Pat.var { loc; txt = ident };
   mismatches = [{
     ident;
     expected = [%e Metaquot.Exp.pattern pat];
     got = quoted; }]}]

let mismatch_here ~loc pat : Ppxlib.expression =
  [%expr
    let index = !__mismatch_count_ref in
    [%e mismatch ~loc pat]]

let pat_of_binders ~loc binders =
  pat_tuple_or_value ~loc (binders |> List.map begin fun txt ->
    Ppxlib.Ast_helper.Pat.var { loc; txt }
  end)

let exp_of_binders ~loc binders =
  exp_tuple_or_value ~loc (binders |> List.map begin fun x ->
    Ppxlib.Ast_helper.Exp.ident { loc; txt = Lident x }
  end)

let rec extract_pat_list (pat : Ppxlib.pattern) =
  match pat with
  | [%pat? []] -> []
  | [%pat? [%p? hd] :: [%p? tl]] -> hd :: extract_pat_list tl
  | _ -> raise Exit

let multiple_match ~loc make_matcher patterns get_pattern destruct
  (destruct_quoted :
     Ppxlib.expression -> string list -> Ppxlib.expression ->
       Ppxlib.expression -> Ppxlib.expression) build_common =
  let subs = List.mapi (fun i _ -> sub i) patterns in
  let sub_pats = List.map (pat_var_of_string ~loc) subs in
  let quoteds = List.mapi (fun i _ -> quoted i) patterns in
  destruct sub_pats (fun () : (string list * Ppxlib.expression) ->
    let binders, subpatterns =
      patterns |> List.mapi begin
           fun i arg : (string list * Ppxlib.expression) ->
             let binders, subpattern = make_matcher (get_pattern arg) in
             binders, [%expr
               let quoted = [%e exp_var_of_string ~loc (quoted i)] in
               let __value__ = [%e exp_var_of_string ~loc (sub i)] in
               [%e subpattern]]
           end |> List.split in
    let all_binders = binders |> List.flatten in
    all_binders, [%expr
         let [%p pat_tuple_or_value ~loc (List.map begin
           fun pat : Ppxlib.pattern ->
             [%pat? ([%p pat_var_of_string ~loc pat]
               : Ppxlib.expression option)]
         end quoteds)] =
           match quoted with
           | None ->
               [%e exp_tuple_or_value ~loc (patterns |> List.mapi begin
                 fun i _ : Ppxlib.expression ->
                   [%expr None]
               end)]
           | Some quoted ->
           [%e destruct_quoted
              [%expr Pattern.elim_type_constraints quoted] quoteds
              (exp_tuple_or_value ~loc (patterns |> List.mapi begin
                fun i _ : Ppxlib.expression ->
                  [%expr Some [%e exp_var_of_string ~loc (quoted i)]]
              end))
              (exp_tuple_or_value ~loc (patterns |> List.mapi begin
                fun i _ : Ppxlib.expression ->
                  [%expr None]
              end))] in
         match [%e exp_tuple_or_value ~loc subpatterns] with
         | [%p pat_tuple_or_value ~loc
              (binders |> List.map begin
                fun binders : Ppxlib.pattern ->
                  [%pat? Ok [%p pat_of_binders ~loc binders]]
              end)] ->
                Ok [%e exp_of_binders ~loc all_binders]
         | [%p pat_tuple_or_value ~loc sub_pats] ->
             let common = let loc = Location.none in
             [%e build_common (subs |> List.map begin
               fun sub : Ppxlib.expression ->
                 [%expr (match [%e exp_var_of_string ~loc sub] with
                 | Ok _ -> [%e build_pat [%expr Ppat_any]]
                 | Error error -> error.common)]
             end)] in
             let mismatches =
               List.flatten [%e List.fold_right begin
                 fun sub list : Ppxlib.expression ->
                   [%expr (match [%e exp_var_of_string ~loc sub] with
                   | Ok _ -> []
                   | Error error -> error.mismatches) :: [%e list]]
               end subs [%expr []]] in
             Error { common; mismatches }])

let multiple_match_tuple ~loc make_matcher args destruct
    (destruct_quoted : Ppxlib.pattern -> Ppxlib.pattern) build_common =
  multiple_match ~loc make_matcher args Fun.id
    (fun sub_pats k -> destruct (Ppxlib.Ast_helper.Pat.tuple ~loc sub_pats) k)
    (fun quoted quoteds success none ->
      [%expr match [%e quoted] with
      | [%p destruct_quoted [%pat?
          { pexp_desc = Pexp_tuple [%p List.fold_right begin
            fun var list : Ppxlib.pattern ->
              [%pat? [%p pat_var_of_string ~loc var] :: [%p list]]
          end quoteds [%pat? []]]; _ }]] -> [%e success]
      | _ -> [%e none]])
    (fun args -> build_common (build_pat_tuple args))

let multiple_match_record ~loc make_matcher fields closed_flag destruct
    (destruct_quoted : Ppxlib.pattern -> Ppxlib.pattern) build_common =
  multiple_match ~loc make_matcher fields (fun (_label, pat) -> pat)
    (fun sub_pats k ->
      destruct (Ppxlib.Ast_helper.Pat.record
        (List.combine (List.map fst fields) sub_pats) closed_flag) k)
    (fun quoted quoteds success none ->
      [%expr
            let extract_field
                [%p pat_tuple_or_value ~loc
                   (List.map (pat_var_of_string ~loc) quoteds)]
                ((label : Longident.t Location.loc), value) =
              [%e Ppxlib.Ast_helper.Exp.match_ [%expr label.txt]
                 (List.mapi (fun i ((label : Longident.t Location.loc), _) ->
                   Ppxlib.Ast_helper.Exp.case
                     (Metaquot.Pat.longident label.txt)
                     (exp_tuple_or_value ~loc (List.mapi begin
                       fun j q : Ppxlib.expression ->
                         if i = j then
                           [%expr Some value]
                         else
                           exp_var_of_string ~loc q
                     end quoteds))) fields @
                  [Ppxlib.Ast_helper.Exp.case (Ppxlib.Ast_helper.Pat.any ~loc ())
                    (exp_tuple_or_value ~loc
                       (List.map (exp_var_of_string ~loc) quoteds))])] in
            match [%e quoted] with
            | [%p destruct_quoted [%pat?
                  { pexp_desc = Pexp_record (fields, None); _ }]] ->
                begin
                  match List.fold_left extract_field [%e none] fields with
                  | [%p pat_tuple_or_value ~loc
                       (List.map
                          (fun q : Ppxlib.pattern ->
                            [%pat? Some [%p pat_var_of_string ~loc q]])
                          quoteds)] -> [%e success]
                  | _ -> [%e none]
                end
            | _ -> [%e none]])
        (fun args ->
          build_common (build_pat
            [%expr Ppat_record
            ([%e Metapp.Exp.list
                (List.map2 (fun (label, _) value ->
                  Ppxlib.Ast_helper.Exp.tuple
              [Metaquot.Exp.loc
                 Metaquot.Exp.longident label; value]) fields args)],
              [%e Metaquot.Exp.closed_flag closed_flag])]))

let single_match ~loc make_matcher pat pattern quoted_pattern build_common
    : string list * Ppxlib.expression =
  let bindings, sub_matcher = make_matcher pat in
  bindings, [%expr
     match __value__ with
     | [%p pattern] ->
         begin
           match
             let (quoted : Ppxlib.expression option) =
               match quoted with
               | None -> None
               | Some quoted ->
               match Pattern.elim_type_constraints quoted with
               | [%p quoted_pattern] ->
                   Some arg
               | _ -> None in
             let __value__ = sub in [%e sub_matcher] with
           | Ok bindings -> Ok bindings
           | Error error ->
               Error {
                 common =
                   (let loc = Location.none in
                   [%e build_common]);
                 mismatches = error.mismatches }
         end
     | _ ->
         [%e mismatch_here ~loc pat]]

let rec make_matcher' make_matcher (pat : Ppxlib.pattern)
    (type_constr : Ppxlib.pattern -> Ppxlib.pattern)
      : string list * Ppxlib.expression =
  let loc = pat.ppat_loc in
  Ppxlib.Ast_helper.with_default_loc loc @@ fun () ->
  match pat with
  | [%pat? ([%p? pat] : [%t? ty])] ->
     make_matcher' make_matcher pat begin fun contents ->
       [%pat? ([%p contents] : [%t ty])]
     end
  | [%pat? _] -> [], [%expr Ok ()]
  | { ppat_desc = Ppat_var x } ->
      [x.txt], [%expr Ok __value__]
  | { ppat_desc = Ppat_alias (pat, x) } ->
      let binders, matcher = make_matcher pat in
      (x.txt :: binders),
      [%expr
         match [%e matcher] with
         | Ok [%p pat_of_binders ~loc binders] ->
             Ok [%e exp_of_binders ~loc ("__value__" :: binders)]
         | Error e -> Error e]
  | { ppat_desc = Ppat_constant constant; _ } ->
      [], [%expr
        match __value__ with
        | [%p type_constr (Ppxlib.Ast_helper.Pat.constant ~loc constant)] ->
            Ok ()
        | _ ->
            [%e mismatch_here ~loc pat]]
  | [%pat? ([%p? a] | [%p? b])] ->
      let binders_a, (a : Ppxlib.expression) = make_matcher a in
      let binders_b, (b : Ppxlib.expression) = make_matcher b in
      begin match
        List.find_opt (fun x -> not (List.mem x binders_b)) binders_a with
      | None -> ()
      | Some x ->
          Location.raise_errorf ~loc:a.pexp_loc
            "%s is bound here but is not bound in the right-hand side"
            x
      end;
      begin match
        List.find_opt (fun x -> not (List.mem x binders_a)) binders_b with
      | None -> ()
      | Some x ->
          Location.raise_errorf ~loc:b.pexp_loc
            "%s is bound here but is not bound in the left-hand side"
            x
      end;
      binders_a, [%expr
        let index = !__mismatch_count_ref in
        match [%e a] with
        | Ok bindings -> Ok bindings
        | Error _ ->
            match [%e b] with
            | Ok [%p pat_of_binders ~loc binders_b] ->
                Ok [%e exp_of_binders ~loc binders_a]
            | Error error_b -> [%e mismatch ~loc pat]]
  | { ppat_desc = Ppat_construct (ctor, None); _ } ->
      [], [%expr
         match __value__ with
         | [%p type_constr (Ppxlib.Ast_helper.Pat.construct ctor None)] -> Ok ()
         | _ ->
             [%e mismatch_here ~loc pat]]
  | { ppat_desc = Ppat_construct (ctor, Some [%pat? _]); _ } ->
      [], [%expr
         match __value__ with
         | [%p type_constr (Ppxlib.Ast_helper.Pat.construct ctor (Some [%pat? _]))] ->
             Ok ()
         | _ ->
             [%e mismatch_here ~loc pat]]
  | { ppat_desc = Ppat_construct (ctor, Some
        { ppat_desc = Ppat_tuple args; _ }); _ } ->
      begin match
        match ctor, args with
        | { txt = Lident "::"; _ }, [hd; tl] ->
            begin
              try Some (hd :: extract_pat_list tl)
              with Exit -> None
            end
        | _ -> None
      with
      | None ->
          multiple_match_tuple ~loc make_matcher args
            (fun sub_pats k : (string list * Ppxlib.expression) ->
              let binders, result = k () in
              binders, [%expr
                 match __value__ with
                 | [%p type_constr
                      (Ppxlib.Ast_helper.Pat.construct ctor (Some sub_pats))] ->
                     [%e result]
                 | _ ->
                     [%e mismatch_here ~loc pat]])
            (fun quoteds ->
              [%pat? {
                pexp_desc = Pexp_construct (_ctor, Some [%p quoteds]); _}])
            (fun args -> build_pat_construct ctor [%expr (Some [%e args])])
      | Some list ->
          let rec make_quoted_patt_list list : Ppxlib.pattern =
            match list with
            | [] -> [%pat? { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ }]
            | hd :: tl -> [%pat? { pexp_desc = Pexp_construct ({ txt = Lident "::"; _ }, Some ({ pexp_desc = Pexp_tuple [[%p hd]; [%p make_quoted_patt_list tl]]; _ })); _ }] in
          let rec make_quoted_expr_list list : Ppxlib.expression =
            match list with
            | [] ->
                build_pat_construct (Metapp.mkloc (Longident.Lident "[]")) [%expr None]
            | hd :: tl ->
                build_pat_construct (Metapp.mkloc (Longident.Lident "::")) [%expr Some
                  [%e build_pat_tuple [hd; make_quoted_expr_list tl]]] in
          multiple_match ~loc make_matcher list Fun.id
            (fun sub_pats k : (string list * Ppxlib.expression) ->
              let binders, result = k () in
              binders, [%expr
                 match __value__ with
                 | [%p type_constr (Metapp.Pat.list sub_pats)] ->
                     [%e result]
                 | _ ->
                     [%e mismatch_here ~loc pat]])
            (fun quoted quoteds success none ->
              [%expr match [%e quoted] with
              | [%p make_quoted_patt_list (List.map (pat_var_of_string ~loc) quoteds)] -> [%e success]
              | _ -> [%e none]])
            make_quoted_expr_list
      end
  | { ppat_desc = Ppat_construct (ctor, Some
        { ppat_desc = Ppat_record (fields, closed_flag); _ }); _ } ->
      multiple_match_record ~loc make_matcher fields closed_flag
        (fun sub_pats k : (string list * Ppxlib.expression) ->
          let binders, result = k () in
          binders, [%expr
             match __value__ with
             | [%p type_constr
                  (Ppxlib.Ast_helper.Pat.construct ctor (Some sub_pats))] ->
                 [%e result]
             | _ ->
                 [%e mismatch_here ~loc pat]])
        (fun quoteds ->
          [%pat? { pexp_desc = Pexp_construct (_ctor, Some [%p quoteds]); _}])
        (fun args -> build_pat_construct ctor [%expr (Some [%e args])])
  | { ppat_desc = Ppat_construct (ctor, Some pat); _ } ->
      single_match ~loc make_matcher pat
        (type_constr (Ppxlib.Ast_helper.Pat.construct ctor (Some [%pat? sub])))
        ([%pat? { pexp_desc = Pexp_construct (_ctor, Some arg); _ }])
        (build_pat_construct ctor [%expr (Some error.common)])
  | { ppat_desc = Ppat_tuple args; _ } ->
      multiple_match_tuple ~loc make_matcher args
        (fun sub_pats k : (string list * Ppxlib.expression) ->
          let binders, result = k () in
          binders, [%expr
           match __value__ with [%p type_constr sub_pats] -> [%e result]])
        Fun.id Fun.id
  | { ppat_desc = Ppat_record (fields, closed_flag); _ } ->
      multiple_match_record ~loc make_matcher fields closed_flag
        (fun sub_pats k : (string list * Ppxlib.expression) ->
          let binders, result = k () in
          binders, [%expr match __value__ with [%p type_constr sub_pats] ->
            [%e result]])
        Fun.id Fun.id
  | _ ->
      Location.raise_errorf ~loc "unimplemented: %a" Ppxlib.Pprintast.pattern pat

let rec make_matcher (pat : Ppxlib.pattern)
    : string list * Ppxlib.expression =
  let bindings, matcher = make_matcher' make_matcher pat Fun.id in
  bindings, [%expr ([%e matcher] : _ Pattern.pattern_result)]

let make_pat ~loc ~path:_ pat =
  let binders, result = make_matcher pat in
  [%expr (fun ?quoted __value__ ->
    let __mismatch_count_ref = ref 0 in
    begin match [%e result] with
    | Ok [%p pat_of_binders ~loc binders] ->
        (Ok [%e
          if binders = [] then
            [%expr ()]
          else
            Ppxlib.Ast_helper.Exp.object_ ~loc (
            Ppxlib.Ast_helper.Cstr.mk
              (Ppxlib.Ast_helper.Pat.any ~loc ())
              (binders |> List.map (fun x ->
                (Ppxlib.Ast_helper.Cf.method_ ~loc
                   { loc; txt = x }
                   Public (Ppxlib.Ast_helper.Cf.concrete Fresh
                     (Ppxlib.Ast_helper.Exp.ident
                        { loc; txt = Lident x}))))))] : (_, _) result)
    | Error e -> (Error e : (_, _) result)
    end) [@ocaml.warning "-26-27"]]

let extension =
  Ppxlib.Extension.declare "pattern" Expression
    Ppxlib.Ast_pattern.(ppat __ none)
    make_pat

let () =
  Ppxlib.Driver.register_transformation "pattern.ppx"
    ~rules:[Ppxlib.Context_free.Rule.extension extension]
