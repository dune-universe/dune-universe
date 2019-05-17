include Types

let format_failure formatter failure =
  Format.fprintf formatter "@[<v>@[%a@]@ " Pprintast.pattern failure.common;
  failure.mismatches |> List.iter begin fun mismatch ->
    Format.fprintf formatter
      "@[%s:@ @[<v>@[@[Expected:@]@ @[%a@]@]@,@[@[Got:@]@ @[%a@]@]@]@]@ "
      mismatch.ident Pprintast.pattern mismatch.expected
      (fun fmt v ->
        match v with
        | None -> Format.fprintf fmt "?"
        | Some v -> Pprintast.expression fmt v) mismatch.got
  end;
  Format.fprintf formatter "@]"

let rec elim_type_constraints (expression : Parsetree.expression) =
  match expression with
  | { pexp_desc = Pexp_constraint (sub, _); _ } -> elim_type_constraints sub 
  | _ -> expression
