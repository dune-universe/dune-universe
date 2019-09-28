include Types

let check quoter (value : 'a) (pattern : ('a, 'b) matcher) =
  pattern ~quoted:(quoter value) value

let format_failure formatter failure =
  Format.fprintf formatter "@[<v>@[%a@]@ "
    Ppxlib.Pprintast.pattern failure.common;
  failure.mismatches |> List.iter begin fun mismatch ->
    Format.fprintf formatter
      "@[%s:@ @[<v>@[@[Expected:@]@ @[%a@]@]@,@[@[Got:@]@ @[%a@]@]@]@]@ "
      mismatch.ident Ppxlib.Pprintast.pattern mismatch.expected
      (fun fmt v ->
        match v with
        | None -> Format.fprintf fmt "?"
        | Some v -> Ppxlib.Pprintast.expression fmt v) mismatch.got
  end;
  Format.fprintf formatter "@]"

let rec elim_type_constraints (expression : Ppxlib.expression) =
  match expression with
  | { pexp_desc = Pexp_constraint (sub, _); _ } -> elim_type_constraints sub 
  | _ -> expression
