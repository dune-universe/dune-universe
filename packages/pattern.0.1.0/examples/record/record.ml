type example = { x : int; y : int; z : int }
    [@@deriving traverse_lift]

let quoter = object
  inherit Ppxlib_metaquot_lifters.expression_lifters Location.none
  inherit [Parsetree.expression] lift
end

let () =
  let v = { x = 1; y = 2; z = 3 } in
  match [%pattern? { x = 1; y = 2; z = 4 }] ~quoted:(quoter#example v) v with
  | Ok () -> assert false
  | Error failure ->
      Format.printf "%a@." Pattern_runtime.format_failure failure;
      (* { x = _; y = _; z = (@0) }
         @0: Expected: 4
             Got: 3 *)
      begin
        match failure with
        | { common = [%pat?
              { x = _; y = _;
                z = [%p? { ppat_desc = Ppat_var { txt = "@0"; _ }; _}]}];
            mismatches = [{
              ident = "@0";
              expected = [%pat? 4];
              got = Some [%expr 3];
            }]} -> ()
        | _ -> assert false
      end

let () =
  let v = { x = 1; y = 2; z = 3 } in
  match [%pattern? { x; y; z }] ~quoted:(quoter#example v) v with
  | Ok binders ->
      assert (binders#x = 1);
      assert (binders#y = 2);
      assert (binders#z = 3)
  | Error failure ->
      Format.printf "%a@." Pattern_runtime.format_failure failure;
      assert false
