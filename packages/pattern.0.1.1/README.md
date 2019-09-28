# Run-time patterns that explain match failures

`pattern` is a PPX extension that generates functions from
patterns that explain match failures by returning the common context
and the list of differences between a pattern and a value.

To quote the differences, the generated function needs a /quoter/ for
the value to be matched, that is to say a function of type `'a ->
Parsetree.expression` where `'a` is the type of the matched value.
Such a quoter may be derived by using the `ppxlib.traverse_lift`
deriving plugin and the `ppxlib.metaquot_lifters` library.

```ocaml
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
```

If patterns have binders, then in case of successful match, the
generated function returns `Ok bindings`, where `bindings` is an
object, with one constant method for each binder.

```ocaml
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
```

`Pattern_runtime.check` can be used to match a value against a pattern
without having to repeat the value when calling the quoter. Since the
value argument is passed before the pattern, if the type of the value
is known during type inference, then it can be used to resolve the
variant constructor and the record field names that appear in the
pattern.

```ocaml
let () =
  let v = { x = 1; y = 2; z = 3 } in
  match Pattern_runtime.check quoter#example v [%pattern? { x; y; z }] with
  | Ok binders ->
      assert (binders#x = 1);
      assert (binders#y = 2);
      assert (binders#z = 3)
  | Error failure ->
      Format.printf "%a@." Pattern_runtime.format_failure failure;
      assert false
```
