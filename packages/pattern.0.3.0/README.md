# `pattern`: Run-time patterns that explain match failures

`pattern` is a PPX extension that generates functions from
patterns that explain match failures by returning the common context
and the list of differences between a pattern and a value.

`pattern` can be used with [`dune`] by using the [`preprocess`] field.

[`dune`]: https://github.com/ocaml/dune
[`preprocess`]: https://dune.readthedocs.io/en/latest/concepts.html#preprocessing-with-ppx-rewriters

```lisp
(executable
  ...
  (preprocess (pps pattern.ppx))
  (libraries ... pattern ...))
```

To quote the differences, the generated function needs a /quoted/
version of the value to be matched, that is to say a value of type
`Parsetree.expression` that represents the AST of the value to be
matched.
This quoted version can be obtained by using a quotation (for instance,
with [`metaquot`]: `[%expr x]` is the quoted version of the value `x`),
or by using a /lifter/, that is to say a function of type `'a ->
Parsetree.expression` where `'a` is the type of the matched value.
Such a lifter can be derived for instance with the [`refl`] library
(`Refl.Lift.Exp.lift [%refl: t] [] x` for lifting `x` of type `t`).

[`metaquot`]: https://github.com/thierry-martinez/metaquot
[`refl`]: https://github.com/thierry-martinez/refl

```ocaml
type example = { x : int; y : int; z : int }
    [@@deriving refl]

let () =
  let v = { x = 1; y = 2; z = 3 } in
  let quoted = Refl.Lift.Exp.lift [%refl: example] [] v in
  match [%pattern? { x = 1; y = 2; z = 4 }] ~quoted v with
  | Ok () -> assert false
  | Error failure ->
      Format.printf "%a@." Pattern.format_failure failure;
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
  let quoted = Refl.Lift.Exp.lift [%refl: example] [] v in
  match [%pattern? { x; y; z }] ~quoted v with
  | Ok binders ->
      assert (binders#x = 1);
      assert (binders#y = 2);
      assert (binders#z = 3)
  | Error failure ->
      Format.printf "%a@." Pattern.format_failure failure;
      assert false
```

`Pattern.check` can be used to match a value against a pattern
without having to repeat the value when calling the quoter. Since the
value argument is passed before the pattern, if the type of the value
is known during type inference, then it can be used to resolve the
variant constructor and the record field names that appear in the
pattern.

```ocaml
let () =
  let v = { x = 1; y = 2; z = 3 } in
  let quoter = Refl.Lift.Exp.lift [%refl: example] [] in
  match Pattern.check quoter v [%pattern? { x; y; z }] with
  | Ok binders ->
      assert (binders#x = 1);
      assert (binders#y = 2);
      assert (binders#z = 3)
  | Error failure ->
      Format.printf "%a@." Pattern.format_failure failure;
      assert false
```
