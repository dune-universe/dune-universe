# prettiest

Implementation of Jean-Philippe Bernardy's [A Pretty But Not Greedy Printer (Functional Pearl). PACMPL 1(ICFP): 6:1-6:21 (2017)](https://jyp.github.io/pdf/Prettiest.pdf) in OCaml.
The author of the paper has a [version in Haskell](https://github.com/jyp/prettiest).

I make no promises about performance.

[Documentation](https://htmlpreview.github.io/?https://raw.githubusercontent.com/andreasfrom/prettiest/master/doc/prettiest/Prettiest/index.html)

# Example from the paper

A version of this example is included in the test folder.
That folder also includes a larger example of pretty printing types.

```ocaml
type sexpr =
  | Atom of string
  | Sexpr of sexpr list

let rec pretty_sexpr xs =
  let open Prettiest in
  let open Prettiest.Infix in
  match xs with
  | Atom s -> text s
  | Sexpr xs -> text "(" <> (sep (List.map ~f:pretty_sexpr xs)) <> text ")"

let test_data =
  let abcd = Sexpr [Atom "a"; Atom "b"; Atom "c"; Atom "d"] in
  let abcd4 = Sexpr [abcd; abcd; abcd; abcd] in
  Sexpr [
    Sexpr [Atom "abcde"; abcd4];
    Sexpr [Atom "abcdefgh"; abcd4];
  ]

let fit = Option.value ~default:"did not fit"

let%expect_test "sexp" =
  test_data |> pretty_sexpr |> Prettiest.render 80 |> fit |> print_endline;
  Out_channel.newline stdout;
  test_data |> pretty_sexpr |> Prettiest.render 20 |> fit |> print_endline;
  [%expect {|
    ((abcde ((a b c d) (a b c d) (a b c d) (a b c d)))
     (abcdefgh ((a b c d) (a b c d) (a b c d) (a b c d))))

    ((abcde ((a b c d)
             (a b c d)
             (a b c d)
             (a b c d)))
     (abcdefgh
      ((a b c d)
       (a b c d)
       (a b c d)
       (a b c d))))
  |}]
```
