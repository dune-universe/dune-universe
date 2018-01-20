open Core

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
  test_data |> pretty_sexpr |> Prettiest.render 50 |> fit |> print_endline;
  Out_channel.newline stdout;
  test_data |> pretty_sexpr |> Prettiest.render 20 |> fit |> print_endline;
  [%expect {|
    ((abcde ((a b c d) (a b c d) (a b c d) (a b c d)))
     (abcdefgh ((a b c d) (a b c d) (a b c d) (a b c d))))

    ((abcde ((a b c d) (a b c d) (a b c d) (a b c d)))
     (abcdefgh
      ((a b c d) (a b c d) (a b c d) (a b c d))))

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

let () = Ppx_inline_test_lib.Runtime.exit ()
