open Core

type sexpr =
  | Atom of string
  | Sexpr of sexpr list

module PrettySexpr (P : Prettiest.S) = struct
  open P.Infix

  let rec pretty_sexpr xs =
    match xs with
    | Atom s -> !^ s
    | Sexpr xs -> !^ "(" <> (P.sep (List.map ~f:pretty_sexpr xs)) <> !^ ")"

end

let test_data =
  let abcd = Sexpr [Atom "a"; Atom "b"; Atom "c"; Atom "d"] in
  let abcd4 = Sexpr [abcd; abcd; abcd; abcd] in
  Sexpr [
    Sexpr [Atom "abcde"; abcd4];
    Sexpr [Atom "abcdefgh"; abcd4];
  ]

let fit = Option.value ~default:"did not fit"

module P80 = Prettiest.Make (struct let width = 80 end)
module PS80 = PrettySexpr (P80)

module P50 = Prettiest.Make (struct let width = 50 end)
module PS50 = PrettySexpr (P50)

module P20 = Prettiest.Make (struct let width = 20 end)
module PS20 = PrettySexpr (P20)

let%expect_test "sexp" =
  test_data |> PS80.pretty_sexpr |> P80.render |> fit |> print_endline;
  Out_channel.newline stdout;
  test_data |> PS50.pretty_sexpr |> P50.render |> fit |> print_endline;
  Out_channel.newline stdout;
  test_data |> PS20.pretty_sexpr |> P20.render |> fit |> print_endline;

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
