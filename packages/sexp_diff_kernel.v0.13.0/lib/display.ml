open Core_kernel
module U = Display_util_internal

module Display_options = struct
  include U.Display_options
end

let center ~width s =
  let left_spaces = (width - String.length s) / 2 in
  let right_spaces = width - String.length s - left_spaces in
  [ String.make left_spaces ' '; s; String.make right_spaces ' ' ] |> String.concat
;;

let display_as_string_with_custom_formatting ?display_options diff ~green ~red ~plain =
  let lines =
    let on_hidden ~num_hidden ~width = center ~width (U.hide_message ~num_hidden) in
    let on_all_hidden ~width = center ~width U.all_hidden_message in
    let on_line_pair ~left ~right ~left_padding ~right_padding =
      [ U.Line.to_text ~green ~red ~plain left
      ; left_padding
      ; U.Line.to_text ~green ~red ~plain right
      ; right_padding
      ]
      |> String.concat
    in
    U.display ?display_options diff ~on_hidden ~on_all_hidden ~on_line_pair
  in
  String.concat ~sep:"\n" lines
;;

let display_as_plain_string ?display_options diff =
  display_as_string_with_custom_formatting
    ?display_options
    diff
    ~green:(fun x -> "+" ^ x)
    ~red:(fun x -> "-" ^ x)
    ~plain:(fun x -> " " ^ x)
;;

let display_with_ansi_colors ?display_options diff =
  display_as_string_with_custom_formatting
    ?display_options
    diff
    ~green:(fun x -> sprintf "\027[32m%s\027[0m" x)
    ~red:(fun x -> sprintf "\027[31m%s\027[0m" x)
    ~plain:Fn.id
;;

let%expect_test "the diff looks like this" =
  let original =
    {|
      ((apple 123)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (carrot -1))
    |}
    |> Sexp.of_string
  in
  let updated =
    {|
      ((apricot 321)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (banana 1000)
       (durian 1234)
       (carrot 42))
    |}
    |> Sexp.of_string
  in
  let diff = Algo.diff ~original ~updated () in
  let () = print_endline (display_as_plain_string diff) in
  [%expect
    {|
  (                         (
 - (apple 123)             + (apricot 321)
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
             ...6 unchanged lines...
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
   (banana 1000)             (banana 1000)
                           + (durian 1234)
   (carrot                   (carrot
 -  -1                     +  42
   ))                        )) |}]
;;
