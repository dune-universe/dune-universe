open OUnit2

let assert_ok r test =
  match r with
  | Result.Ok x -> test x
  | Result.Error s -> assert_failure s

let equal_options ~(equal : 'a -> 'a -> bool) (a : 'a option) (b : 'a option) =
  match a, b with
  | (Some x), (Some y) ->
    equal x y
  | None, None -> true
  | _, _ -> false

let cstruct_of_hex str = `Hex (String.lowercase_ascii str) |> Hex.to_cstruct
