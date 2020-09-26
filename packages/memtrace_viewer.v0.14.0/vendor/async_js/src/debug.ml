open Js_of_ocaml
open Core_kernel

let is_valid_id s =
  if String.equal s ""
  then false
  else (
    match s.[0] with
    | '0' .. '9' -> false
    | _ ->
      String.for_all s ~f:(function
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
        | _ -> false))
;;

let is_valid_field_name ~seen name = is_valid_id name && not (Set.mem seen name)

let rec key_value_shape ~seen ~rev_acc list =
  match (list : Sexp.t list) with
  | [] -> Some (List.rev rev_acc)
  | Atom name :: rest when is_valid_field_name ~seen name ->
    let rev_acc = (name, None) :: rev_acc in
    let seen = Set.add seen name in
    key_value_shape ~seen ~rev_acc rest
  | List [ Atom name; v ] :: rest when is_valid_field_name ~seen name ->
    let rev_acc = (name, Some v) :: rev_acc in
    let seen = Set.add seen name in
    key_value_shape ~seen ~rev_acc rest
  | _ -> None
;;

let rec any_of_sexp = function
  | Sexp.Atom s -> Js.Unsafe.inject (Js.string s)
  | Sexp.List [ Atom name; v ] -> Js.Unsafe.obj [| name, any_of_sexp v |]
  | Sexp.List l ->
    (match key_value_shape ~seen:String.Set.empty ~rev_acc:[] l with
     | None -> List.map l ~f:any_of_sexp |> Array.of_list |> Js.array |> Js.Unsafe.inject
     | Some [] -> Js.array [||] |> Js.Unsafe.inject
     | Some l ->
       Js.Unsafe.obj
         (List.map l ~f:(function
            | name, Some v -> name, any_of_sexp v
            | name, None -> name, Js.Unsafe.inject Js.null)
          |> Array.of_list))
;;

let log_s sexp = Firebug.console##log (any_of_sexp sexp)

let%expect_test _ =
  let module M = struct
    type u = { some_name : string } [@@deriving sexp]

    type t =
      | Foo
      | Bar of
          { field1 : u
          ; field2 : string option
          }
    [@@deriving sexp]
  end
  in
  let to_string : Js.Unsafe.any -> string =
    fun any -> Js.to_string (Js._JSON##stringify any : Js.js_string Js.t)
  in
  let open M in
  print_endline (to_string (any_of_sexp (sexp_of_t Foo)));
  [%expect {| "Foo" |}];
  print_endline
    (to_string
       (any_of_sexp
          (sexp_of_t
             (Bar { field1 = { some_name = "debug" }; field2 = Some "other string" }))));
  [%expect {| {"Bar":null,"field1":{"some_name":"debug"},"field2":["other string"]} |}]
;;

let%expect_test "duplicate keys are displayed correctly" =
  let to_string : Js.Unsafe.any -> string =
    fun any -> Js.to_string (Js._JSON##stringify any : Js.js_string Js.t)
  in
  let sexp_with_duplicate_keys = {| ((A B) (A C)) |} |> Sexp.of_string in
  print_endline (to_string (any_of_sexp sexp_with_duplicate_keys));
  [%expect {| [{"A":"B"},{"A":"C"}] |}]
;;

let%expect_test "no stack overflow" =
  let a = Array.init 10000 ~f:(fun i -> sprintf "f%d" i, i) in
  let sexp = [%sexp_of: (string * int) array] a in
  let x = any_of_sexp sexp in
  printf "%d" (Js.Unsafe.coerce x)##.f999;
  [%expect {| 999 |}]
;;
