exception Test_failure of string;;

let string_of_list f xs =
  List.fold_left
    (fun a e -> if a = "" then f e else a ^ ", " ^ f e)
    ""
    xs
;;

let string_of_int_opt o =
  match o with
  | None -> "None"
  | Some i -> Printf.sprintf "Some %d" i

module NondeterminismMonad = struct
  let return x = [x];;
  let bind m k = List.concat (List.map k m);;
  let zero () = [];;
end;;

module Option = struct
  let return x = Some x;;
  let bind o f =
    match o with
    | None -> None
    | Some x -> f x
  ;;
  let zero () = None;;
end;;

let nondeterminism_test =
  let open NondeterminismMonad in
  let%bind x = [1;2;3] in
  let%bind y = [4;5] in
  return (x+y)
;;

let match_test =
  let open Option in
  let l = [None; Some 0; Some 1] in
  List.map (
    fun x ->
      match%bind x with
      | 0 -> return 1
      | _ -> return 2
  ) l
;;

let nested_match_test =
  let open Option in
  let l = [None; Some 0; Some 1] in
  List.map (
    fun x ->
      match%bind x with
      | 0 -> return 1
      | _ ->
        match%bind None with
        | _ -> return 2
  ) l
;;

let if_test =
  let open Option in
  let l = [None; Some true; Some false] in
  List.map (
    fun x ->
      if%bind x then
        return 1
      else
        return 2
  ) l
;;

let nested_if_test =
  let open Option in
  let l = [None; Some true; Some false] in
  List.map (
    fun x ->
      if%bind x then
        if%bind None then
          return 3
        else
          return 4
      else
        return 2
  ) l
;;

let sequence_test =
  let open Option in
  let should_be_none =
    None;%bind
    Some ();%bind
    return 1
  in
  let should_be_some =
    Some ();%bind
    Some ();%bind
    return 1
  in
  [should_be_none; should_be_some]
;;

let nested_bind_test =
  let open NondeterminismMonad in
  let%bind x =
    let%bind y = [1;2;3] in
    return (y+1)
  in
  return (x+1)
;;

let orzero_test_first =
  let open NondeterminismMonad in
  let a = [1;2] in
  let%orzero [x;y] = a in
  return @@ x + y
;;

let orzero_test_second =
  let open NondeterminismMonad in
  let b = [1;2;3] in
  let%orzero [x;y] = b in
  return @@ x + y
;;

let orzero_test_third =
  let open NondeterminismMonad in
  let a = [1;2] in
  let b = [1;2;3] in
  let%orzero [x;y] = a in
  let%orzero [z] = b in
  return @@ x + y + z
;;

let orzero_test_fourth =
  let open NondeterminismMonad in
  let a = [1;2] in
  let%orzero [x] =
    let%orzero [y;z] = a in
    return @@ y + z
  in
  return @@ x + 1
;;

let guard_test =
  let open NondeterminismMonad in
  let x = [1;2;3;4] in
  let%bind y = x in
  [%guard y mod 2 == 0];
  return y
;;

let chained_guard_test =
  let open NondeterminismMonad in
  let x = [-9;-8;-7;-6;-5;-4;-3;-2;-1;0;1;2;3;4;5;6;7;8;9] in
  let%bind y = x in
  [%guard y mod 2 == 0];
  [%guard y > 0];
  [%guard y <> 4];
  return y
;;

let zero_guard_interleave_test =
  let open NondeterminismMonad in
  let xss = [[0;1;2];[3;4];[5;6;7];[];[8;9;10]] in
  let%bind xs = xss in
  let%orzero [a;_b;c] = xs in
  [%guard a <> 0];
  return c
;;

let halt_with s = print_endline s; exit 1;;

let () =
  begin
    match nondeterminism_test with
    | [5;6;6;7;7;8] -> ()
    | _ ->
      halt_with
        ("Unexpected value from nondeterminism test: " ^
         string_of_list string_of_int nondeterminism_test)
  end;
  begin
    match match_test with
    | [None; Some 1; Some 2] -> ()
    | _ ->
      halt_with
        ("Unexpected value from match test: " ^
         string_of_list string_of_int_opt match_test)
  end;
  begin
    match nested_match_test with
    | [None; Some 1; None] -> ()
    | _ ->
      halt_with
        ("Unexpected value from nested match test: " ^
         string_of_list string_of_int_opt nested_match_test)
  end;
  begin
    match if_test with
    | [None; Some 1; Some 2] -> ()
    | _ ->
      halt_with
        ("Unexpected value from if test: " ^
         string_of_list string_of_int_opt if_test)
  end;
  begin
    match nested_if_test with
    | [None; None; Some 2] -> ()
    | _ ->
      halt_with
        ("Unexpected value from nested if test: " ^
         string_of_list string_of_int_opt nested_if_test)
  end;
  begin
    match sequence_test with
    | [None; Some 1] -> ()
    | _ ->
      halt_with
        ("Unexpected value from sequence test: " ^
         string_of_list string_of_int_opt sequence_test)
  end;
  begin
    match nested_bind_test with
    | [3;4;5] -> ()
    | _ ->
      halt_with
        ("Unexpected value from nested bind test: " ^
         string_of_list string_of_int nested_bind_test)
  end;
  begin
    match orzero_test_first with
    | [3] -> ()
    | _ ->
      halt_with
        ("Unexpected value from first orzero test: " ^
         string_of_list string_of_int orzero_test_first)
  end;
  begin
    match orzero_test_second with
    | [] -> ()
    | _ ->
      halt_with
        ("Unexpected value from second orzero test: " ^
         string_of_list string_of_int orzero_test_second)
  end;
  begin
    match orzero_test_third with
    | [] -> ()
    | _ ->
      halt_with
        ("Unexpected value from third orzero test: " ^
         string_of_list string_of_int orzero_test_third)
  end;
  begin
    match orzero_test_fourth with
    | [4] -> ()
    | _ ->
      halt_with
        ("Unexpected value from fourth orzero test: " ^
         string_of_list string_of_int orzero_test_fourth)
  end;
  begin
    match guard_test with
    | [2;4] -> ()
    | _ ->
      halt_with
        ("Unexpected value from guard test: " ^
         string_of_list string_of_int guard_test)
  end;
  begin
    match chained_guard_test with
    | [2;6;8] -> ()
    | _ ->
      halt_with
        ("Unexpected value from chained guard test: " ^
         string_of_list string_of_int chained_guard_test)
  end;
  begin
    match zero_guard_interleave_test with
    | [7;10] -> ()
    | _ ->
      halt_with
        ("Unexpected value from zero/guard interleave test: " ^
         string_of_list string_of_int zero_guard_interleave_test)
  end;
  print_endline "All tests passed."
;;
