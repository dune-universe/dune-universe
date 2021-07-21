open Core_kernel
open Result.Monad_infix

type matrix = {
  id : string ;
  tf_name : string ;
  counts : int array array ;
}

let parse_header s =
  try Scanf.sscanf s ">%s %s" (fun x y -> Ok (x, y))
  with Scanf.Scan_failure _ -> Error "Incorrect header"

let%test "Jaspar header" =
  Poly.(parse_header ">MA0597.1       THAP1" = Ok ("MA0597.1", "THAP1"))

let space_re =
  Re.(rep1 (alt [ char ' ' ; char '\t' ]))
  |> Re.compile

let parse_row s =
  match Re.split space_re s with
  | _ :: "[" :: toks ->
    Array.(slice (of_list toks) 0 (-1))
    |> Array.map ~f:Int.of_string
    |> Result.return
  | _ -> Error "Incorrect count line"

let%expect_test "Jaspar row" =
  let s = "C  [    93     47      0    182    195    136     17     71     34 ]" in
  print_endline
    Fmt.(to_to_string
           (result ~ok:(array ~sep:(const string ";") int) ~error:string)
           (parse_row s));
  [%expect {| 93;47;0;182;195;136;17;71;34 |}]

let counts_of_rows rows =
  match Array.(transpose (of_list rows)) with
  | None -> Error "Unequal number of elements in rows"
  | Some t -> Ok t

let of_file fn =
  match In_channel.read_lines fn with
  | [ header ; p_a ; p_c ; p_g ; p_t ] ->
    parse_header header >>= fun (id, tf_name) ->
    parse_row p_a >>= fun row_a ->
    parse_row p_c >>= fun row_c ->
    parse_row p_g >>= fun row_g ->
    parse_row p_t >>= fun row_t ->
    counts_of_rows [ row_a ; row_c ; row_g ; row_t ] >>= fun counts ->
    Ok { id ; tf_name ; counts }
  | _ -> Error "Unexpected number of lines"
