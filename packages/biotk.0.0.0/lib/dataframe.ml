open Core_kernel
open Rresult

type column =
  | Ints of int array
  | Int_opts of int option array
  | Floats of float array
  | Float_opts of float option array
  | Strings of string array
  | String_opts of string option array
[@@deriving show]

type t = {
  nrows : int ;
  ncols : int ;
  cols : (string * column) list ;
}

let column_length = function
  | Ints xs -> Array.length xs
  | Int_opts xs -> Array.length xs
  | Floats xs -> Array.length xs
  | Float_opts xs -> Array.length xs
  | Strings xs -> Array.length xs
  | String_opts xs -> Array.length xs

let make = function
  | [] -> Error (`Msg "empty dataframe")
  | (_, first_col) :: t as cols ->
    let nrows = column_length first_col in
    if not (List.for_all t ~f:(fun (_, col) -> column_length col = nrows)) then
      Error (`Msg "columns with different lengths")
    else
      match List.(find_a_dup ~compare:String.compare (map cols ~f:fst)) with
      | Some l -> Rresult.R.error_msgf "duplicate label %S" l
      | None ->
        let ncols = List.length cols in
        Ok { nrows ; ncols ; cols }

let columns df = df.cols

let nrows t = t.nrows
let ncols t = t.ncols

let get_col_by_name t = List.Assoc.find t.cols ~equal:String.equal
let get_col t i =
  List.nth t.cols i
  |> Option.map ~f:snd

let parse_header h =
  let labels = String.split ~on:'\t' h in
  labels, List.length labels

let fold_lines xs ~init ~f =
  let rec loop i acc = function
    | [] -> Ok acc
    | x :: xs ->
      match f i acc x with
      | Ok r -> loop (i + 1) r xs
      | Error _ as e -> e
  in
  loop 0 init xs

let optionally f = function
  | "NA" -> None
  | s -> Some (f s)

let rev_convert_col col =
  let conv f =
    List.rev_map col ~f
    |> Array.of_list
  in
  let conv_opt f = conv (optionally f) in
  if List.mem col "NA" ~equal:String.equal then
    try Int_opts (conv_opt Int.of_string) with _ ->
    try Float_opts (conv_opt Float.of_string)
    with _ ->
      String_opts (
        List.map col ~f:Option.some
        |> Array.of_list
      )
  else
    try Ints (conv Int.of_string) with _ ->
    try Floats (conv Float.of_string) with _ ->
      Strings (Array.of_list_rev col)

let parse_lines ncols lines =
  let open Result.Monad_infix in
  let init = 0, List.init ncols ~f:(Fn.const []) in
  fold_lines lines ~init ~f:(fun i (nr, acc) l ->
      let fields = String.split l ~on:'\t' in
      match List.map2 fields acc ~f:List.cons with
      | Ok r -> Ok (nr + 1, r)
      | Unequal_lengths -> Rresult.R.error_msgf "Line %d doesn't have the expected %d fields" (i + 1) ncols
    ) >>| fun (nrows, cols) ->
  nrows, List.map cols ~f:rev_convert_col

type parse_result = (int * column list, [`Msg of string]) result
[@@deriving show]

let%expect_test "Dataframe.parse_line ex1" =
  let got =
    parse_lines 3 [
      "a\t1.2\tNA" ;
      "a\t1.2\t2" ;
      "c\t-1.2\tNA" ;
    ]
  in
  print_endline (show_parse_result got) ;
  [%expect {|
    (Ok (3,
         [(Dataframe.Strings [|"a"; "a"; "c"|]);
           (Dataframe.Floats [|1.2; 1.2; -1.2|]);
           (Dataframe.Int_opts [|None; (Some 2); None|])])) |}]

let check_header ~colnames header =
  match List.for_all2 colnames header ~f:String.equal with
  | Ok true -> Ok ()
  | Ok false -> Error (`Msg "header is different from expected value")
  | Unequal_lengths -> Error (`Msg "incorrect number of columns")

let from_file ?(header = `Read_in_file) path =
  let open Let_syntax.Result in
  let lines = In_channel.read_lines path in
  let* labels, ncols, data_lines =
    match header, lines with
    | (`Read_in_file | `Expect _), [] ->
      Error (`Msg "empty file but expected header")
    | `Read_in_file, header :: lines ->
      let labels, ncols = parse_header header in
      Ok (labels, ncols, lines)
    | `Expect colnames, header :: data_lines ->
      let labels, ncols = parse_header header in
      let+ () = check_header ~colnames labels in
      labels, ncols, data_lines
    | `Use colnames, data_lines ->
      Ok (colnames, List.length colnames, data_lines)
    | `None, (h :: _ as data_lines) ->
      let colnames =
        String.split h ~on:'\t'
        |> List.mapi ~f:(fun i _ -> sprintf "C%d" i)
      in
      Ok (colnames, List.length colnames, data_lines)
    | `None, [] -> Ok ([], 0, [])
  in
  parse_lines ncols data_lines >>= fun (nrows, cols) ->
  let cols = List.zip_exn labels cols in
  Ok { nrows ; ncols ; cols }

exception Error of string

module Ez = struct
  let from_file ?header path =
    Rresult.R.failwith_error_msg (from_file ?header path)

  let error msg = raise (Error msg)
  let errorf fmt = Printf.ksprintf error fmt

  let by_int gen_func = gen_func ~f:get_col ~string_of_id:string_of_int
  let by_name gen_func = gen_func ~f:get_col_by_name ~string_of_id:Fn.id

  let get_ints_gen ~f ~string_of_id df id  = match f df id with
    | Some (Ints xs) -> xs
    | Some _ -> errorf "Column %s is not integer" (string_of_id id)
    | None -> errorf "No column %s" (string_of_id id)
  let get_ints = by_int get_ints_gen
  let get_ints_by_name = by_name get_ints_gen

  let get_int_opts_gen ~f ~string_of_id df id = match f df id with
    | Some (Int_opts xs) -> xs
    | Some _ -> errorf "Column %s is not integer with options" (string_of_id id)
    | None -> errorf "No column %s" (string_of_id id)
  let get_int_opts = by_int get_int_opts_gen
  let get_int_opts_by_name = by_name get_int_opts_gen

  let get_floats_gen ~f ~string_of_id df id = match f df id with
    | Some (Floats xs) -> xs
    | Some _ -> errorf "Column %s is not float" (string_of_id id)
    | None -> errorf "No column %s" (string_of_id id)
  let get_floats = by_int get_floats_gen
  let get_floats_by_name = by_name get_floats_gen


  let get_float_opts_gen ~f ~string_of_id df id = match f df id with
    | Some (Float_opts xs) -> xs
    | Some _ -> errorf "Column %s is not float with options" (string_of_id id)
    | None -> errorf "No column %s" (string_of_id id)
  let get_float_opts = by_int get_float_opts_gen
  let get_float_opts_by_name = by_name get_float_opts_gen

  let get_strings_gen ~f ~string_of_id df id = match f df id with
    | Some (Strings xs) -> xs
    | Some _ -> errorf "Column %s is not string" (string_of_id id)
    | None -> errorf "No column %s" (string_of_id id)
  let get_strings = by_int get_strings_gen
  let get_strings_by_name = by_name get_strings_gen

  let get_string_opts_gen ~f ~string_of_id df id = match f df id with
    | Some (String_opts xs) -> xs
    | Some _ -> errorf "Column %s is not string with options" (string_of_id id)
    | None -> errorf "No column %s" (string_of_id id)
  let get_string_opts = by_int get_string_opts_gen
  let get_string_opts_by_name = by_name get_string_opts_gen


end

type html_formatter =
  int -> string -> Html_types.td_content Tyxml.Html.elt

let to_html ?(formatters = []) d =
  let open Tyxml.Html in
  let default_cell _ s = txt s in
  let cols = List.map d.cols ~f:(fun (label, col) ->
      let cell_renderer =
        List.Assoc.find formatters ~equal:String.equal label
        |> Option.value ~default:default_cell
      in
      label, cell_renderer, col
    )
  in
  let thead =
    thead [
      tr (List.map cols ~f:(fun (label, _, _) -> td [txt label]))
    ]
  in
  let elem col i =
    let default = "NA" in
    match col with
    | Ints t -> Int.to_string t.(i)
    | Int_opts t -> Option.value_map ~default ~f:Int.to_string t.(i)
    | Floats t -> Float.to_string t.(i)
    | Float_opts t -> Option.value_map ~default ~f:Float.to_string t.(i)
    | Strings t -> t.(i)
    | String_opts t -> Option.value ~default t.(i)
  in
  let row i =
    List.map cols ~f:(fun (_, renderer, col) -> td [renderer i (elem col i)])
    |> tr
  in
  table ~thead (List.init (nrows d) ~f:row)
