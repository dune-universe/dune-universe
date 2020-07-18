(**/**)

open Wktxt_type
(* pair_list : ((bool, int), inlines) *)

let rec get_pair_list_from_depth depth pair_list =
  match pair_list with
  | ((_, d), _) :: tl when d > depth -> get_pair_list_from_depth depth tl
  | list -> list

let rec parse_list depth pair_list list_type : block list list =
  let build_list l_type l_content =
    if l_type = Unordered then List l_content
    else NumList l_content
  in match pair_list with
  | ((cur_type, next_depth), _) :: _ when next_depth = 1 && cur_type <> list_type && depth <> 0 ->
    []
  | ((cur_type, next_depth), inlines) :: tl1 when depth = next_depth ->
    if cur_type <> list_type then
      prerr_string "Warning : Two list items of different type have been declared on the same level.\n"
      ;
    begin match tl1 with
      | ((next_type, d'), _) :: tl2 when next_depth < d' ->
        [Paragraph (List.flatten inlines) ; build_list next_type (parse_list (depth + 1) tl1 next_type )]
          :: parse_list depth (get_pair_list_from_depth depth tl2) list_type
      | _ ->
        [Paragraph (List.flatten inlines)]
          :: parse_list depth (get_pair_list_from_depth depth tl1) list_type
    end
  | ((next_type, _), _) :: tl when depth = 0 ->
    let rec get_next_list l_type l =
      match l with
      | ((n_type, l_depth), _) :: tl when n_type = next_type || l_depth > 1 -> get_next_list l_type tl
      | list -> list
    in [build_list next_type (parse_list (depth + 1) pair_list next_type)]
      :: parse_list depth (get_next_list next_type tl) next_type
  | ((next_type, next_depth), _) :: tl when depth < next_depth ->
    [build_list next_type (parse_list (depth + 1) pair_list next_type)]
      :: parse_list depth (get_pair_list_from_depth depth tl) list_type
  | _ -> []

let parse_list d l t =
 parse_list d l t
 |> List.flatten

let rec get_next_term_list l depth =
  match l with
  | ((cur_type, cur_depth),_) :: tl when cur_type = Description || cur_depth > depth ->
    get_next_term_list tl depth
  | list -> list

let rec get_descriptions l depth :(block list)=
  match l with
  | ((cur_type, cur_depth), inlines) :: tl when cur_type = Description && cur_depth = depth ->
    Paragraph (List.flatten inlines) :: get_descriptions tl depth
  | ((_, cur_depth), _) :: tl when cur_depth > depth ->
    DefList (get_def_blocks l (depth + 1)) ::
      get_descriptions (get_pair_list_from_depth depth tl) depth
  | _ -> []

and get_def_blocks l depth :(def_block list)=
  match l with
  | ((cur_type, cur_depth), inlines) :: tl when cur_type = Term && cur_depth = depth ->
    (List.flatten inlines, get_descriptions tl depth) ::
      get_def_blocks (get_next_term_list tl depth) depth
  | ((_, cur_depth), _) :: tl when cur_depth >= depth ->
    ([], get_descriptions l depth) :: get_def_blocks (get_next_term_list tl depth) depth
  | _ -> []

let rec get_table_line line :(table_block list)=
  match line with
  | (cell_type, inlines) :: tl when cell_type = TableHeader -> TableHead (List.flatten inlines) :: get_table_line tl
  | (_, inlines) :: tl -> TableItem (List.flatten inlines) :: get_table_line tl
  | _ -> []
