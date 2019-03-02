open Core_kernel

module Color = struct
  type t =
    | Red
    | Green
    | Plain
  [@@deriving sexp_of, compare]

  let equal = [%compare.equal: t]
end

module Line = struct
  type t =
    { color : Color.t
    ; content : string
    }
  [@@deriving fields, sexp_of]

  let empty = { color = Color.Plain; content = "" }

  let to_text ~green ~red ~plain t =
    let formatting =
      match t.color with
      | Red -> red
      | Green -> green
      | Plain -> plain
    in
    formatting t.content
  ;;

  let plain content = { color = Plain; content }
  let red content = { color = Red; content }
  let green content = { color = Green; content }
  let length t = String.length t.content
  let first t = String.find t.content ~f:(fun x -> x <> ' ') |> Option.value_exn
  let last t = t.content.[String.length t.content - 1]

  let concat a b =
    assert (Color.equal a.color b.color);
    let b = String.lstrip b.content in
    let { color; content } = a in
    let content =
      if last a = '(' || b.[0] = ')' then content ^ b else content ^ " " ^ b
    in
    { color; content }
  ;;
end

module Linear_diff = struct
  type t =
    | Same_open_paren
    | Same_close_paren
    | Same of Sexp.t
    | Add of Sexp.t
    | Delete of Sexp.t
    | Replace of (Sexp.t * Sexp.t)
  [@@deriving sexp_of]

  let rec of_diff = function
    | Diff.Same x -> [ Same x ]
    | Add x -> [ Add x ]
    | Delete x -> [ Delete x ]
    | Replace (x, y) -> [ Replace (x, y) ]
    | Enclose xs -> [ Same_open_paren ] @ List.bind xs ~f:of_diff @ [ Same_close_paren ]
  ;;
end

module Display_options = struct
  type t =
    { collapse_threshold : int
    ; num_shown : int
    }
  [@@deriving sexp_of, fields]

  let create ?(collapse_threshold = 10) ?(num_shown = 3) () =
    Fields.create ~collapse_threshold ~num_shown
  ;;

  let default = create ()
end

module Line_pair = struct
  type t =
    | Same of Line.t
    | Different of (Line.t * Line.t)

  let fst = function
    | Same x -> x
    | Different (x, _) -> x
  ;;

  let snd = function
    | Same x -> x
    | Different (_, x) -> x
  ;;

  let is_same = function
    | Same _ -> true
    | Different _ -> false
  ;;
end

module Hideable_line_pair = struct
  type t =
    | Line_pair of Line_pair.t
    | Hidden of int
    | All_hidden
end

let spaces ~indentation = String.make (indentation * 1) ' '

let sexp_to_lines ~indentation sexp =
  let spaces = spaces ~indentation in
  Sexp.to_string_hum sexp |> String.split_lines |> List.map ~f:(fun x -> spaces ^ x)
;;

let same x = Line_pair.Same (Line.plain x)

let diff_to_lines ~indentation = function
  | Linear_diff.Same_open_paren -> [ same (spaces ~indentation ^ "(") ]
  | Same_close_paren -> [ same (spaces ~indentation:(indentation - 1) ^ ")") ]
  | Same sexp ->
    let lines = sexp_to_lines ~indentation sexp in
    List.map lines ~f:same
  | Add sexp ->
    let lines = sexp_to_lines ~indentation sexp in
    List.map lines ~f:(fun x -> Line_pair.Different (Line.empty, Line.green x))
  | Delete sexp ->
    let lines = sexp_to_lines ~indentation sexp in
    List.map lines ~f:(fun x -> Line_pair.Different (Line.red x, Line.empty))
  | Replace (sexp_a, sexp_b) ->
    let rec loop ~lines_a ~lines_b ~acc =
      match lines_a, lines_b with
      | [], [] -> List.rev acc
      | a :: lines_a, [] ->
        let elt = Line_pair.Different (Line.red a, Line.empty) in
        loop ~lines_a ~lines_b ~acc:(elt :: acc)
      | [], b :: lines_b ->
        let elt = Line_pair.Different (Line.empty, Line.green b) in
        loop ~lines_a ~lines_b ~acc:(elt :: acc)
      | a :: lines_a, b :: lines_b ->
        let elt = Line_pair.Different (Line.red a, Line.green b) in
        loop ~lines_a ~lines_b ~acc:(elt :: acc)
    in
    let lines_a = sexp_to_lines ~indentation sexp_a in
    let lines_b = sexp_to_lines ~indentation sexp_b in
    loop ~lines_a ~lines_b ~acc:[]
;;

let diff_to_indentation_delta = function
  | Linear_diff.Same _ | Replace _ | Add _ | Delete _ -> 0
  | Same_open_paren -> 1
  | Same_close_paren -> -1
;;

let combine a b =
  match a, b with
  | Line_pair.Different _, _
  | _, Line_pair.Different _ -> None
  | Line_pair.Same a, Line_pair.Same b ->
    let combine () = Some (Line_pair.Same (Line.concat a b)) in
    if Line.first b = ')'
    then combine ()
    else if Line.last a = '('
    then combine ()
    else None
;;

let combine_lines lines =
  List.fold_right lines ~init:[] ~f:(fun line lines ->
    match line, lines with
    | a, b :: rest ->
      (match combine a b with
       | None -> a :: b :: rest
       | Some x -> x :: rest)
    | line, lines -> line :: lines)
;;

let hide_lines ~display_options lines =
  if List.for_all lines ~f:Line_pair.is_same
  then [ Hideable_line_pair.All_hidden ]
  else (
    let combined =
      List.fold_right lines ~init:[] ~f:(fun line lines ->
        match line, lines with
        | Line_pair.Same _, (Line_pair.Same _ :: _ as list) :: rest ->
          (line :: list) :: rest
        | _ -> [ line ] :: lines)
    in
    List.map combined ~f:(fun lines ->
      let lines = List.map lines ~f:(fun x -> Hideable_line_pair.Line_pair x) in
      let num_shown = Display_options.num_shown display_options in
      if List.length lines >= Display_options.collapse_threshold display_options
      && (num_shown * 2) + 1 < List.length lines
      then (
        let start = List.take lines num_shown in
        let end_ = List.rev (List.take (List.rev lines) num_shown) in
        let num_hidden = List.length lines - List.length start - List.length end_ in
        start @ [ Hideable_line_pair.Hidden num_hidden ] @ end_)
      else lines)
    |> List.concat)
;;

let display ?(display_options = Display_options.default) changes =
  let indentation = 0 in
  let indentation, lines =
    List.fold_map changes ~init:indentation ~f:(fun indentation change ->
      let lines = diff_to_lines ~indentation change in
      let delta = diff_to_indentation_delta change in
      let indentation = indentation + delta in
      indentation, lines)
  in
  assert (indentation = 0);
  List.concat lines |> combine_lines |> hide_lines ~display_options
;;

let hide_message ~num_hidden = sprintf "...%d unchanged lines..." num_hidden
let all_hidden_message = "(no changes)"

let display ?display_options diff ~on_hidden ~on_all_hidden ~on_line_pair =
  let diff = Linear_diff.of_diff diff in
  let lines = display ?display_options diff in
  let length ~project =
    List.map lines ~f:(function
      | Hideable_line_pair.Hidden num_hidden ->
        String.length (hide_message ~num_hidden)
      | All_hidden -> String.length all_hidden_message
      | Line_pair x -> Line.length (project x))
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let left_length = length ~project:Line_pair.fst in
  let right_length = length ~project:Line_pair.snd in
  let pad_to_left = left_length + 2 in
  let pad_to_right = right_length in
  let width = pad_to_left + pad_to_right in
  List.map lines ~f:(function
    | Hideable_line_pair.Hidden num_hidden -> on_hidden ~num_hidden ~width
    | Hideable_line_pair.All_hidden -> on_all_hidden ~width
    | Line_pair line_pair ->
      let left = Line_pair.fst line_pair in
      let right = Line_pair.snd line_pair in
      let left_padding = String.make (pad_to_left - Line.length left) ' ' in
      let right_padding = String.make (pad_to_right - Line.length right) ' ' in
      on_line_pair ~left ~right ~left_padding ~right_padding)
;;
