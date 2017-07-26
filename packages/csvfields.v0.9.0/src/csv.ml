open! Core

module Helper = struct

  let prepend_name acc field = (Field.name field) :: acc

  let add x acc _field = x + acc

  (*let separator = ","

  let newline = "\n"*)

  let write ~is_first:_ ~is_last:_ ~writer ~to_string _ _ field =
    writer (to_string field);

end

module type Stringable = sig
  type t
  val to_string : t -> string
  val of_string : string -> t
end

module Spec = struct
  exception Illegal_csv_file
  type t = Leaf of string | Tree of (string * t list)

  let rec depth (t : t) =
    match t with
    | Leaf _ -> 1
    | Tree (_, children) ->
        List.fold_left ~f:(fun acc child -> max acc ((depth child) + 1)) ~init:0 children

  let depth t =
    (List.fold_left ~f:(fun acc t -> max acc (depth t)) ~init:0 t)

  let rec matches' (parent_rows, current_header, children_rows) (t : t) =
    match t, current_header with
    | (Tree _ | Leaf _), [] -> raise Illegal_csv_file
    | Leaf spec_title, (real_title :: rest_of_header) ->
        if real_title <> spec_title then raise Illegal_csv_file
        else
          List.map ~f:List.tl_exn parent_rows,
          rest_of_header,
          List.map ~f:List.tl_exn children_rows
    | (Tree (spec_title, children)), (real_title :: _) ->
        if spec_title = real_title then
          match children_rows with
          | [] -> raise Illegal_csv_file
          | h :: children_rows ->
              let init = (current_header :: parent_rows), h, children_rows in
              let parent_rows, current_header, children_rows =
                List.fold_left ~f:matches' ~init children in
              match parent_rows with
              | h :: parent_rows -> parent_rows, h, (current_header :: children_rows)
              | [] -> raise Illegal_csv_file
        else raise Illegal_csv_file

  let matches header_of_csv t =
    match header_of_csv with
    | [] -> raise Illegal_csv_file
    | current_header :: children_rows ->
        let init = ([], current_header, children_rows) in
        try ([], [], []) = List.fold_left ~f:matches' ~init t with
        | _exn -> false

  let check ~csv ~header:t ~f =
    let depth = depth t in
    let rec aux depth acc csv =
      if depth = 0 then
        if matches (List.rev acc) t then f csv
        else raise Illegal_csv_file
      else
        match csv with
        | h :: csv -> aux (depth - 1) (h :: acc) csv
        | [] -> raise Illegal_csv_file
    in
    aux depth [] csv


  let prepend result depth headers =
    let len = Array.length result in
    for i = len - 1 downto 0 do
      let to_prepend =
        if i > depth || Stack.is_empty headers then
            ""
        else Stack.pop_exn headers
      in
      result.(i) <- to_prepend :: result.(i);
    done

  let str_to_human_readable str =
    let str =
      String.map str ~f:(function
        | '_' -> ' '
        | c -> c)
    in
    String.capitalize str

  let rec header_of_t ~depth ~result ~parents t =
    match t with
    | Leaf str ->
        let str = str_to_human_readable str in
        Stack.push parents str;
        prepend result depth parents
    | Tree (str, children) ->
        let str = str_to_human_readable str in
        let depth = depth + 1 in
        Stack.push parents str;
        header_of_list ~depth ~result ~parents children

  and header_of_list ~depth ~result ~parents lst =
    match lst with
    | [] -> assert false
    | h :: t ->
        List.iter
          ~f:(fun x -> header_of_t ~depth ~result ~parents:(Stack.create ()) x)
        (List.rev t);
        header_of_t ~depth ~result ~parents h

  let header lst =
    let depth = depth lst in
    let result = Array.create ~len:depth [] in
    header_of_list ~depth:0 ~result ~parents:(Stack.create ()) lst;
    Array.to_list result

end

module type Csvable_simple = sig
  type t
  val is_csv_atom : bool
  val rev_csv_header' : string list -> _ -> _ -> string list
  val rev_csv_header_spec' : Spec.t list -> _ -> _ -> Spec.t list
  val t_of_row' : _ -> string list -> (unit -> t) * (string list)
  val write_row_of_t' :
    is_first:bool -> is_last:bool -> writer:(string -> unit) -> _ -> _ -> t -> unit
end

module type Csvable = sig
  include Csvable_simple
  val csv_header : string list
  val csv_header_spec : Spec.t list
  val t_of_row : string list -> t
  val row_of_t : t -> string list
  val csv_load : ?separator:char -> string -> t list
  val csv_load_in : ?separator:char -> In_channel.t -> t list
  val csv_save_fn : ?separator:char -> (string -> unit) -> t list -> unit
  val csv_save_out : ?separator:char -> Out_channel.t -> t list -> unit
  val csv_save : ?separator:char -> string -> t list -> unit
end

exception Excess_of_elements_in_row of string list

module Record (S : Csvable_simple) : Csvable with type t = S.t = struct
  include S

  let rev_csv_header' reverse_headers _ _ =
    if S.is_csv_atom then reverse_headers
    else S.rev_csv_header' reverse_headers () ()

  let rev_csv_header_spec' specs _ _ =
    if S.is_csv_atom then specs
    else S.rev_csv_header_spec' specs () ()

  let csv_header = List.rev (rev_csv_header' [] () ())

  let rec aux_csv_header_spec (t : Spec.t) =
    match t with
    | Spec.Leaf _ -> t
    | Spec.Tree (name, children) ->
        Spec.Tree (name, aux_csv_header_spec' children)

  and aux_csv_header_spec' lst =
    List.rev_map ~f:aux_csv_header_spec lst

  let csv_header_spec = aux_csv_header_spec' (rev_csv_header_spec' [] () ())

  let of_list_without_tail aux strings =
    match aux () strings with
    | f, [] -> f ()
    | _, lst -> raise (Excess_of_elements_in_row lst)

  let t_of_row strings = of_list_without_tail S.t_of_row' strings

  (* This is not being used anymore really *)
  (*let write_row_of_t ~writer csvable =
    S.write_row_of_t' ~is_first:true ~is_last:true ~writer () () csvable*)

  let row_of_t csvable =
    let list = ref [] in
    let writer str = list := str :: !list in
    S.write_row_of_t' ~is_first:true ~is_last:true ~writer () () csvable;
    List.rev !list

  let csv_save_fn ?separator writer csvable =
    Csvlib.Csv.save_fn ?separator writer (List.map ~f:row_of_t csvable)

  let csv_save_out ?separator channel csvable =
    Csvlib.Csv.save_out ?separator channel  (List.map ~f:row_of_t csvable)

  let csv_save ?separator channel csvable =
    Csvlib.Csv.save ?separator channel (List.map ~f:row_of_t csvable)

  let csv_load_in ?separator channel =
    let list = Csvlib.Csv.load_in ?separator channel in
    List.map ~f:t_of_row list

  let csv_load ?separator file =
    let list = Csvlib.Csv.load ?separator file in
    List.map ~f:t_of_row list

end

exception Incomplete_row

module Make_csvable_simple (S : Stringable) : Csvable_simple with type t = S.t =
  struct

    type t = S.t

    let is_csv_atom = true

    let rev_csv_header' acc _ _ = acc

    let rev_csv_header_spec' acc _ _ = acc

    let t_of_row' _ strings =
      match strings with
      | [] -> raise Incomplete_row
      | value :: csvable -> (fun () -> S.of_string value), csvable

    let write_row_of_t' ~is_first ~is_last ~writer _ _ csvable =
      Helper.write ~is_first ~is_last ~writer ~to_string:S.to_string () () csvable

end

module Atom (S : Stringable) : Csvable with type t = S.t =
  Record (Make_csvable_simple (S))


let use_head f lst =
  match lst with
  | [] -> raise Incomplete_row
  | h :: t -> (fun _ -> (f h)), t

exception Illegal_atom of string

(** All the conversion functions *)
let unit_of_row  _ strings =
  use_head (fun string -> if string = "" then () else raise (Illegal_atom string)) strings

let bool_of_row _ strings =
  use_head (fun h ->
    let h = String.uppercase h in
    h = "TRUE") strings

let string_of_row _ strings =
  use_head (fun h -> h) strings

let char_of_row _ strings =
  use_head (fun string ->
    if (String.length string) = 1 then String.get string 0 else raise (Illegal_atom string))
    strings

let int_of_row _ strings =
  use_head int_of_string strings

let float_of_row _ strings =
  use_head float_of_string strings

let int32_of_row _ strings =
  use_head Int32.of_string strings

let int64_of_row _ strings =
  use_head Int64.of_string strings

let nativeint_of_row _ strings =
  use_head Nativeint.of_string strings

let big_int_of_row _ strings =
  use_head Big_int.big_int_of_string strings

let nat_of_row _ strings =
  use_head Nat.nat_of_string strings

let num_of_row _ strings =
  use_head Num.num_of_string strings

let ratio_of_row _ strings =
  use_head Ratio.ratio_of_string strings

type ('a, 'b, 'c) row_of =
  is_first:bool
  -> is_last:bool
  -> writer:(string -> unit)
  -> 'b
  -> 'c
  -> 'a
  -> unit

let write is_first is_last writer to_string t =
  Helper.write ~is_first ~is_last ~writer ~to_string () () t

let row_of_unit ~is_first ~is_last ~writer _ _ t =
  let f () = "" in
  write is_first is_last writer f t

let row_of_bool ~is_first ~is_last ~writer _ _ t =
  let f bool = if bool then "true" else "false" in
  write is_first is_last writer f t

let row_of_string ~is_first ~is_last ~writer _ _ t =
  let f string = string in
  write is_first is_last writer f t

let row_of_char ~is_first ~is_last ~writer _ _ t =
  let f char = String.make 1 char in
  write is_first is_last writer f t

let row_of_int ~is_first ~is_last ~writer _ _ t =
  write is_first is_last writer string_of_int t

let row_of_float ~is_first ~is_last ~writer _ _ t =
  write is_first is_last  writer string_of_float t

let row_of_int32 ~is_first ~is_last  ~writer _ _ t =
  write is_first is_last  writer Int32.to_string t

let row_of_int64 ~is_first ~is_last  ~writer _ _ t =
  write is_first is_last  writer Int64.to_string t

let row_of_nativeint ~is_first ~is_last  ~writer _ _ t =
  write is_first is_last  writer Nativeint.to_string t

let row_of_big_int ~is_first ~is_last  ~writer _ _ t =
  write is_first is_last  writer Big_int.string_of_big_int t

let row_of_nat ~is_first ~is_last  ~writer _ _ t =
  write is_first is_last  writer Nat.string_of_nat t

let row_of_num ~is_first ~is_last  ~writer _ _ t =
  write is_first is_last  writer Num.string_of_num t

let row_of_ratio ~is_first ~is_last  ~writer _ _ t =
  write is_first is_last  writer Ratio.string_of_ratio t
