open Core
open Core_profiler

type 'a point =
  | Direct_point of 'a
  | Point of 'a
[@@deriving sexp, compare]

type 'id t =
  { first : 'id point
    (* i.e., penultimate point first *)
  ; rest_rev : 'id point list
  ; last : 'id
  }
[@@deriving sexp, compare]

let first t =
  match t.first with
  | Direct_point i -> i
  | Point i -> i

let last t = t.last

let direct_sep = ","
let indirect_sep = ".."

let next str pos =
  let next_pos pattern = String.substr_index str ~pos ~pattern in
  let sub pos2 = String.sub str ~pos:pos ~len:(pos2 - pos) in
  match (next_pos indirect_sep, next_pos direct_sep) with
  | (Some a, None) -> Some (Point (sub a), a + String.length indirect_sep)
  | (None, Some b) -> Some (Direct_point (sub b), b + String.length direct_sep)

  | (Some a, Some b) ->
    if a < b
    then Some (Point (sub a), a + String.length indirect_sep)
    else Some (Direct_point (sub b), b + String.length direct_sep)

  | (None, None) -> None

let string_t_of_string str =
  let (>>|) = Option.(>>|) in

  next str 0 >>| fun (first, first_pos) ->
  let rec loop pos rest_rev =
    match next str pos with
    | Some (pt, new_pos) ->
      loop new_pos (pt :: rest_rev)
    | None ->
      let last = String.sub str ~pos ~len:(String.length str - pos) in
      { last; rest_rev; first }
  in
  loop first_pos []

let string_t_to_string { first; rest_rev; last } =
  let point_to_string = function
    | Direct_point name -> [name; direct_sep]
    | Point name -> [name; indirect_sep]
  in

  (
    point_to_string first
    ::
    List.rev_map rest_rev ~f:point_to_string
    @
    [[last]]
  )
  |> List.concat
  |> String.concat



let%test_module _ = (module struct
  let check s p =
    [%test_eq: string t option] (string_t_of_string s) (Some p);
    [%test_eq: string] (string_t_to_string p) s

  let%test_unit "aaa..bbb" =
    check "aaa..bbb"
      { last = "bbb"
      ; rest_rev = []
      ; first = Point "aaa"
      }

  let%test_unit "aaa..b..cc..ddd" =
    check "aaa..b..cc..ddd"
      { first = Point "aaa"
      ; rest_rev = [Point "cc"; Point "b"]
      ; last = "ddd"
      }

  let%test_unit "aaa,bbb" =
    check "aaa,bbb"
      { first = Direct_point "aaa"
      ; rest_rev = []
      ; last = "bbb"
      }

  let%test_unit "a..b,c..d,e" =
    check "a..b,c..d,e"
      { first = Point "a"
      ; rest_rev = [Direct_point "d"; Point "c"; Direct_point "b"]
      ; last = "e"
      }

end)



let examples =
  [ { first = Point "a"; last = "b"; rest_rev = [] }
  ; { first = Direct_point "b"
    ; rest_rev = [Direct_point "d"; Point "c"]
    ; last = "e"
    }
  ]

let readme = lazy (
  let examples =
    examples
    |> List.map ~f:(fun p -> "    " ^ string_t_to_string p)
    |> String.concat
  in
  "To describe a path, specify a sequence of points. The separator determines whether \
  whether you would like to consider paths that went from a to b directly (\"a.b\"), \
  or paths that went from a to b, possibly (but not necessarily) via some other \
  points (\"a..b\"). \
  A point may not appear twice, except for when its second appearance is as the last \
  point in the path.\n\n\
  Some examples:" ^ examples)

let lookup_ids path { Util.Name_map.children = name_map; _ } =
  let lookup_point = function
    | Point p ->
      Point (Map.find_exn name_map p)
    | Direct_point p ->
      Direct_point (Map.find_exn name_map p)
  in
  { first = lookup_point path.first
  ; rest_rev = List.map ~f:lookup_point path.rest_rev
  ; last = Map.find_exn name_map path.last
  }

let lookup_names path id_map =
  let get_name = Fn.compose Reader.Header.Item.name (Reader.Header.find_exn id_map) in
  let lookup_point = function
    | Point p -> Point (get_name p)
    | Direct_point p -> Direct_point (get_name p)
  in
  { first = lookup_point path.first
  ; rest_rev = List.map ~f:lookup_point path.rest_rev
  ; last = get_name path.last
  }

let id_t_to_string path ?with_group id_map =
  begin
    match with_group with
    | Some sep ->
      let group_name = Reader.Header.((get_parent_exn id_map path.last).Item.name) in
      group_name ^ sep
    | None -> ""
  end
    ^
  string_t_to_string (lookup_names path id_map)

module I = struct
  type id_path = Probe_id.t t [@@deriving sexp, compare]
  module T = struct
    type t = id_path    [@@deriving sexp, compare]

    let hash_point = function
      | Direct_point id -> Probe_id.to_int_exn id
      | Point id -> Probe_id.to_int_exn id + (1 lsl 17 - 1)

    let hash { last; rest_rev; first } =
      let product =
        List.fold rest_rev ~init:1 ~f:(fun accum pt ->
          (accum * hash_point pt) mod (1 lsl 31 - 1)
        )
          *
        hash_point first
          *
        Probe_id.to_int_exn last
      in
      Int.hash product
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make_and_derive_hash_fold_t(T)
end
