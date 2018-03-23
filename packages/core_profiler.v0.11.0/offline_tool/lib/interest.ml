open Core
open Core_profiler
open Core_profiler_disabled

module Raw = struct
  type 'a t =
    | Single of 'a
    | Group_point of 'a * 'a
    | Group_path of 'a * 'a Path.t
  [@@deriving sexp, compare]

  let is_path = function
    | Single _
    | Group_point _ -> false
    | Group_path _ -> true

  module I = struct
    type id_raw_interest = Probe_id.t t [@@deriving sexp, compare]
    module T = struct
      type t = id_raw_interest    [@@deriving sexp, compare]

      let hash = function
        | Single id -> Probe_id.to_int_exn id
        | Group_point (_, id) -> Probe_id.to_int_exn id
        | Group_path (_, path) -> Path.I.hash path
    end
    include T
    include Comparable.Make(T)
    include Hashable.Make_and_derive_hash_fold_t(T)
  end
end

module Interval_subject = struct
  type t =
    | Value
    | Delta
    | Time_delta
  [@@deriving sexp, compare]

  let of_string = function
    | "v"  -> Value
    | "dv" -> Delta
    | "dt" -> Time_delta
    | _ -> failwith "Bad interval subject, expected one of v, dv or dt"

  let to_string = function
    | Value      -> "v"
    | Delta      -> "dv"
    | Time_delta -> "dt"

  let to_int = function
    | Value -> 1
    | Delta -> 2
    | Time_delta -> 3

end

type 'a t =
  | All of 'a Raw.t
  | In_interval of 'a Raw.t * Interval_subject.t * Profiler_units.t * Interval.Int.t
[@@deriving sexp]

let interval_compare a b =
  match (Interval.Int.bounds a, Interval.Int.bounds b) with
  | (None, None) -> 0
  | (Some _, None) -> 1
  | (None, Some _) -> -1
  | (Some (la, ua), Some (lb, ub)) ->
    let c = Int.compare la lb in
    if c <> 0 then c else Int.compare ua ub

let compare a_compare x y =
  match (x, y) with
  | (All rx, All ry) -> Raw.compare a_compare rx ry
  | (In_interval (rx, sx, ux, ivx), In_interval(ry, sy, uy, ivy)) ->
    let c = Raw.compare a_compare rx ry in
    if c <> 0 then c else
    let c = Interval_subject.compare sx sy in
    if c <> 0 then c else
    let c = Profiler_units.compare ux uy in
    if c <> 0 then c else
    interval_compare ivx ivy

  | (All _, In_interval _) -> -1
  | (In_interval _, All _) -> 1

let raw = function
  | All r -> r
  | In_interval (r, _, _, _) -> r

let string_t_of_sexp = t_of_sexp String.t_of_sexp

let sexp_of_string_t = sexp_of_t String.sexp_of_t

let parse_filter =
  let regex = Or_error.ok_exn (Re2.create "(v|dv|dt)\\[(.+)\\,(.+)\\]") in
  (fun str ->
     let subs = Re2.find_submatches_exn regex str in
     let subject_str = Option.value_exn subs.(1) ~message:"missing subject" in
     let left_str = Option.value_exn subs.(2) ~message:"missing left limit" in
     let right_str = Option.value_exn subs.(3) ~message:"missing right limit" in
     let subject = Interval_subject.of_string subject_str in
     (subject, Util.int_units_of_string left_str, Util.int_units_of_string right_str))

let string_t_of_string str =
  let (str, filter_part) =
    match String.rsplit2 str ~on:'~' with
    | Some (l, r) -> (l, Some r)
    | None -> (str, None)
  in
  let raw =
    let open Raw in
    match String.lsplit2 str ~on:':' with
    | None -> Single str
    | Some (group, group_interest) ->
      begin
        match Path.string_t_of_string group_interest with
        | None -> Group_point (group, group_interest)
        | Some path -> Group_path (group, path)
      end
  in
  match filter_part with
  | Some filter_part ->
    let (subject, (left, left_units), (right, right_units)) = parse_filter filter_part in
    let units = Util.choose_units left_units right_units in
    let left  = Util.coerce_units left  ~current:left_units  ~desired:units in
    let right = Util.coerce_units right ~current:right_units ~desired:units in
    let interval = Interval.Int.create left right in
    In_interval (raw, subject, units, interval)

  | None -> All raw

let string_t_to_string interest =
  let raw_to_string r =
    let open Raw in
    match r with
    | Single a -> a
    | Group_point (g, pt) -> g ^ ":" ^ pt
    | Group_path (g, pth) -> g ^ ":" ^ Path.string_t_to_string pth
  in
  match interest with
  | All raw -> raw_to_string raw
  | In_interval (raw, sub, units, interval) ->
    let sub = Interval_subject.to_string sub in
    let (l, r) = Interval.Int.bounds_exn interval in
    sprintf "%s~%s[%s,%s]"
      (raw_to_string raw)
      sub
      (Profiler_units.format_int units l)
      (Profiler_units.format_int units r)

let lookup_ids' t (name_map : Util.Name_map.t) =
  let lookup_raw r =
    let open Raw in
    match r with
    | Single name ->
      Single (Map.find_exn name_map.singles name)

    | Group_point (name, point) ->
      let group = Map.find_exn name_map.groups name in
      Group_point (group.id, Map.find_exn group.children point)

    | Group_path (name, path) ->
      let group = Map.find_exn name_map.groups name in
      Group_path (group.id, Path.lookup_ids path group)
  in
  match t with
  | All raw -> All (lookup_raw raw)
  | In_interval (raw, subject, units, interval) ->
    In_interval (lookup_raw raw, subject, units, interval)

let lookup_ids t name_map =
  try
    lookup_ids' t name_map
  with
  | Not_found_s _ | Caml.Not_found as ex ->
    Exn.reraisef ex
      "Invalid interest %s: name lookup in header failed"
      (string_t_to_string t) ()

let lookup_names t id_map =
  let get_name x = Reader.Header.get_name_exn ?with_group:None id_map x in
  let lookup_raw r =
    let open Raw in
    match r with
    | Single id -> Single (get_name id)
    | Group_point (group, point) ->
      Group_point (get_name group, get_name point)
    | Group_path (group, path) ->
      Group_path (get_name group, Path.lookup_names path id_map)
  in
  match t with
  | All raw -> All (lookup_raw raw)
  | In_interval (raw, subject, units, interval) ->
    In_interval (lookup_raw raw, subject, units, interval)

let id_t_to_string t id_map = string_t_to_string (lookup_names t id_map)

let spec interest id_map =
  match (raw interest : Probe_id.t Raw.t) with
  | Single id
  | Group_point (id, _)
  | Group_path (id, _) ->
    Reader.Header.get_spec_exn id_map id

let is_path = function
  | All raw -> Raw.is_path raw
  | In_interval (raw, _, _, _) -> Raw.is_path raw

let coerce_interval_units t id_map =
  match t with
  | All _ -> t
  | In_interval (raw, iv_subject, iv_units, interval) ->
    let desired =
      match iv_subject with
      | Value | Delta ->
        Probe_type.units (spec t id_map)
        |> Option.value_exn ~message:"Can't filter the value / delta of a Timer"
      | Time_delta ->
        Profiler_units.Nanoseconds
    in
    let interval =
      Interval.Int.map
        interval
        ~f:(Util.coerce_units ~current:iv_units ~desired)
    in
    In_interval (raw, iv_subject, desired, interval)

let examples =
  let open Raw in
  [ All (Single "probe_or_timer_name")
  ; All (Group_point ("group1", "single_point_name"))
  ; In_interval
      ( Single "some_probe"
      , Value
      , Profiler_units.Words
      , Interval.Int.create 20_000 20_100
      )
  ; In_interval
      ( Group_path ("some_group", List.nth_exn Path.examples 0)
      , Delta
      , Profiler_units.Int
      , Interval.Int.create 5 12
      )
  ; In_interval
      ( Group_path ("some_other_group", List.nth_exn Path.examples 1)
      , Time_delta
      , Profiler_units.Nanoseconds
      , Interval.Int.create 20 10_000
      )
  ]

let%test "of_to_string" =
  List.for_all examples ~f:(fun ex ->
    let ex2 = ex |> string_t_to_string |> string_t_of_string in
    ex = ex2
  )

let readme = lazy (
  [ `S "An interest is a string that specifies some subset of the core-profiler file \
        that you are interested in / wish to inspect.\n\n"
  ; `S "You can ask for an individual timer or probe by specifying its name:\n\n"
  ; `S "    "; `E (All (Single "probe_or_timer_name")); `S "\n\n"
  ; `S "And inspect the values (no deltas) of a point of a probe group like so:\n\n"
  ; `S "    "; `E (All (Group_point ("group1", "single_point_name"))); `S "\n\n"
  ; `S "To get deltas from a group, you need to describe the path you're interested in.\n"
  ; `S (Lazy.force Path.readme); `S "\n\n"
  ; `S "You may also filter the data by demanding that the value, delta or time_delta be \
        in some interval. \
        The syntax for filtering is as follows: \n\n"
  ; `S "    interest~subject[lower,upper]\n\n"
  ; `S "Where interest is a raw interest as described above, subject is one of \
        't', 'd' or 'v', and [lower, upper] specify the interval. The interval may be \
        specified using units appropriate to the probe filtered, e.g., '3ms' or \
        '1kw'.\n\n"
  ; `S "Some examples:\n\n"
  ; `S "    "; `E (List.nth_exn examples 2); `S "\n"
  ; `S "    "; `E (List.nth_exn examples 3); `S "\n"
  ; `S "    "; `E (List.nth_exn examples 4)
  ]
  |> List.map ~f:(function
    | `S s -> s
    | `E e -> string_t_to_string e
  )
  |> String.concat
)

let arg_type = Command.Spec.Arg_type.create string_t_of_string
let list_arg =
  Command.Spec.(
    anon (sequence ("interest" %: arg_type))
  )

module I = struct
  type id_interest = Probe_id.t t [@@deriving sexp, compare]
  module T = struct
    type t = id_interest    [@@deriving sexp, compare]

    let hash_in_interval subj interval =
      let subj = Interval_subject.to_int subj in
      let l = Option.value ~default:1 (Interval.Int.lbound interval) in
      let h = Option.value ~default:2 (Interval.Int.ubound interval) in
      (1 lsl 17 - 1) + subj + l + h

    let hash = function
      | All raw -> Raw.I.hash raw
      | In_interval (raw, subj, _units, interval) ->
        Int.hash (Raw.I.hash raw * hash_in_interval subj interval)
  end
  include T
  include Comparable.Make(T)
  include Hashable.Make_and_derive_hash_fold_t(T)
end

let default_interests id_map =
  let get_points group_id =
    Reader.Header.((find_group_exn id_map group_id).Item.children)
  in

  let get_points_sources group_id =
    get_points group_id
    |> List.map ~f:(fun child_id ->
      let sources = Reader.Header.((find_group_point_exn id_map child_id).Item.sources) in
      (child_id, sources)
    )
  in

  let product_fold outer ~init ~get_inner ~f =
    List.fold outer ~init ~f:(fun acc b ->
      List.fold (get_inner b) ~init:acc ~f:(fun acc a ->
        f acc a b
      )
    )
  in

  let add_group_default_interests accum group_id =
    let group_points = get_points_sources group_id in
    let some_defaults_specified =
      List.exists
        group_points
        ~f:(fun (_id, sources) -> not (List.is_empty sources))
    in

    if some_defaults_specified
    then
      product_fold
        group_points
        ~get_inner:(fun (_, sources) -> sources)
        ~init:accum
        ~f:(fun acc a (b, _) ->
          let path = Path.({ last = b; first = Point a; rest_rev = [] }) in
          All (Group_path (group_id, path)) :: acc
        )
    else
      product_fold
        group_points
        ~get_inner:(fun _ -> group_points)
        ~init:accum
        ~f:(fun acc (a, _) (b, _) ->
          let path = Path.({ last = b; first = Direct_point a; rest_rev = [] }) in
          All (Group_path (group_id, path)) :: acc
        )
  in

  Id_table.fold
    id_map
    ~init:[]
    ~f:(fun acc id header_item ->
      match header_item with
      | Single _ -> All (Single id) :: acc
      | Group _ -> add_group_default_interests acc id
      | Group_point { parent; _ } -> All (Group_point (parent, id)) :: acc
    )
