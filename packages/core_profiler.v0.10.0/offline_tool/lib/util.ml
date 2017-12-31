open Core
open Core_profiler
open Core_profiler_disabled

module Name_map = struct
  type group = { id : Probe_id.t; children : Probe_id.t String.Map.t }
  type t = { singles : Probe_id.t String.Map.t; groups : group String.Map.t }

  let of_id_map (id_map : Reader.Header.t) =
    let warn_if_duplicate what map key data =
      match Map.find map key with
      | Some _ ->
        printf !"Duplicate %s name %s; keeping %{Probe_id}" what key data
      | None -> ()
    in

    let collect_children group_id group_name =
      let warning_what = sprintf "group %s point" group_name in

      Id_table.fold
        id_map
        ~init:String.Map.empty
        ~f:(fun t id item ->
          match item with
          | Single _ | Group _ -> t
          | Group_point { name; parent; _ } ->
            if parent = group_id
            then begin
              warn_if_duplicate warning_what t name id;
              Map.set t ~key:name ~data:id
            end
            else
              t
        )
    in

    Id_table.fold
      id_map
      ~init:{ singles = String.Map.empty; groups = String.Map.empty}
      ~f:(fun t id item ->
        match item with
        | Single { name; _ } ->
          warn_if_duplicate "single" t.singles name id;
          { t with singles = Map.set t.singles ~key:name ~data:id }

        | Group { name; _ } ->
          warn_if_duplicate "group" t.groups name id;
          let data = { id; children = collect_children id name } in
          { t with groups = Map.set t.groups ~key:name ~data:data }

        | Group_point _ ->
          t
      )
end

let int_units_of_string str =
  let str = String.lowercase str in
  let len = String.length str in

  let getend n =
    if len >= n
    then str.[len - n]
    else ' '
  in

  let (multiplier, slice, units) =
    match (getend 2, getend 1) with
    | ('n', 's') -> (1. , 2, Profiler_units.Nanoseconds)
    | ('u', 's') -> (1e3, 2, Profiler_units.Nanoseconds)
    | ('m', 's') -> (1e6, 2, Profiler_units.Nanoseconds)
    | (_  , 's') -> (1e9, 1, Profiler_units.Nanoseconds)

    | ('k', 'w') -> (1e3, 2, Profiler_units.Words)
    | ('m', 'w') -> (1e6, 2, Profiler_units.Words)
    | ('g', 'w') -> (1e9, 2, Profiler_units.Words)
    | (_  , 'w') -> (1.,  1, Profiler_units.Words)

    | (_  , 'k') -> (1e3, 1, Profiler_units.Int)
    | (_  , 'm') -> (1e6, 1, Profiler_units.Int)
    | (_  , 'g') -> (1e9, 1, Profiler_units.Int)
    | _          -> (1.,  0, Profiler_units.Int)
  in

  let number = String.slice str 0 (-slice) |> Float.of_string in
  ( Float.iround_nearest_exn (number *. multiplier)
  , units
  )

let%test_unit "int_units_of_string" =
  List.iter
    [ ("5s",        5_000_000_000L,  Profiler_units.Nanoseconds)
    ; ("-12ms",     -12_000_000L,    Profiler_units.Nanoseconds)
    ; ("1.23us",    1_230L,          Profiler_units.Nanoseconds)
    ; ("50_500ns",  50_500L,         Profiler_units.Nanoseconds)

    ; ("5kw",       5_000L,          Profiler_units.Words)
    ; ("100Mw",     100_000_000L,    Profiler_units.Words)
    ; ("-1.5Gw",    -1_500_000_000L, Profiler_units.Words)
    ; ("-100_000w", -100_000L,       Profiler_units.Words)

    ; ("100k",      100_000L,        Profiler_units.Int)
    ; ("1M",        1_000_000L,      Profiler_units.Int)
    ; ("-9G",       -9_000_000_000L, Profiler_units.Int)

    ; ("-2.56",     -3L,             Profiler_units.Int)
    ; ("12",        12L,             Profiler_units.Int)
    ]
    ~f:(fun (str, num, units) ->
      let num = Int64.to_int_exn num in
      [%test_eq: int * Profiler_units.t] (int_units_of_string str) (num, units)
    )

let coerce_units value ~current ~desired =
  match (current, desired) with
  | (_, Profiler_units.Int) -> value
  | (Profiler_units.Int, _) -> value
  | (x, y) when x = y -> value
  | (Profiler_units.Seconds, Profiler_units.Nanoseconds) -> value * 1_000_000_000
  | (Profiler_units.Nanoseconds, Profiler_units.Seconds) -> value / 1_000_000_000
  | _ -> failwithf !"Don't know how to convert %{Profiler_units} to %{Profiler_units}"
           current desired ()

let choose_units a b =
  match (a, b) with
  | (Profiler_units.Seconds, Profiler_units.Nanoseconds) -> Profiler_units.Nanoseconds
  | (Profiler_units.Nanoseconds, Profiler_units.Seconds) -> Profiler_units.Nanoseconds
  | (x, y) when x = y -> x
  | (x, Profiler_units.Int) -> x
  | (Profiler_units.Int, x) -> x
  | _ -> Profiler_units.Int

let span_to_string sp =
  Profiler_units.format_int Profiler_units.Nanoseconds (Time_ns.Span.to_int_ns sp)

let time_ns_to_ofday_string t =
  let t = Time_ns.to_int_ns_since_epoch t in
  let s = t / 1_000_000_000 in
  let ns = t mod 1_000_000_000 in

  let hms =
    Time.of_span_since_epoch (Time.Span.of_sec (float s))
    |> Time.to_ofday ~zone:(force Time.Zone.local)
    |> Time.Ofday.to_sec_string
  in

  sprintf !"%s.%09i" hms ns

let choices_argtype name choices =
  (* Not only is this a fast path, but if one choice is a prefix of another
     (e.g., "time" and "time-delta") we don't want to suggest "time-delta" in
     response to "time". *)
  let fast_path = String.Map.of_alist_exn choices in
  let search prefix =
    match Map.find fast_path prefix with
    | Some d -> [(prefix, d)]
    | None ->   List.filter choices ~f:(fun (key, _) -> String.is_prefix key ~prefix)
  in
  Command.Spec.Arg_type.create
    ~complete:(fun _ ~part -> List.map ~f:fst (search part))
    (fun s ->
       match search s with
       | [(_key, data)] -> data
       | [] -> failwithf "Unrecognised %s %s" name s ()
       | (x, _)::(y, _)::others ->
         let more =
           match others with
           | [] -> ""
           | _ -> ", ..."
         in
         failwithf "Ambiguous %s %s: could be %s, %s%s" name s x y more ()
    )
