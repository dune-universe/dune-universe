open Alcotest
open Interval_map
module Ivl_map = Make (Int)
module Ivl = Ivl_map.Interval

let create_and_add () =
  let map =
    Ivl_map.empty
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
    |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
    |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
    |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
    |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
  in
  check int "expected size" 6 (Ivl_map.size map)

let remove () =
  let map =
    Ivl_map.empty
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
    |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
    |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
    |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
    |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
  in
  let map =
    Ivl_map.remove_by
      (Ivl.create (Included 0) (Excluded 10))
      (fun v -> v = "foo")
      map
  in
  check int "expected size" 5 (Ivl_map.size map);
  let map =
    Ivl_map.remove_by
      (Ivl.create (Included 0) (Excluded 10))
      (fun v -> v = "foo2")
      map
  in
  check int "expected size" 4 (Ivl_map.size map);
  let map =
    Ivl_map.remove_by
      (Ivl.create (Excluded 0) (Included 10))
      (fun v -> v = "bar")
      map
  in
  check int "expected size" 3 (Ivl_map.size map);
  let map = Ivl_map.remove_interval (Ivl.create Unbounded (Excluded 4)) map in
  check int "expected size" 2 (Ivl_map.size map);
  let map =
    Ivl_map.remove_interval (Ivl.create (Excluded 4) (Excluded 10)) map
  in
  check int "expected size" 1 (Ivl_map.size map);
  let map =
    Ivl_map.remove_interval (Ivl.create (Included 5) (Included 10)) map
  in
  check int "expected size" 0 (Ivl_map.size map)

let generator () =
  let module Gen = Ivl_map.Gen in
  let map =
    Ivl_map.empty
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
    |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
    |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
    |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
    |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
    |> Ivl_map.add (Ivl.create (Excluded 0) Unbounded) "zip"
  in
  let check_next_exists value next =
    check bool "has next" true (Option.is_some next);
    let (_ivl, values), gen = Option.get next in
    check (list string) "next value" value (List.sort String.compare values);
    gen
  in
  let rec check_produces values gen =
    let next = Gen.next gen in
    match values with
    | hd :: tl ->
      let gen = check_next_exists hd next in
      check_produces tl gen
    | [] ->
      check bool "is exhausted" false (Option.is_some next)
  in
  (* Ascending *)
  let gen = Ivl_map.generator ~order:Asc map in
  check_produces
    [ [ "zab" ]; [ "foo"; "foo2" ]; [ "bar" ]; [ "zip" ]; [ "oof" ]; [ "baz" ] ]
    gen;
  (* Descending *)
  let gen = Ivl_map.generator ~order:Desc map in
  check_produces
    [ [ "baz" ]; [ "oof" ]; [ "zip" ]; [ "bar" ]; [ "foo"; "foo2" ]; [ "zab" ] ]
    gen

let map () =
  let map =
    Ivl_map.empty
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
    |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
    |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
    |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
    |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
    |> Ivl_map.add (Ivl.create (Excluded 0) Unbounded) "zip"
  in
  let map =
    Ivl_map.mapi
      (fun _ivl values -> "pre" :: List.sort String.compare values)
      map
  in
  let res =
    Ivl_map.to_list map
    |> List.sort (fun (ivl1, _) (ivl2, _) -> Ivl.compare ivl1 ivl2)
    |> List.map snd
  in
  check
    (list (list string))
    "map"
    [ [ "pre"; "zab" ]
    ; [ "pre"; "foo"; "foo2" ]
    ; [ "pre"; "bar" ]
    ; [ "pre"; "zip" ]
    ; [ "pre"; "oof" ]
    ; [ "pre"; "baz" ]
    ]
    res

let find_and_mem () =
  let map =
    Ivl_map.empty
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
    |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
    |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
    |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
    |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
    |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
  in
  check
    bool
    "mem should not be found"
    false
    (Ivl_map.mem (Ivl.create (Included 0) (Included 10)) map);
  check
    bool
    "mem should be found"
    true
    (Ivl_map.mem (Ivl.create (Included 0) (Excluded 10)) map);
  check
    bool
    "mem should be found"
    true
    (Ivl_map.mem (Ivl.create Unbounded (Excluded 4)) map);
  let res = Ivl_map.find_opt (Ivl.create (Included 0) (Excluded 10)) map in
  let res = Option.map (fun xs -> List.sort String.compare xs) res in
  check (option (list string)) "find result" (Some [ "foo"; "foo2" ]) res;
  check_raises "not found" Not_found (fun () ->
      let _ = Ivl_map.find (Ivl.create Unbounded (Included 4)) map in
      ())

let query () =
  let open Ivl_map.Bound in
  Random.self_init ();
  let rand_bound bound_value =
    let bound_roll = Random.int 100 in
    if bound_roll >= 90 then
      Unbounded
    else if bound_roll >= 45 then
      Excluded bound_value
    else
      Included bound_value
  in
  let rand_ivl () =
    let bv1 = Random.int 100 in
    let bv2 = bv1 + Random.int 20 - 10 in
    let b1 = rand_bound bv1 in
    let b2 = rand_bound bv2 in
    let low, high =
      if compare_lower b1 b2 <= 0 then
        b1, b2
      else
        b2, b1
    in
    Ivl.create low high
  in
  (* build the map with random intervals *)
  let ivls = ref [] in
  let map = ref Ivl_map.empty in
  let c = ref 0 in
  while !c < 1000 do
    try
      let ivl = rand_ivl () in
      ivls := ivl :: !ivls;
      map := Ivl_map.add ivl (Random.int 10) !map;
      c := !c + 1
    with
    | Invalid_interval ->
      ()
  done;
  (* query the map with random intervals *)
  c := 0;
  while !c < 1000 do
    try
      let query = rand_ivl () in
      let expected_count =
        List.fold_left
          (fun acc ivl -> if Ivl.overlaps query ivl then acc + 1 else acc)
          0
          !ivls
      in
      let results_count =
        Ivl_map.query_interval query !map
        |> Ivl_map.Gen.fold (fun acc _ xs -> acc + List.length xs) 0
      in
      check int "same number of query results" expected_count results_count;
      c := !c + 1
    with
    | Invalid_interval ->
      ()
  done

let suite =
  [ "create and add", `Quick, create_and_add
  ; "remove", `Quick, remove
  ; "generator", `Quick, generator
  ; "find and mem", `Quick, find_and_mem
  ; "query", `Quick, query
  ; "map", `Quick, map
  ]
