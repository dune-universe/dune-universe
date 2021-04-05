open Zlist

(* [items] *)
let () = assert (strict (items [1; 2; 3]) = [1; 2; 3])

(* [of_array] *)
let () = assert (strict (of_array [|1; 2; 3|]) = [1; 2; 3])

(* [unit] *)
let () = assert (strict (unit 1) = [1])

(* [fill] *)
let () = assert (strict (fill 3 10) = [10; 10; 10])

(* [unfold] *)
let () =
  assert (
    strict (unfold 0 (fun i -> if i < 3 then Some (i + 1, 10) else None))
    = [10; 10; 10] )

(* [iterate] *)
let () = assert (strict (iterate 0 (fun x -> x + 1) |> take 5) = [0; 1; 2; 3; 4])

(* [continually] *)
let () = assert (strict (continually 1 |> take 3) = [1; 1; 1])

(* [enum_from] *)
let () = assert (strict (enum_from 0 |> take 3) = [0; 1; 2])

(* [enum_from_to] *)
let () = assert (strict (enum_from_to 0 3) = [0; 1; 2; 3])

(* [cycle] *)
let () =
  assert (strict (cycle (items [1; 2; 3]) |> take 7) = [1; 2; 3; 1; 2; 3; 1])

(* [head] *)
let () =
  assert (head (items [1; 2; 3]) = Some 1) ;
  assert (head (lazy Nil) = None)

(* [tail] *)
let () = assert (strict (tail (items [1; 2; 3])) = [2; 3])

(* [take_while] *)
let () =
  assert (strict (items [1; 2; 3; 4; 5] |> take_while (fun x -> x <> 3)) = [1; 2]
  )

(* [drop] *)
let () = assert (strict (items [1; 2; 3; 4] |> drop 2) = [3; 4])

(* [drop_while] *)
let () =
  assert (strict (items [1; 3; 2; 5] |> drop_while (fun x -> x <> 2)) = [2; 5])

(* [map] *)
let () =
  assert (strict (continually 0 |> map (fun x -> x + 1) |> take 3) = [1; 1; 1])

(* [flat_map] *)
let () =
  assert (
    strict (enum_from 0 |> flat_map (fun x -> items [x; x + 1]) |> take 6)
    = [0; 1; 1; 2; 2; 3] )

(* [filter] *)
let () =
  assert (
    strict (cycle (items [1; 2]) |> filter (fun x -> x < 2) |> take 3)
    = [1; 1; 1] )

(* [map_filter] *)
let () =
  assert (
    strict
      ( enum_from_to 0 10
      |> map_filter (fun x -> if x < 5 then Some (10 * x) else None) )
    = [0; 10; 20; 30; 40] )

(* [flatten] *)
let () =
  assert (
    strict (continually (items [1; 2; 3]) |> flatten |> take 5) = [1; 2; 3; 1; 2]
  )

(* [exists] *)
let () =
  assert (exists (fun x -> x = 1) (items [1; 2; 3])) ;
  assert (not @@ exists (fun x -> x = 1) (items [2; 3]))

(* [for_all] *)
let () =
  assert (for_all (fun _ -> false) (lazy Nil)) ;
  assert (for_all (fun x -> x mod 2 = 0) (items [0; 2; 4])) ;
  assert (not @@ for_all (fun x -> x mod 2 = 0) (items [0; 1; 4]))

(* [find] *)
let () =
  assert (find (fun x -> x = 3) (items [1; 3; 5]) = Some 3) ;
  assert (find (fun x -> x > 100) (items [10; 20]) = None)

(* [concat] *)
let () =
  assert (
    strict (concat (items [1; 2; 3]) (items [4; 5; 6])) = [1; 2; 3; 4; 5; 6] )

(* [zip_with] *)
let () =
  assert (
    strict (zip_with (fun x y -> x + y) (items [1; 2; 3]) (items [10; 20; 30]))
    = [11; 22; 33] )

(* [zip] *)
let () =
  assert (
    strict (zip (items [1; 2; 3]) (items [10; 20; 30]))
    = [(1, 10); (2, 20); (3, 30)] )

(* [zip_all_with] *)
let () =
  assert (
    strict
      (zip_all_with
         (fun x y -> match (x, y) with Some x, Some y -> x + y | _ -> -1)
         (items [2; 3])
         (items [10; 20; 30]))
    = [12; 23; -1] )

(* [zip_all] *)
let () =
  assert (
    strict (zip_all (items [1; 2]) (items [1; 2; 3]))
    = [(Some 1, Some 1); (Some 2, Some 2); (None, Some 3)] )

(* [strict] *)
let () = assert (strict (items [1; 2; 3]) = [1; 2; 3])

(* [fold_right] *)
let () =
  assert (
    fold_right (fun x n -> x + Lazy.force n) (enum_from_to 1 10) (lazy 0) = 55
  )

(* [fold_left] *)
let () = assert (fold_left (fun n x -> n + x) 0 (enum_from_to 1 10) = 55)

(* [length] *)
let () = assert (length (items [1; 2; 3]) = 3)

(* [equal] *)
let () =
  assert (equal ( = ) (items [1; 2; 3]) (items [1; 2; 3])) ;
  assert (not @@ equal ( = ) (items [1; 2; 3]) (continually 1))
