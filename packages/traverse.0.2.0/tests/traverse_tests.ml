open Traverse
let () =
  let module List =
    Primitives.List.Make (Applicative.Option (Applicative.Map)) in
  assert (List.traverse (S O)
    (fun x -> Some (x + 1)) [1; 2; 3] = Some [2; 3; 4])

let () =
  let module List =
    Primitives.List.Make (Applicative.Map) in
  assert (List.traverse (S (S O))
    (fun x y -> x + y) [1; 2; 3] [4; 5; 6]= [5; 7; 9])

let () =
  let module List =
    Primitives.List.Make (Applicative.Option (Applicative.Map)) in
  let flag = ref 0 in
  assert (List.traverse (S O)
    (fun f -> if f () then Some () else None)
      [(fun () -> flag := 1; true);
        (fun () -> false);
        (fun () -> flag := 2; true)] = None);
  assert (!flag = 1)
