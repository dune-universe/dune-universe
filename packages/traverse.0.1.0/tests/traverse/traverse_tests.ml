open Traverse

let () =
  assert (list Applicative.(option map) (S O)
    (fun x -> Some (x + 1)) [1; 2; 3] = Some [2; 3; 4])

let () =
  assert (list Applicative.map (S (S O))
    (fun x y -> x + y) [1; 2; 3] [4; 5; 6]= [5; 7; 9])

let () =
  let flag = ref 0 in
  assert (list Applicative.(option map) (S O)
    (fun f -> if f () then Some () else None)
      [(fun () -> flag := 1; true);
        (fun () -> false);
        (fun () -> flag := 2; true)] = None);
  assert (!flag = 1)
