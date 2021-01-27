
open Flex_array

let eqint = Alcotest.(check int)

let pp fmt a =
  Format.fprintf fmt "@[[";
  for i = 0 to length a - 1 do
    Format.fprintf fmt "%d" (get a i);
    if i < length a - 1 then Format.fprintf fmt ",@ "
  done;
  Format.fprintf fmt "@]]"

let fa : int t Alcotest.testable = Alcotest.testable pp (=)
let eq_fa = Alcotest.(check fa)
let raises = Alcotest.check_raises

let test0 () =
  raises "make" (Invalid_argument "make") (fun () -> ignore (make (-1) 0));
  let a = empty in
  raises "get" (Invalid_argument "get") (fun () -> get a 0);
  raises "set" (Invalid_argument "set") (fun () -> ignore (set a 0 1));
  raises "tail" (Invalid_argument "tail") (fun () -> ignore (tail a));
  raises "liat" (Invalid_argument "liat") (fun () -> ignore (liat a));
  let a = make 10 0 in
  raises "get" (Invalid_argument "get") (fun () -> ignore (get a (-1)));
  raises "get" (Invalid_argument "get") (fun () -> ignore (get a 10));
  raises "set" (Invalid_argument "set") (fun () -> ignore (set a (-1) 1));
  raises "set" (Invalid_argument "set") (fun () -> ignore (set a 10 1));
  ()

let test1 m =
  let rec fill a i = if i = m then a else fill (snoc a i) (i + 1) in
  let a = fill empty 0 in
  eqint "length" m (length a);
  let a = cons 0 a in
  eqint "length cons" (m + 1) (length a);
  let a = tail a in
  eqint "length tail" m (length a);
  let a = snoc a m in
  eqint "length snoc" (m + 1) (length a);
  let a = liat a in
  eqint "length snoc" m (length a);
  for i = 0 to m - 1 do eqint "get" i (get a i) done;
  let next = ref 0 in
  (iter (fun j -> eqint "iter" !next j; incr next)) a;
  next := 0;
  (iteri (fun i j ->
       eqint "iteri" !next i; eqint "iteri" !next j; incr next)) a;
  let l = fold (fun acc x -> x :: acc) [] a in
  eqint "fold length" (List.length l) m;
  if m > 0 then eqint "fold order" (List.hd l) (m - 1);
  let l = foldi (fun acc i x -> eqint "foldi" i x; x :: acc) [] a in
  eqint "foldi length" (List.length l) m;
  if m > 0 then eqint "foldi order" (List.hd l) (m - 1)

let test1 () =
  for m = 0 to 42 do test1 m done

let test2 () =
  let a = make 0 42 in
  eqint "length make" 0 (length a);
  let a = make 1729 42 in
  eqint "length make" 1729 (length a);
  for i = 0 to 1728 do eqint "get" 42 (get a i) done;
  let b = cons 1 (snoc a 2) in
  eqint "length cons snoc" 1731 (length b);
  eqint "get cons" 1 (get b 0);
  eqint "get snoc" 2 (get b 1730);
  eq_fa "liat tail" (liat (tail b)) a;
  ()

let test3 () =
  for n = 0 to 100 do
    let a = Array.init n (fun _ -> Random.int (2 * n + 1)) in
    let fa = of_array a in
    eqint "length" n (length fa);
    for i = 0 to n - 1 do eqint "get" a.(i) (get fa i) done
  done

let test4 () =
  let rec build n = function
    | [] -> [] | _ when n = 0 -> [] | x :: l -> x :: build (n-1) l in
  let fib = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55] in
  for n = 0 to 10 do
    let l = build n fib in
    let a = of_list l in
    eqint "length" n (length a);
    for i = 0 to n-1 do eqint "get" (List.nth l i) (get a i) done
  done

let testl1 () =
  let n = 1_000_000 in
  let rec loop a i = if i = n then a else loop (set a i i) (i + 1) in
  let a = loop (make n 0) 0 in
  eqint "length" n (length a);
  let s = ref 0 in
  iter (fun x -> s := !s + x) a;
  eqint "sum" (n*(n-1)/2) !s

let testl2 () =
  let n = 1_000_000 in
  let rec make acc i = if i < 0 then acc else make (i :: acc) (i - 1) in
  let l = make [] (n - 1) in
  let a = of_list l in
  eqint "length" n (length a);
  for i = 0 to n - 1 do eqint "get" i (get a i) done

let () =
  Alcotest.run "Flex_array"
    ["quick tests",
      [Alcotest.test_case "test0" `Quick test0;
       Alcotest.test_case "test1" `Quick test1;
       Alcotest.test_case "test2" `Quick test2;
       Alcotest.test_case "test3" `Quick test3;
       Alcotest.test_case "test4" `Quick test4;
      ];
     "long tests",
      [Alcotest.test_case "test1" `Slow testl1;
       Alcotest.test_case "test2" `Slow testl2;
      ];
    ]
