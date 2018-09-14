(* Test for simple, common objects. *)

type t0 = A | B | C | D | E

type t1 = { a: t1; b : t0; c: t0 }

let () =
  (* Tests a cycle. *)
  let rec x = { a = x; b = B; c = C } in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (z.a == z);
  assert (z.b = B);
  assert (z.c = C);
  Offheap.free y

let () =
  (* Tests a sequence of bytes. *)
  let x =
      "I know a mouse, and he hasn’t got a house" ^
      "I don't know why. I call him Gerald." ^
      "He's getting rather old, but he's a good mouse."
  in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (z = x);
  Offheap.free y

let () =
  (* Tests a closure. *)
  let n = "1" in
  let x = Some (fun () -> int_of_string n + 2) in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  let n = match z with None -> 1 | Some f -> f () in
  assert (n = 3);
  Offheap.free y

let () =
  (* Should fail if object is an integer. *)
  try
    ignore (Offheap.copy 4);
    failwith "failed"
  with Invalid_argument _ -> ()

let () =
  (* Should fail if object is abstract. *)
  try
    let rec x = { a = x; b = B; c = C } in
    ignore (Offheap.copy (Offheap.copy x));
    failwith "failed"
  with Invalid_argument _ -> ()
