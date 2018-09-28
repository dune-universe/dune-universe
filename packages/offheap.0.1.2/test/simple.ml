(* Test for simple, common objects. *)

type t0 = A | B | C | D | E

type t1 = { a: t1; b : t0; c: t0 }

type t2 = { x: t1; y: t1 }

let () =
  (* Tests a cycle. *)
  let rec x = { a = x; b = B; c = C } in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (z.a == z);
  assert (z.b = B);
  assert (z.c = C);
  Offheap.delete y

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
  Offheap.delete y

let () =
  (* Tests a complex object. *)
  let rec a = { a = a; b = B; c = C } in
  let rec b = { a = b; b = C; c = B } in
  let x = { x = a; y = b } in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (z.x.a == z.x);
  assert (z.y.a == z.y);
  assert (z.x.b = B && z.x.c = C);
  assert (z.y.b = C && z.y.c = B);
  Offheap.delete y

let () =
  (* Tests a closure. *)
  let n = "1" in
  let x = Some (fun () -> int_of_string n + 2) in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  let n = match z with None -> 1 | Some f -> f () in
  assert (n = 3);
  Offheap.delete y

let () =
  (* Should fail if object is abstract. *)
  try
    let rec x = { a = x; b = B; c = C } in
    ignore (Offheap.copy (Offheap.copy x));
    failwith "failed"
  with Invalid_argument _ -> ()

let () =
  (* Should handle primitives. *)
  let x = 1 in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (z = x);
  Offheap.delete y

let () =
  (* Should handle static data. *)
  let x = "123" in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (z = x);
  Offheap.delete y

let () =
  (* Should handle native ints. *)
  let x = Nativeint.one in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (z = x);
  Offheap.delete y

let () =
  (* Should be marked as being part of a value area so compare/hash work. *)
  let x = Some (
    "I've got a clan of gingerbread men." ^
    "Here a man, there a man, lots of gingerbread men." ^
    "Take a couple if you wish. They're on the dish."
  ) in
  let y = Offheap.copy x in
  let z = Offheap.get y in
  assert (Pervasives.compare x z = 0);
  assert (Hashtbl.hash x = Hashtbl.hash z);
  Offheap.delete y

