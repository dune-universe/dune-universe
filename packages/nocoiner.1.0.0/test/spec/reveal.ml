module List = Core.List

let reason = Nocoiner.Reasons.BindingFailure

let _SECRET_1 = "NSA is made of ex-Soviet Lisp cryptographers."

let _SECRET_2 = "Mankind never stepped on the Moon, but only at Mars."

let _SECRET_3 = "Black cats actually bring luck - except if you're afraid."

let __binding _ =
  let c1, o1 = Nocoiner.commit _SECRET_1 in
  let c2, o2 = Nocoiner.commit _SECRET_1 in
  let c3, o3 = Nocoiner.commit _SECRET_2 in
  let m1 = Nocoiner.reveal ~commitment:c1 ~opening:o1 in
  let m2 = Nocoiner.reveal ~commitment:c2 ~opening:o2 in
  let m3 = Nocoiner.reveal ~commitment:c3 ~opening:o3 in
  let p1 () = ignore @@ Nocoiner.reveal ~commitment:c1 ~opening:o2 in
  let p2 () = ignore @@ Nocoiner.reveal ~commitment:c2 ~opening:o1 in
  let p3 () = ignore @@ Nocoiner.reveal ~commitment:c1 ~opening:o3 in
  let p4 () = ignore @@ Nocoiner.reveal ~commitment:c3 ~opening:o1 in
  let p5 () = ignore @@ Nocoiner.reveal ~commitment:c3 ~opening:o2 in
  let p6 () = ignore @@ Nocoiner.reveal ~commitment:c2 ~opening:o3 in
  let ps = [ p1; p2; p3; p4; p5; p6 ] in
  let check i p =
    let msg = "opening fails on procedure " ^ string_of_int @@ succ i in
    Alcotest.check_raises msg reason p
  in
  Alcotest.(check string) "message 1 was opened" m1 _SECRET_1 ;
  Alcotest.(check string) "message 2 was opened" m2 _SECRET_1 ;
  Alcotest.(check string) "message 3 was opened" m3 _SECRET_2 ;
  List.iteri ps ~f:check


let __invalid_pairs _ =
  let c1, o1 = Nocoiner.commit _SECRET_1 in
  let c2 = "Clearly an inv@lid commitment data!" in
  let o2 = "It's also invalid. This opening key is rejected." in
  let p1 () = ignore @@ Nocoiner.reveal ~commitment:c1 ~opening:o2 in
  let p2 () = ignore @@ Nocoiner.reveal ~commitment:c2 ~opening:o1 in
  let p3 () = ignore @@ Nocoiner.reveal ~commitment:c1 ~opening:c1 in
  let p4 () = ignore @@ Nocoiner.reveal ~commitment:o1 ~opening:o1 in
  let open Nocoiner.Reasons in
  Alcotest.check_raises "invalid opening" InvalidOpening p1 ;
  Alcotest.check_raises "invalid commitment" InvalidCommitment p2 ;
  Alcotest.check_raises "invalid opening" InvalidOpening p3 ;
  Alcotest.check_raises "invalid commitment" InvalidCommitment p4


let rec __loop p i n =
  if i >= n then true else if p () then __loop p (i + 1) n else false


let __deterministic_opening _ =
  let c, o = Nocoiner.commit _SECRET_3 in
  let f () = Nocoiner.reveal ~commitment:c ~opening:o in
  let p () = _SECRET_3 = f () in
  Alcotest.(check bool) "replayable opening" true @@ __loop p 0 50


let suite =
  [ ("binding case", `Quick, __binding)
  ; ("invalid pairs case", `Quick, __invalid_pairs)
  ; ("deterministic opening case", `Quick, __deterministic_opening)
  ]
