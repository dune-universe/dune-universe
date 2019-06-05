let _SECRET = "I don't know programming! I can't even code 2 + 2!"

let __nondeterministic_commitment _ =
  let c1, o1 = Nocoiner.commit _SECRET in
  let c2, o2 = Nocoiner.commit _SECRET in
  let c3, o3 = Nocoiner.commit _SECRET in
  let ky1, iv1 = Helpers.__split ~on:'.' o1 in
  let ky2, iv2 = Helpers.__split ~on:'.' o2 in
  let ky3, iv3 = Helpers.__split ~on:'.' o3 in
  let cp1, tg1 = Helpers.__split ~on:'@' c1 in
  let cp2, tg2 = Helpers.__split ~on:'@' c2 in
  let cp3, tg3 = Helpers.__split ~on:'@' c3 in
  Alcotest.(check @@ neg string) "commitments differ" c1 c2 ;
  Alcotest.(check @@ neg string) "commitments differ" c1 c3 ;
  Alcotest.(check @@ neg string) "commitments differ" c3 c2 ;
  Alcotest.(check @@ neg string) "opening keys differ" o1 o2 ;
  Alcotest.(check @@ neg string) "opening keys differ" o3 o2 ;
  Alcotest.(check @@ neg string) "opening keys differ" o1 o3 ;
  Alcotest.(check @@ neg string) "encryption keys differ" ky1 ky2 ;
  Alcotest.(check @@ neg string) "encryption keys differ" ky1 ky3 ;
  Alcotest.(check @@ neg string) "encryption keys differ" ky3 ky2 ;
  Alcotest.(check @@ neg string) "input vectors differ" iv1 iv2 ;
  Alcotest.(check @@ neg string) "input vectors differ" iv1 iv3 ;
  Alcotest.(check @@ neg string) "input vectors differ" iv3 iv2 ;
  Alcotest.(check @@ neg string) "cipher texts differ" cp1 cp2 ;
  Alcotest.(check @@ neg string) "cipher texts differ" cp1 cp3 ;
  Alcotest.(check @@ neg string) "cipher texts differ" cp3 cp2 ;
  Alcotest.(check @@ neg string) "integrity tags differ" tg1 tg2 ;
  Alcotest.(check @@ neg string) "integrity tags differ" tg1 tg3 ;
  Alcotest.(check @@ neg string) "integrity tags differ" tg3 tg2

let suite =
  [("non-deterministic commitment case", `Quick, __nondeterministic_commitment)]
