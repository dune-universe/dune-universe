module A = Nullable_array

let check_invalid_arg (f: 'a -> unit) (a: 'a) =
  try f a; raise Exit
  with
  | Exit -> assert false
  | Invalid_argument _ -> ()

let ign_make (n:int) =
  ignore(A.make n:'a A.t)

let ign_get (a:'a A.t) n =
  ignore(A.get a n:'a option)

let marshal_identity (v:'a A.t) : 'a A.t =
  Marshal.from_string (Marshal.to_string v []) 0

let t1 () =
  let a = A.make 8 in
  let b = A.make 8 in
  assert(a = b);
  assert(not (a == b));
  assert(A.length a = 8);
  check_invalid_arg ign_make (-1);
  check_invalid_arg ign_make (-120908);
  check_invalid_arg ign_make Sys.max_array_length

let t2 () =
  let a = A.make 3 in
  assert(A.get a 0 = None);
  assert(A.get a 1 = None);
  assert(A.get a 2 = None);
  check_invalid_arg (ign_get a) (-1);
  check_invalid_arg (ign_get a) (-1000);
  check_invalid_arg (ign_get a) (3);
  check_invalid_arg (ign_get a) max_int;
  check_invalid_arg (ign_get a) min_int

let t3 () =
  let a = A.make 3 in
  let empty = A.make 3 in
  assert(A.length a = 3);
  assert(A.length A.empty_array = 0);
  check_invalid_arg (A.set a (-1)) (Some 'a');
  check_invalid_arg (A.set a (-1000)) (Some 'a');
  check_invalid_arg (A.set a (3)) (Some 'a');
  check_invalid_arg (A.set a (max_int)) (Some 'a');
  check_invalid_arg (A.set a (min_int)) (Some 'a');
  check_invalid_arg (A.set_some a (-1)) 'a';
  check_invalid_arg (A.set_some a (-1000)) 'a';
  check_invalid_arg (A.set_some a (3)) 'a';
  check_invalid_arg (A.set_some a (max_int)) 'a';
  check_invalid_arg (A.set_some a (min_int)) 'a';
  check_invalid_arg (A.clear a) (-1);
  check_invalid_arg (A.clear a) (-1000);
  check_invalid_arg (A.clear a) (3);
  check_invalid_arg (A.clear a) (max_int);
  check_invalid_arg (A.clear a) (min_int);
  assert(a = empty)

let t4 () =
  let a = A.make 3 in
  let empty = A.make 3 in
  A.set a 0 (Some 'a');
  A.set a 2 (Some 'c');
  assert(A.get a 0 = Some 'a');
  assert(A.get a 1 = None);
  assert(A.get a 2 = Some 'c');
  A.set_some a 1 'b';
  assert(A.get a 0 = Some 'a');
  assert(A.get a 1 = Some 'b');
  assert(A.get a 2 = Some 'c');
  A.clear a 2;
  assert(A.get a 0 = Some 'a');
  assert(A.get a 1 = Some 'b');
  assert(A.get a 2 = None);
  A.clear a 0;
  assert(A.get a 0 = None);
  assert(A.get a 1 = Some 'b');
  assert(A.get a 2 = None);
  A.clear a 1;
  assert(A.get a 0 = None);
  assert(A.get a 1 = None);
  assert(A.get a 2 = None);
  assert(a = empty)

let t5 () =
  let a = A.make 10 in
  A.set_some a 3 3;
  A.set_some a 5 5;
  A.set_some a 9 9;
  let some = ref 0 in
  let count = ref 0 in
  let last = ref (-1) in
  A.iteri
    ~some:(fun i n ->
        incr some;
        incr count;
        assert(!last = i - 1);
        last := i;
        assert(i = n))
    ~none:(fun i ->
        incr count;
        assert(!last = i - 1);
        last := i)
    a;
  assert(!last = 9);
  assert(!some = 3);
  assert(!count = 10)

let t6 () =
  let a = A.make 3 in
  let b = marshal_identity a in
  assert(A.get b 0 = None);
  assert(A.get b 1 = None);
  assert(A.get b 2 = None);
  check_invalid_arg (ign_get b) (-1);
  check_invalid_arg (ign_get b) 3;
  assert(a = b)

let test_blit id =
  let len = 5 in
  let a = A.make len in
  let b = id (A.make (2 * len)) in
  check_invalid_arg (A.blit a 0 b 0) (len + 1);
  check_invalid_arg (A.blit a 1 b 0) (len);
  check_invalid_arg (A.blit a len b 0) 1;
  check_invalid_arg (A.blit a 0 b (len+1)) len;
  check_invalid_arg (A.blit a 2 b 2) (-1);
  check_invalid_arg (A.blit a (-1) b 2) 0;
  check_invalid_arg (A.blit a 1 b (-1)) 0;
  let init arr =
    for i = 0 to A.length arr - 1 do
      A.set_some arr i i;
    done
  in
  let eq_array_some arr tarr =
    assert(A.length arr = Array.length tarr);
    for i = 0 to A.length arr - 1 do
      assert(Some tarr.(i) = A.get arr i)
    done
  in
  let eq_array arr tarr =
    assert(A.length arr = Array.length tarr);
    for i = 0 to A.length arr - 1 do
      assert(tarr.(i) = A.get arr i)
    done
  in
  init a;
  eq_array_some a [|0;1;2;3;4|];
  eq_array b [| None; None; None; None; None; None; None; None; None; None |];
  A.blit a 0 b len len;
  eq_array b [| None;   None;   None;   None;   None;
                Some 0; Some 1; Some 2; Some 3; Some 4 |];
  A.blit a 2 b 3 2;
  eq_array b [| None;   None;   None;   Some 2; Some 3;
                Some 0; Some 1; Some 2; Some 3; Some 4; |];
  A.clear a 3;
  eq_array a [| Some 0; Some 1; Some 2; None  ; Some 4 |];
  A.blit a 2 b 3 2;
  eq_array b [| None;   None;   None;   Some 2; None;
                Some 0; Some 1; Some 2; Some 3; Some 4; |];
  A.blit a 0 a 1 4;
  eq_array a [| Some 0; Some 0; Some 1; Some 2; None |];
  A.blit a 2 a 1 2;
  eq_array a [| Some 0; Some 1; Some 2; Some 2; None |];
  ()

let t7 () =
  test_blit (fun x -> x);
  test_blit marshal_identity

let t8 () =
  let zero1 = A.make 0 in
  let zero2 = A.make 0 in
  let empty_not_zero = A.make 1 in
  assert(A.equal zero1 zero2 ~equal:(fun _ _ -> false));
  assert(not (A.equal zero1 empty_not_zero ~equal:(fun _ _ -> assert false)));

  let a = A.make 3 in
  let a' = A.make 3 in
  let a'' = marshal_identity a' in
  let b = A.make 2 in
  assert(not (A.equal a b ~equal:(fun _ _ -> true)));
  assert(A.equal a a' ~equal:(fun _ _ -> assert false));
  assert(A.equal a a'' ~equal:(fun _ _ -> assert false));

  Random.init 0;
  let count = 10000 in
  let size = 3 in
  let len = 3 in
  let a = A.make len in
  let b = A.make len in
  let v () =
    let r = Random.int size in
    if r = 0 then None
    else Some (Int64.of_int r)
  in
  let equal = ref 0 in
  for _ = 0 to count do
    A.set a (Random.int len) (v ());
    A.set b (Random.int len) (v ());
    let eq = a = b in
    let eq' = A.equal a b ~equal:Int64.equal in
    assert(eq = eq');
    if eq then incr equal;
  done;
  assert(!equal > 0); (* Verify that we tested the equal case at least once *)
  Printf.printf "eq: %i\n%!" !equal;
  ()

let () =
  t1 ();
  t2 ();
  t3 ();
  t4 ();
  t5 ();
  t6 ();
  t7 ();
  t8 ();
  ()
