module A = Nullable_array

let check_invalid_arg (f: 'a -> unit) (a: 'a) =
  try f a; raise Exit
  with
  | Exit -> assert false
  | Invalid_argument _ -> ()


let t3 () =
  let a = A.make 3 in
  let empty = A.make 3 in
  assert(A.length a = 3);
  assert(A.length A.empty_array = 0);
  check_invalid_arg (A.set a (-1)) (Some 3.14);
  check_invalid_arg (A.set a (-1000)) (Some 3.14);
  check_invalid_arg (A.set a (3)) (Some 3.14);
  check_invalid_arg (A.set a (max_int)) (Some 3.14);
  check_invalid_arg (A.set a (min_int)) (Some 3.14);
  check_invalid_arg (A.set_some a (-1)) 3.14;
  check_invalid_arg (A.set_some a (-1000)) 3.14;
  check_invalid_arg (A.set_some a (3)) 3.14;
  check_invalid_arg (A.set_some a (max_int)) 3.14;
  check_invalid_arg (A.set_some a (min_int)) 3.14;
  check_invalid_arg (A.clear a) (-1);
  check_invalid_arg (A.clear a) (-1000);
  check_invalid_arg (A.clear a) (3);
  check_invalid_arg (A.clear a) (max_int);
  check_invalid_arg (A.clear a) (min_int);
  assert(a = empty)

let t4 () =
  let a = A.make 3 in
  let empty = A.make 3 in
  A.set a 0 (Some 1.1);
  A.set a 2 (Some 3.3);
  assert(A.get a 0 = Some 1.1);
  assert(A.get a 1 = None);
  assert(A.get a 2 = Some 3.3);
  A.set_some a 1 2.2;
  assert(A.get a 0 = Some 1.1);
  assert(A.get a 1 = Some 2.2);
  assert(A.get a 2 = Some 3.3);
  A.clear a 2;
  assert(A.get a 0 = Some 1.1);
  assert(A.get a 1 = Some 2.2);
  assert(A.get a 2 = None);
  A.clear a 0;
  assert(A.get a 0 = None);
  assert(A.get a 1 = Some 2.2);
  assert(A.get a 2 = None);
  A.clear a 1;
  assert(A.get a 0 = None);
  assert(A.get a 1 = None);
  assert(A.get a 2 = None);
  assert(a = empty)

let t5 () =
  let a = A.make 10 in
  A.set_some a 3 3.;
  A.set_some a 5 5.;
  A.set_some a 9 9.;
  let some = ref 0 in
  let count = ref 0 in
  let last = ref (-1) in
  A.iteri
    ~some:(fun i n ->
        incr some;
        incr count;
        assert(!last = i - 1);
        last := i;
        assert(float i = n))
    ~none:(fun i ->
        incr count;
        assert(!last = i - 1);
        last := i)
    a;
  assert(!last = 9);
  assert(!some = 3);
  assert(!count = 10)

let () =
  t3 ();
  t4 ();
  t5 ();
  ()
