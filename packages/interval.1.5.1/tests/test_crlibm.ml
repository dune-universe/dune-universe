open Interval_crlibm

(* Test this private function of Interval_crlibm. *)
external is_odd : (float [@unboxed]) -> bool
  = "interval_is_odd_bc" "interval_is_odd" [@@noalloc]

let () =
  for i = 0 to 10 do
    assert(not(is_odd (float (2 * i))));
    assert(is_odd (float (2 * i + 1)));
  done

let () =
  for i = 1 to 1000 do
    let x = I.v Low.(float i /. 1000.) High.(float i /. 1000.) in
    assert(I.subset x I.(sin (asin x)));
  done;
  let y = I.(sin pi) in
  assert(y.low <> y.high)

let () =
  for i = 1 to 1000 do
    let x = I.v Low.(float i /. 1000.) High.(float i /. 1000.) in
    assert(I.subset x I.(cos (acos x)));
  done;
  let y = I.(cos pi) in
  assert(y.low <> y.high)
