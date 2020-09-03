let rec int_pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = int_pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let max_int = (int_pow 2 30) -1

let test empty add mem =
  let seed = Random.int max_int in
  Random.init seed;
  let s =
    let rec loop s i =
      if i = 1000 then s else loop (add (Random.int max_int) s) (succ i)
    in
    loop empty 0
  in
  Random.init seed;
  for _i = 0 to 999 do assert (mem (Random.int max_int) s) done

let main () =
  test Ptset.empty Ptset.add Ptset.mem;
  test Ptset.Big.empty Ptset.Big.add Ptset.Big.mem;
  test Ptset.BigPos.empty Ptset.BigPos.add Ptset.BigPos.mem

let () = main ()
