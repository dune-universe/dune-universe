let _ =
  let fibo_res = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89|] in
  let fibo fibo x =
    if x < 0 then invalid_arg "fibo" ;
    if x < 2 then x else fibo (x - 1) + fibo (x - 2)
  in
  let fibo1 = Memo.memo fibo in
  let module Mem = Memo.Make (struct
    type t = int

    let equal = ( = )

    let hash = Hashtbl.hash
  end) in
  let fibo2 = Mem.memo fibo in
  let module MemWeak = Memo.MakeWeak (struct
    type t = int

    let equal = ( = )

    let hash = Hashtbl.hash
  end) in
  let fibo3 = MemWeak.memo fibo in
  let module MemFake = Memo.Fake (struct
    type t = int

    let equal = ( = )

    let hash = Hashtbl.hash
  end) in
  let fibo4 = MemFake.memo fibo in
  for i = 0 to Array.length fibo_res - 1 do
    assert (fibo1 i = fibo_res.(i)) ;
    assert (fibo2 i = fibo_res.(i)) ;
    assert (fibo3 i = fibo_res.(i)) ;
    assert (fibo4 i = fibo_res.(i))
  done ;
  Format.printf "Tests are OK !@."
