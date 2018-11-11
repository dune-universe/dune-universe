let rec fib : (int -> int) = fun n -> 
  if n <= 1 then
    1
  else
    fib (n-1) + fib (n-2)


let n = 35
let _ = Printf.printf "fib(%d) = %d\n%!" n (fib n)

