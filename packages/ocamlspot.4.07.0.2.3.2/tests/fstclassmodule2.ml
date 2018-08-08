module type ASig = sig val (* B.f => *) f (* <= B.f *) : int -> int end
module A = struct let f x = x + 1 end
let x = (module A : ASig)
let y = 
  let module B = (val x : ASig) in
  B.f (* ? B.f *) 1
