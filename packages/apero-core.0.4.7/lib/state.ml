module StateFunc = struct
  module type S = sig        
    type s 
    type 'a m = s -> s * 'a
    val return : 'a -> s -> s * 'a    
    val bind : (s -> 'a * 'b) -> ('b -> 'a -> 'c) -> s -> 'c
    val read : s -> s * s
    val write : 'a -> s -> 'a * unit
    val run : (s -> 'a) -> s -> 'a
    val eval : (s -> 'a * 'b) -> s -> 'b
    val modify : (s -> 'a) -> s -> 'a * unit

    module Infix :
    sig val ( >>= ) : (s -> 'a * 'b) -> ('b -> 'a -> 'c) -> s -> 'c 
    end
  end 

  module Make (T : sig type s end) = struct 
    type s = T.s
    type 'a m = T.s -> (T.s * 'a)
    let return x (s: T.s)  = (s, x)
    let bind xf f (s: T.s) =
      let s',x = xf s in f x s'

    let read (s: T.s)    = (s,s)
    let write x (_: T.s) = (x,())
    let run x (s: T.s)   = x s
    let eval x (s: T.s)  = snd (x s)
    let modify f  = bind read (fun s -> write (f s))
    module Infix = struct
      let (>>=) = bind
    end
  end
end

module State = struct 
  type ('s, 'a) m = 's -> 's * 'a

  let return x s  = (s, x)
  let bind xf f s = let s',x = xf s in f x s'              
  let read s    = (s,s)
  let write x _ = (x,())
  let run x s   = x s
  let eval x s  = snd (x s)
  let modify f  = bind read (fun s -> write (f s))
  module Infix = struct 
    let ( >>= ) = bind 
  end
end 
