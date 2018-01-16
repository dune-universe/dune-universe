include Lazy

include (Monad.Make(struct
  type nonrec 'a t = 'a t
  let return : 'a -> 'a t = from_val
  let bind z f = f (force z)
end) : Monad.T with type 'a t := 'a Lazy.t)

let (!!) = force
let eager = Lazy.from_val

let peek v = if is_val v then Some (!!v) else None
      
let map f v = 
  if is_val v then eager (f !!v)
  else lazy (f !!v)

let detuple xy = 
  map fst xy,
  map snd xy
