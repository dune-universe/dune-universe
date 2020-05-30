module M1_0 = struct
  type +'a t =
    | Return : 'a -> 'a t
    | Bind   : 'a t * ('a -> 'b t) -> 'b t
  
  let return a = Return a
  
  let bind at f = Bind (at, f)
end

module M1 = struct
  include M1_0
  include Monad.Make(M1_0)
end

module M2_0 = struct
  type (+'a, 'x) t =
    | Return : 'a -> ('a, 'x) t
    | Bind   : ('a, 'x) t * ('a -> ('b, 'x) t) -> ('b, 'x) t
  
  let return a = Return a
  
  let bind at f = Bind (at, f)
end

include M1

module M2 = struct
  include M2_0
  include Monad.Make2(M2_0)
end
                         
