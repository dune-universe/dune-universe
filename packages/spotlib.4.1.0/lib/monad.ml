open Base

include Monad_intf

module Make2(M : S2) : T2 with type ('a, 'z) t := ('a, 'z) M.t = struct
  module S = M

  include S
    
  module EX = struct
    let fmap f t = bind t & fun x -> return (f x)
    let liftM = fmap
  
    let fmap2 f t u = bind t & fun t -> bind u & fun u -> return & f t u
    let liftM2 = fmap2
  
    let void a = bind a & fun _ -> return ()
  
    let rec seq = function
      | [] -> return []
      | x::xs ->
          bind x & fun x ->
          bind (seq xs) & fun xs ->
          return (x::xs)
  
    let rec seq_ = function
      | [] -> return ()
      | x::xs -> bind x & fun () -> seq_ xs
  
    let mapM f ls = seq (List.map f ls)
  
    let rec mapM_ f = function
      | [] -> return ()
      | x::xs -> bind (f x) & fun () -> mapM_ f xs
  
    let iteri f ls = seq_ (List.mapi f ls)
  
    let rec for_ i to_ f =
      if i > to_ then return ()
      else bind (f i) & fun () -> for_ (i+1) to_ f
      
    let join tt = bind tt & fun at -> at
  end

  include EX
    
  module Infix = struct
    let (>>=) = M.bind

    let (>>|) t f = fmap f t

    (* Applicative style *)
    let ( ^<$> ) f t = fmap f t 
  
    let ( /<*> ) = fun f a ->
      f >>= fun f -> 
      a >>= fun a ->
      return (f a)
  end

  include Infix
end
  
module Make1(M : S1) : T1 with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t
  type ('a, 'dummy) m = 'a t
  module M2 = struct
    type nonrec ('a, _) t = 'a t
    include (M : S1 with type 'a t := 'a M.t) (* hiding 'a t *)
  end
  include (Make2(M2) : T2 with type ('a, 'dummy) t := ('a, 'dummy) m)
end

module Make = Make1
