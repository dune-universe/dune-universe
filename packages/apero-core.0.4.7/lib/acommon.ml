let identity v = v

let drop n xs =
  let rec adrop n xs = match n with
    | 0 -> xs
    | _ -> match xs with
      | [] -> []
      | _::tl -> adrop (n-1) tl
  in adrop n xs

let take n xs = 
  let rec atake n xs ys = match n with
    | 0 -> List.rev ys
    | _ -> match xs with
      | [] -> List.rev ys
      | h::tl -> atake (n-1) tl  (h::ys)
  in atake n xs []


let compose f g = fun x -> f @@ g @@ x


let some default f o = match o with
  | Some v -> f v
  | _ -> default

let result f g r = match r with
  | Ok v -> f v
  | Error e -> g e

let apply f v = f v

let flip f = fun a b -> f b a 

let apply_n (t : 'a) (f : 'a -> 'b)  (n: int) =
  let rec loop_n n xs =
    if n = 1 then xs
    else loop_n (n-1) ((f t) :: xs)
  in loop_n n []

module Astring = struct
  include Astring.String

  let contains c = exists (fun c' -> c = c')

  let contains_sub ~sub s = match find_sub ~sub s with | Some _ -> true | None -> false

  let replace c c' = map (function | x when x=c -> c' | x -> x)

  let after i = with_index_range ~first:i ~last:max_int

  let before i = with_index_range ~first:0 ~last:(i-1)
end


module Infix = struct
  let (<.>) = compose
  let (%) = compose
  let (%>) f g =  g % f
  let (<*>) f v = f v
end


module Option = struct 
  let bind o f = match o with 
    | Some v -> f v
    | None -> None

  let map o f = match o with 
    | Some v -> Some (f v)
    | None -> None

  let return v = Some v

  let zero () = None

  let is_some = function
    | Some _ -> true
    | _ -> false 

  let get = function 
    | Some v -> v 
    | _ -> failwith "Can't get value out of None"

  let get_or_else opt f = match opt with
    | Some v  -> v 
    | _ -> f ()

  let get_or_default opt d = match opt with
    | Some v  -> v 
    | _ -> d

  let or_else opt (f: unit -> 'a option) = match opt with 
    | Some _ -> opt 
    | None -> f ()

  let iter opt f = match opt with
    | Some v -> f v 
    | _ -> ()

  let flatten os = 
    let rec rflat os fos = match os with 
      | h::tl -> (match h with 
          | Some v -> rflat tl (v::fos)
          | None -> rflat tl fos)
      | [] -> Some fos
    in rflat os []

  let lift f = function 
    | Some x -> Some (f x)
    | None -> None

  let lift2 f  = fun oa ob -> bind oa @@ fun a -> bind ob @@ fun b -> Some (f a b)

  module Infix = struct 
    let (>>=) = bind
    let (>|=) = map
    let (>>) a b = a >>= fun _ -> b 
    let (<$>) = lift
    let (<$$>) = lift2
    let (>==) a b = lift b a
    let (>?=) a b = get_or_default a b
  end


end

module Result = struct
  type (+'a, +'e) t = ('a, 'e) result

  let bind r  f = match r with 
    | Ok v -> f v 
    | Error e -> Error e

  let bind2 r f = match r with 
    | Ok (x, y) -> f x y
    | Error e -> Error e  

  let map r f = match r with 
    | Ok v -> Ok (f v)
    | Error e -> Error e 

  let bind_error r f = match r with 
    |Error e -> f e
    | Ok _ as ok -> ok

  let return v = Ok v

  let ok = return

  let fail e = Error e 

  let is_ok = function 
    | Ok _ -> true
    | _ -> false

  let is_error r = not @@ is_ok r

  let get = function 
    | Ok v -> v
    | Error _ -> failwith "Cannot extract result from error!"

  let try_get ~run:f ~fail_with:g ~on:r  = match r with
    | Ok v -> f v 
    | Error e -> g e

  let get_or_else r f = match r with
    | Ok v -> v
    | Error e -> f e

  let or_else r f = match r with 
    | Ok _ -> r 
    | Error e -> f e

  let flatten rs = 
    let rec rflat frs = function      
      | h::tl -> (match h with 
          | Ok v -> rflat (v::frs) tl 
          | Error _ as err-> err)
      | [] -> Ok frs
    in rflat [] rs

  let to_option = function
    | Ok v -> Option.return v
    | Error _ -> Option.zero ()

  let iter r f = match r with 
    | Ok v -> f v
    | Error _  -> ()

  let lift f = function 
    | Ok v -> Ok (f v)
    | Error _ as e -> e 

  let rec fold_m f xs b  = match xs with
    | [] -> return b
    | h::tl -> bind (f h b) (fun b -> fold_m f tl b)


  let cons x xs = match x with 
    | Ok y -> bind xs (fun ys -> Ok (y::ys))
    | Error _ as e  -> e 

  module Infix = struct
    let (>>=) = bind
    let (>>) a b = a >>= fun _ -> b 
    let (>>==) = bind2
    let (>>>) = map
    let (>>=!) = bind_error
    let (<$>) = lift
  end

end


module LwtM = struct
  include Lwt

  let rec fold_m f b xs = match xs with
    | [] -> Lwt.return b
    | h::tl -> f b h >>= (fun b -> fold_m f b tl)

  let lift f =  fun x -> bind x (fun y -> return (f  y))

  let of_result to_exn = function 
    | Ok v -> Lwt.return v
    | Error e -> Lwt.fail (to_exn e)

  let sequence a (lazy b) = a >>= fun _ -> b

  let flatten xs = 
    let rec do_flatten acc ys = 
      match ys with
      | [] ->  Lwt.return acc
      | hd::tl ->  Lwt.bind hd (fun h -> (do_flatten (h::acc) tl))
    in do_flatten [] xs      

  let read_mvar mvar = 
    match Lwt_mvar.take_available mvar with 
    | Some v -> sequence (Lwt_mvar.put mvar v) (lazy (Lwt.return v))
    | None -> Lwt_mvar.take mvar >>= (fun v -> sequence (Lwt_mvar.put mvar v) (lazy (Lwt.return v)))

  module InfixM = struct 
    let (<$>) = lift
    let (>>=) = bind
    let (>>) = sequence
    let (>|=) = Lwt.Infix.(>|=)
    let (<&>) = Lwt.Infix.(<&>)
    let (<?>) = Lwt.Infix.(<?>)
    let (%>>=) f g = fun x -> f x >>= g
  end
end

module type Monad = sig 
  type 'a m
  val return : 'a -> 'a m  
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val map : 'a m -> ('a -> 'b) -> 'b m
  val lift : ('a -> 'b) -> ('a m -> 'b m)
  val iter : 'a m -> ('a -> unit) -> unit
  (* val flatten : ('a m) list -> ('a list) m   *)
  module Infix : sig
    val (<$>) : ('a -> 'b) -> ('a m -> 'b m)    
    val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  end 
end

