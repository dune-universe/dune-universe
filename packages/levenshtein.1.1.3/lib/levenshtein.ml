(**

   Levenshtein distance algorithm for general array.
   
   Author: jun.furuse@gmail.com
   License: public domain

*)

module PTest = Ppx_test.Test
  
(** Minimum of three integers *)
let min3 (x:int) y z =
  let m' (a:int) b = if a < b then a else b in
  m' (m' x y) z

(* Matrix initialization. 

    ------- 2 ----------
   | 0123456789...    m
   | 1
   1 2          0
   | .              
   | n
*)
let init_matrix n m =
  let init_col = Array.init (m+1) in
  Array.init (n+1) (function
    | 0 -> init_col (function j -> j)
    | i -> init_col (function 0 -> i | _ -> 0))

module type S = sig
  type t
  val distance : ?upper_bound: int -> t -> t -> int
  (** Calculate Levenshtein distance of 2 t's *)
end

module type Array = sig
  type t
  type elem
  val compare : elem -> elem -> int
  val get : t -> int -> elem
  val size : t -> int
end

module Make(A : Array) = struct

  type t = A.t

  (* It is simply slow but nearest to the math *)
  let slow_but_simple xs ys =
    let rec d i j =
      match i, j with
      | 0, _ -> j
      | _, 0 -> i
      | _ -> 
          let i' = i - 1 in
          let j' = j - 1 in
          min3 
            (d i' j + 1)
            (d i j' + 1)
            (d i' j' + abs (A.compare (A.get xs i') (A.get ys j')))
    in
    d (A.size xs) (A.size ys)

  (* slow_but_simple + memoization *)      
  let memoized xs ys =
    let cache = Array.init (A.size xs+1) (fun _ -> Array.make (A.size ys+1) (-1)) in
    let rec d i j =
      match i, j with
      | 0, _ -> j
      | _, 0 -> i
      | _ -> 
          let cache_i = Array.unsafe_get cache i in
          match Array.unsafe_get cache_i j with
          | -1 ->
              let res = 
                let i' = i - 1 in
                let j' = j - 1 in
                min3 
                  (d i' j + 1)
                  (d i j' + 1)
                  (d i' j' + abs (A.compare (A.get xs i') (A.get ys j')))
              in
              Array.unsafe_set cache_i j res;
              res
          | res -> res
    in
    d (A.size xs) (A.size ys)

  (* slow_but_simple + memoization + upperbound 

     There is a property: d(i-1)(j-1) <= d(i)(j)
     so if d(i-1)(j-1) >= upper_bound then we can immediately say
     d(i)(j) >= upper_bound, and skip the calculation of d(i-1)(j) and d(i)(j-1)
  *)
  let distance ?(upper_bound=max_int) xs ys =
    let size_xs = A.size xs 
    and size_ys = A.size ys in
    (* cache: d i j is stored at cache.(i-1).(j-1) *)
    let cache = Array.init size_xs (fun _ -> Array.make size_ys (-1)) in
    let rec d i j =
      match i, j with
      | 0, _ -> j
      | _, 0 -> i
      | _ -> 
          let i' = i - 1 in
          let cache_i = Array.unsafe_get cache i' in
          let j' = j - 1 in
          match Array.unsafe_get cache_i j' with
          | -1 ->
              let res = 
                let upleft = d i' j' in
                if upleft >= upper_bound then upper_bound
                else 
                  let cost = abs (A.compare (A.get xs i') (A.get ys j')) in
                  let upleft' = upleft + cost in
                  if upleft' >= upper_bound then upper_bound
                  else
                    (* This is not tail recursive *)
                    min3 (d i' j + 1)
                         (d i j' + 1)
                         upleft'
              in
              Array.unsafe_set cache_i j' res;
              res
          | res -> res
    in
    min (d size_xs size_ys) upper_bound

  (** http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance *)
  let wikibook xs ys =
    let get = Array.unsafe_get in
    match A.size xs, A.size ys with
    | 0, n -> n
    | m, 0 -> m
    | m, n ->
        let matrix = init_matrix m n in
        (* d(0)(j) = j
           d(i)(0) = i
           d(i)(j) = min of d(i-1)(j  ) + 1
                            d(i)  (j-1) + 1
                            d(i-1)(j-1) + if x.[i-1] = y.[j-1] then 0 else 1
        *)
        for i = 1 to m do
          let s = get matrix i and t = get matrix (i - 1) in
          for j = 1 to n do
            let cost = abs (A.compare (A.get xs (i - 1)) (A.get ys (j - 1))) in
            Array.unsafe_set s j (min3 (get t j + 1) (get s (j - 1) + 1) (get t (j - 1) + cost))
          done
        done;
        get (get matrix m) n
end

(** With inter-query cache by hashtbl *)

module type Cache = sig
  type 'a t
  type key
  val create : int -> 'a t
  val alter : 'a t -> key -> ('a option -> 'a option) -> 'a option
end

type result = 
  | Exact of int
  | GEQ of int (* the result is culled by upper_bound. We know it is GEQ to this value *)

module type WithCache = sig
  type t
  type cache
  val create_cache : int -> cache
  val distance : cache -> ?upper_bound: int -> t -> t -> result
end

module CacheByHashtbl(H : Hashtbl.HashedType) : Cache with type key = H.t = struct
  include Hashtbl.Make(H)
  let alter t k f =
    let v = f (try Some (find t k) with Not_found -> None) in
    begin match v with
    | None -> remove t k
    | Some v -> replace t k v
    end;
    v
end
    

module MakeWithCache(A : Array)(C : Cache with type key = A.t * A.t) = struct

  type t = A.t

  type cache = result C.t

  module WithoutCache = Make(A)

  let create_cache = C.create

  let distance cache ?(upper_bound=max_int) xs ys =
    let k = (xs, ys) in
    let vopt = C.alter cache k @@ function
      | Some (Exact _) as vopt -> vopt
      | Some (GEQ res) as vopt when res >= upper_bound -> vopt
      | _ (* not known, or inaccurate with this upper_bound *) ->
          Some (
            let res = WithoutCache.distance ~upper_bound xs ys in
            if res >= upper_bound then GEQ upper_bound
            else Exact res
          )
    in
    match vopt with
    | Some v -> v
    | None -> assert false
end

module StringWithHashtbl = struct

  module Array = struct
    type t = string
    type elem = char
    let compare (c1 : char) c2 = compare c1 c2
    let get = String.unsafe_get
    let size = String.length
  end

  module Cache = CacheByHashtbl(struct
    type t = string * string
    let equal = (=)
    let hash = Hashtbl.hash
  end)

  include MakeWithCache(Array)(Cache)
end

module String = struct

  include Make(struct
    type t = string
    type elem = char
    let compare (c1 : char) c2 = compare c1 c2
    let get = String.unsafe_get
    let size = String.length
  end)

  let %TEST slow_but_simple =
    slow_but_simple "xx" "xaaax" = 3

  let%TEST distance =
    distance "xx" "xaaax" = 3

  let random_char = 
    let offset = Char.code 'A' in
    let length = Char.code 'Z' - offset + 1 in
    fun () -> Char.chr (Random.int length + offset)

  let random len = String.init len @@ fun _ -> random_char ()

  let test ?(upper_bound=max_int) loop len dist dist' =
    for i = 0 to loop do
      if i mod (loop / 10) = 0 then Printf.eprintf "%d\n%!" i;
      let l1 = Random.int len in
      let l2 = Random.int len in
      let s1 = random l1 in
      let s2 = random l2 in
      let d = dist s1 s2 in
      let d' = dist' s1 s2 in
      if d < upper_bound && d' < upper_bound && d <> d' then begin
        Printf.eprintf "%s %s : %d %d\n" s1 s2 d d';
        assert false
      end
    done

  let %TEST_UNIT "wikibook correctness" =  
    test 1000 10 slow_but_simple wikibook

  let %TEST_UNIT "memoized and wikibook" =  
    test 10000 20 memoized wikibook

  let %TEST_UNIT "distance and wikibook" =  
    test ~upper_bound:20 100000 30 (distance ~upper_bound:20) wikibook

  let %TEST_UNIT "distance and wikibook performance check (it takes long time)" =  
    let sample_size = 100000 in
    for _i = 1 to 10 do
      let samples = Array.init sample_size (fun _ -> random 30, random 30) in
      let time name f v =
        let () = Gc.full_major () in
        let start = Unix.gettimeofday () in
        f v;
        let end_ = Unix.gettimeofday () in
        Printf.eprintf "%s : %f\n%!" name (end_ -. start)
      in
      let bench d =
        Array.iter (fun (s1,s2) -> ignore (d s1 s2)) samples
      in
      time "wikibook" bench wikibook;
      time "distance ~upper_bound:20" bench (distance ~upper_bound:20)
    done
        
end
