
include Printf

module List = struct

  (* This module takes code from Janestreet's Core library:

    Copyright (C) 2008-
      Jane Street Holding, LLC
      1 New York Plaza, 33rd Floor
      New York, NY 10004
      USA

    email: opensource@janestreet.com

    The contents of some files in this distribution was derived from external
    sources with compatible licenses. The original copyright and license
    notice was preserved in the affected files.

    See https://github.com/janestreet/core/blob/master/COPYRIGHT.txt
  *)
  include ListLabels

  let hd_exn = hd
  let hd = function o :: _ -> Some o | [] -> None

  let tl_exn = tl
  let tl = function [] -> None | _ :: t -> Some t

  let nth t n =
    if n < 0 then None else
      let rec nth_aux t n =
        match t with
        | [] -> None
        | a :: t -> if n = 0 then Some a else nth_aux t (n-1)
      in
      nth_aux t n

  let nth_exn t n =
    match nth t n with
    | None ->
      ksprintf invalid_arg "List.nth_exn %d called on list of length %d"
        n (length t)
    | Some a -> a

  let fold ~init ~f l = fold_left ~init ~f l

  let rev_filter t ~f =
    let rec find ~f accu = function
    | [] -> accu
    | x :: l -> if f x then find ~f (x :: accu) l else find ~f accu l
    in
    find ~f [] t
  let filter t ~f = rev (rev_filter t ~f)

  let find_map t ~f =
    let rec loop = function
    | [] -> None
    | x :: l ->
      match f x with
      | None -> loop l
      | Some _ as r -> r
    in
    loop t

  let find t ~f =
    let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
    in
    loop t

  let find_exn t ~f = ListLabels.find ~f t

  let findi t ~f =
    let rec loop i t =
      match t with
      | [] -> None
      | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
    in
    loop 0 t

  (** changing the order of arguments on some standard [List] functions. *)
  let exists t ~f = exists t ~f
  let for_all t ~f = for_all t ~f
  let iter t ~f = iter t ~f

  (** For the container interface. *)
  let fold t ~init ~f = fold_left t ~f ~init
  let fold_left = fold
  let to_array = Array.of_list
  let to_list t = t

  (** Tail recursive versions of standard [List] module *)

  let slow_append l1 l2 = rev_append (rev l1) l2

  let rec count_append l1 l2 count =
    match l2 with
    | [] -> l1
    | _ ->
      match l1 with
      | []               ->                         l2
      | [x1]             -> x1                   :: l2
      | [x1; x2]         -> x1 :: x2             :: l2
      | [x1; x2; x3]     -> x1 :: x2 :: x3       :: l2
      | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
      | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
        x1 :: x2 :: x3 :: x4 :: x5 ::
          (if count > 1000
           then slow_append tl l2
           else count_append tl l2 (count + 1))

  let append l1 l2 = count_append l1 l2 0


  let map_slow l ~f = rev (rev_map ~f l)

  let rec count_map ~f l ctr =
    match l with
    | [] -> []
    | [x1] ->
      let f1 = f x1 in
      [f1]
    | [x1; x2] ->
      let f1 = f x1 in
      let f2 = f x2 in
      [f1; f2]
    | [x1; x2; x3] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4] ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f x1 in
      let f2 = f x2 in
      let f3 = f x3 in
      let f4 = f x4 in
      let f5 = f x5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 1000
         then map_slow ~f tl
         else count_map ~f tl (ctr + 1))

  let map l ~f = count_map ~f l 0

  let fold_right l ~f ~init =
    fold ~f:(fun a b -> f b a) ~init (List.rev l)

  let rev_mapi l ~f ~i =
    let rec loop i acc = function
    | [] -> acc
    | h :: t -> loop (i + 1) (f i h :: acc) t
    in
    loop i [] l

  let rec count_mapi ~f l ctr =
    match l with
    | [] -> []
    | [x1] ->
      let f1 = f ctr x1 in
      [f1]
    | [x1; x2] ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      [f1; f2]
    | [x1; x2; x3] ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      let f3 = f (ctr + 2) x3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4] ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      let f3 = f (ctr + 2) x3 in
      let f4 = f (ctr + 3) x4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      let f1 = f ctr x1 in
      let f2 = f (ctr + 1) x2 in
      let f3 = f (ctr + 2) x3 in
      let f4 = f (ctr + 3) x4 in
      let f5 = f (ctr + 4) x5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 5000
          then rev_mapi ~f ~i:(ctr + 5) tl
          else count_mapi ~f tl (ctr + 5))

  let mapi l ~f = count_mapi ~f l 0

  let map2_slow l1 l2 ~f = List.rev (rev_map2 ~f l1 l2)

  let rec count_map2_exn ~f l1 l2 ctr =
    match l1, l2 with
    | [], [] -> []
    | [x1], [y1] ->
      let f1 = f x1 y1 in
      [f1]
    | [x1; x2], [y1; y2] ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      [f1; f2]
    | [x1; x2; x3], [y1; y2; y3] ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      let f3 = f x3 y3 in
      [f1; f2; f3]
    | [x1; x2; x3; x4], [y1; y2; y3; y4] ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      let f3 = f x3 y3 in
      let f4 = f x4 y4 in
      [f1; f2; f3; f4]
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl1,
      y1 :: y2 :: y3 :: y4 :: y5 :: tl2 ->
      let f1 = f x1 y1 in
      let f2 = f x2 y2 in
      let f3 = f x3 y3 in
      let f4 = f x4 y4 in
      let f5 = f x5 y5 in
      f1 :: f2 :: f3 :: f4 :: f5 ::
        (if ctr > 1000
          then map2_slow ~f tl1 tl2
          else count_map2_exn ~f tl1 tl2 (ctr + 1))
    | _, _ -> failwith "count_map2"

  let map2_exn l1 l2 ~f = count_map2_exn ~f l1 l2 0

  let iteri l ~f =
    ignore (fold l ~init:0 ~f:(fun i x -> let () = f i x in i + 1))

  let foldi t ~f ~init =
    snd (fold t ~init:(0, init) ~f:(fun (i, acc) v -> (i + 1, f i acc v)))

  let filteri l ~f =
    List.rev (foldi l
                ~f:(fun pos acc x ->
                    if f pos x then x :: acc else acc)
                ~init:[])

  let reduce l ~f = match l with
  | [] -> None
  | hd :: tl -> Some (fold ~init:hd ~f tl)

  let concat_map l ~f =
    let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (rev_append (f hd) acc) tl
    in
    aux [] l

  let concat_mapi l ~f =
    let rec aux cont acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (cont + 1) (rev_append (f cont hd) acc) tl
    in
    aux 0 [] l

  let merge l1 l2 ~cmp =
    let rec loop acc l1 l2 =
      match l1,l2 with
      | [], l2 -> rev_append acc l2
      | l1, [] -> rev_append acc l1
      | h1 :: t1, h2 :: t2 ->
        if cmp h1 h2 <= 0
        then loop (h1 :: acc) t1 l2
        else loop (h2 :: acc) l1 t2
    in
    loop [] l1 l2


  let rec last list = match list with
  | [x] -> Some x
  | _ :: tl -> last tl
  | [] -> None

  let remove_consecutive_duplicates list ~equal =
    let rec loop list accum = match list with
    | [] -> accum
    | hd :: [] -> hd :: accum
    | hd1 :: hd2 :: tl ->
      if equal hd1 hd2
      then loop (hd2 :: tl) accum
      else loop (hd2 :: tl) (hd1 :: accum)
    in
    rev (loop list [])

  let dedup ?(compare=Pervasives.compare) list =
    let equal x x' = compare x x' = 0 in
    let sorted = sort ~cmp:compare list in
    remove_consecutive_duplicates ~equal sorted

  let contains_dup ?compare lst = length (dedup ?compare lst) <> length lst

  let find_a_dup ?(compare=Pervasives.compare) l =
    let sorted = sort ~cmp:compare l in
    let rec loop l = match l with
      [] | [_] -> None
    | hd1 :: hd2 :: tl ->
      if compare hd1 hd2 = 0 then Some (hd1) else loop (hd2 :: tl)
    in
    loop sorted

  let init n ~f =
    if n < 0 then []
    else
      let rec loop i accum =
        assert (i >= 0);
        if i = 0 then accum
        else loop (i-1) (f (i-1) :: accum)
      in
      loop n []

  let rev_filter_map l ~f =
    let rec loop l accum =
      match l with
      | [] -> accum
      | hd :: tl ->
        match f hd with
        | Some x -> loop tl (x :: accum)
        | None   -> loop tl accum
    in
    loop l []
  let filter_map l ~f = List.rev (rev_filter_map l ~f)

  let filter_opt l = filter_map l ~f:(fun x -> x)

  let partition_map t ~f =
    let rec loop t fst snd =
      match t with
      | [] -> (rev fst, rev snd)
      | x :: t ->
        match f x with
        | `Fst y -> loop t (y :: fst) snd
        | `Snd y -> loop t fst (y :: snd)
    in
    loop t [] []


  let split_n t_orig n =
    if n <= 0 then
      ([], t_orig)
    else
      let rec loop n t accum =
        if n = 0 then
          (List.rev accum, t)
        else
          match t with
          | [] -> (t_orig, []) (* in this case, t_orig = List.rev accum *)
          | hd :: tl -> loop (n - 1) tl (hd :: accum)
      in
      loop n t_orig []
  let take t n = fst (split_n t n)
  let drop t n = snd (split_n t n)

  let split_while xs ~f =
    let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | t -> (rev acc, t)
    in
    loop [] xs

  let take_while t ~f = fst (split_while t ~f)
  let drop_while t ~f = snd (split_while t ~f)

  module Assoc = struct

    let get e l = try Some (assoc e l) with Not_found -> None
    let getq e l = try Some (assq e l) with Not_found -> None

    let mem = mem_assoc
    let memq = mem_assq

    let remove_assoc = remove_assoc
    let remove_assq = remove_assq

    let remove_and_get el list =
      let rec loop acc = function
        | []                      -> None
        | (e, v) :: t when e = el -> Some (v, (List.rev acc @ t))
        | h :: t                  -> loop (h :: acc) t
      in
      loop [] list

    let remove_and_getq el list =
      let rec loop acc = function
        | []                       -> None
        | (e, v) :: t when e == el -> Some (v, (List.rev acc @ t))
        | h :: t                   -> loop (h :: acc) t
      in
      loop [] list
  end

  let assoc = `Use_sub_module
  let assq = `Use_sub_module
  let mem_assoc = `Use_sub_module
  let mem_assq = `Use_sub_module
  let remove_assoc = `Use_sub_module
  let remove_assq = `Use_sub_module

end

module Array = ArrayLabels

module Option : sig
  exception No_value of string
  val value : 'a option -> default:'a -> 'a
  val value_exn : 'a option -> msg:string -> 'a
  val map : 'a option -> f:('a -> 'b) -> 'b option
  val iter : 'a option -> f:('a -> unit) -> unit
  val value_map : 'a option -> default:'b -> f:('a -> 'b) -> 'b
  val return : 'a -> 'a option
  val bind : 'a option -> f:('a -> 'b option) -> 'b option
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
end = struct
  exception No_value of string
  let value o ~default = match o with Some s -> s | None -> default
  let value_exn o ~msg =
    match o with
    | Some o -> o
    | None -> raise (No_value msg)
  let map o ~f =
    match o with
    | None -> None
    | Some s -> Some (f s)

  let maybe o ~f =
    match o with
    | None -> ()
    | Some s -> f s

  let iter o ~f =
    match o with
    | None -> ()
    | Some s -> f s

  let value_map o ~default ~f =
    match o with
    | Some s -> f s
    | None -> default
  let return s = Some s
  let bind o ~f =
    match o with
    | None -> None
    | Some s -> f s
  let (>>=) x f = bind x ~f


end


module Int = struct
  type t = int
  let compare (a: int) (b: int) = compare a b
  let to_string i = string_of_int i
  let of_string s = try Some (int_of_string s) with _ -> None

end
module Float = struct

  type t = float
  let compare (a: float) (b: float) = compare a b
  let to_string i = string_of_float i
  let of_string s = try Some (float_of_string s) with _ -> None
end
