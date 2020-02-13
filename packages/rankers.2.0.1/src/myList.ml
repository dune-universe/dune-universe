(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

include BatList

(* count elements satisfying 'p' *)
let filter_count p l =
  fold_left (fun acc x ->
      if p x then acc + 1
      else acc
    ) 0 l

let filter_counts p l =
  let ok_count = ref 0 in
  let ko_count = ref 0 in
  iter (fun x ->
      if p x then incr ok_count
      else incr ko_count
    ) l;
  (!ok_count, !ko_count)

(* split a list into n parts (the last part might have
   a different number of elements) *)
let nparts n l =
  let len = length l in
  let res = ref [] in
  let curr = ref l in
  let m = int_of_float (BatFloat.ceil (float len /. float n)) in
  for _ = 1 to n - 1 do
    let xs, ys = takedrop m !curr in
    curr := ys;
    res := xs :: !res
  done;
  rev (!curr :: !res)

(* create folds of cross validation; each fold consists in (train, test) *)
let cv_folds n l =
  let test_sets = nparts n l in
  let rec loop acc prev curr =
    match curr with
    | [] -> acc
    | x :: xs ->
      let before_after = flatten (rev_append prev xs) in
      let prev' = x :: prev in
      let train_test = (before_after, x) in
      let acc' = train_test :: acc in
      loop acc' prev' xs in
  loop [] [] test_sets

(* dump list to file *)
let to_file (fn: string) (to_string: 'a -> string) (l: 'a list): unit =
  Utls.with_out_file fn (fun out ->
      iter (fun x -> fprintf out "%s\n" (to_string x)) l
    )

(* factorize code using parmap *)
let parmap ?init:(init = fun _ -> ()) ?pin_cores:(pin_cores = false)
    ?chunksize:(chunksize = 1)
    (ncores: int) (f: 'a -> 'b) (l: 'a list)
  : 'b list =
  if ncores <= 1 then map f l (* don't invoke parmap in vain *)
  else
    begin
      if not pin_cores then Parmap.disable_core_pinning ();
      Parmap.parmap ~ncores ~init ~chunksize f (Parmap.L l)
    end

(* factorize code using parmap *)
let parmapi ?init:(init = fun _ -> ()) ?pin_cores:(pin_cores = false)
    (ncores: int) (f: int -> 'a -> 'b) (l: 'a list): 'b list =
  if ncores <= 1 then mapi f l (* don't invoke parmap in vain *)
  else
    begin
      if not pin_cores then Parmap.disable_core_pinning ();
      Parmap.parmapi ~ncores ~init ~chunksize:1 f (Parmap.L l)
    end

let really_take n l =
  let res = take n l in
  assert(length res = n);
  res

let rev_combine l1 l2 =
  let rec loop acc l r =
    match (l, r) with
    | ([], []) -> acc
    | (x :: xs, y :: ys) -> loop ((x, y) :: acc) xs ys
    | _ -> raise (Invalid_argument "MyList.rev_combine: list lengths differ")
  in
  loop [] l1 l2

let rev_sort cmp l =
  sort (fun x y -> cmp y x) l

(* List.min with a comparison function *)
let minimum cmp l =
  fst (min_max ~cmp l)

(* List.max with a comparison function *)
let maximum cmp l =
  snd (min_max ~cmp l)

let numerate offset l =
  mapi (fun i x -> (i + offset, x)) l
