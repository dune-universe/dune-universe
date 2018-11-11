type empty =  {
  all: 'a . 'a} 
type 'a neg = 'a -> empty 
type ('a,'b) arrow = 'a -> 'b neg neg 
let run f =
  let res = ref None in
  let r x = res := (Some x); raise Exit in
  Printf.printf "start\n%!";
  (let _ = try f r; assert false with | Exit  -> () in
   match !res with | None  -> assert false | Some x -> x)
let rec fib kk =
  kk
    (fun n  kk  ->
       if n <= 1
       then kk 1
       else
         (let kk a0 =
            let kk a1 = kk ((( + ) a1) a0) in
            let v = n - 1 in let kk f = f v kk in fib kk in
          let v = n - 2 in let kk f = f v kk in fib kk))
let n = 30
let f kk = let v = n in let kk f = f v kk in fib kk
let _ = Printf.printf "fib(%d) = %d\n%!" n (run f)
type ('a,'b) sum =  
  | Inl of 'a
  | Inr of 'b 
let print_list ch l = List.iter (Printf.fprintf ch "%d ") l
let print_sum f g =
  function
  | Inl l -> Printf.printf "Inl %a\n%!" f l
  | Inr l -> Printf.printf "Inr %a\n%!" g l
let inl kk = kk (fun x  kk  -> kk (Inl x))
and inr kk = kk (fun x  kk  -> kk (Inr x))
let cons kk = kk (fun x  kk  -> kk (fun l  kk  -> kk (x :: l)))
let f kk = kk (fun x  kk  -> kk x)
let g kk =
  kk
    (fun x  kk  ->
       kk
         (fun y  kk  ->
            let kk v =
              let kk f = f v kk in
              let kk v = let kk f = f v kk in kk x in kk y in
            kk y))
let g' kk =
  kk
    (fun x  kk  ->
       kk
         (fun y  kk  ->
            let v = y in
            let kk f = f v kk in let v = y in let kk f = f v kk in kk x))
let g'' kk =
  kk
    (fun x  kk  ->
       kk (fun y  kk  -> let v = y in let kk f = f v kk in x y kk))
type ('a,'b) stream = ('a,'b) stream' -> 'b neg neg 
and ('a,'b) stream' = ('a* ('a,'b) stream) -> 'b neg neg 
let rec init_aux: (int -> 'a) -> int -> ('a,'b) stream =
  fun f  n  g  -> g ((f n), (init_aux f (n + 1)))
let init: (int -> 'a) -> ('a,'b) stream = fun f  -> init_aux f 0
let rec extract:
  bool -> (('a,'b) stream -> (int -> int list neg neg) neg neg) neg neg =
  fun b  kk  ->
    kk
      (fun s  kk  ->
         kk
           (fun n  kk  ->
              if n = 0
              then kk []
              else
                (let kk v = let kk f = f v kk in kk s in
                 kk
                   (fun c  kk  ->
                      let (p,s') = c in
                      Printf.printf "%b %d %d\n%!" b p n;
                      (let kk v =
                         let kk f = f v kk in
                         let v = p in let kk f = f v kk in cons kk in
                       let v = n - 1 in
                       let kk f = f v kk in
                       let v = s' in let kk f = f v kk in extract b kk)))))
let test' n kk =
  let kk v =
    let kk f = f v kk in
    let v = init (fun n  -> n) in let kk f = f v kk in extract true kk in
  kk n
let _ = print_list stdout (run (test' 15))
let extract_sum:
  ((('a,'c) stream,('b,'c) stream) sum ->
     (int -> (int list,int list) sum neg neg) neg neg)
    neg neg
  =
  fun kk  ->
    kk
      (fun s  kk  ->
         kk
           (fun n  kk  ->
              match s with
              | Inl s ->
                  let kk v = let kk f = f v kk in inl kk in
                  let kk v =
                    let kk f = f v kk in
                    let kk v = let kk f = f v kk in extract true kk in kk s in
                  kk n
              | Inr s ->
                  let kk v = let kk f = f v kk in inr kk in
                  let kk v =
                    let kk f = f v kk in
                    let kk v = let kk f = f v kk in extract false kk in kk s in
                  kk n))
let rec tT:
  ('a,'c) stream' ->
    ('b,'c) stream' -> ((('a,'b) sum,'d) stream -> 'e) neg neg
  =
  fun k1  k2  kk  ->
    kk
      (fun s  kk  ->
         let kk v = let kk f = f v kk in kk s in
         kk
           (fun c  kk  ->
              let (x,sp) = c in
              match x with
              | Inl x1 ->
                  (Printf.printf "L%!";
                   k1
                     (x1,
                       (fun k1p  ->
                          let v = sp in
                          let kk f = f v kk in (tT k1p k2 kk).all)))
              | Inr x2 ->
                  (Printf.printf "R%!";
                   k2
                     (x2,
                       (fun k2p  ->
                          let v = sp in
                          let kk f = f v kk in (tT k1 k2p kk).all)))))
