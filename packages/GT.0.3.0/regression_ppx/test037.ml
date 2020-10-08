type 'a t1 = [`A | `B of 'a] [@@deriving gt ~show ~gmap]
type 'a t2 = [`C | `D of 'a] [@@deriving gt ~show ~gmap]
type 'a t  = ['a t1 | 'a t2] [@@deriving gt ~show ~gmap]

let _ = 
  let a = `B (`B `A) in
  let rec mapt1 x =
    GT.fix0 (fun self -> GT.transform(t1) (new gmap_t1 self self) ()) x
  in
  let rec show1 x =
    GT.fix0 (fun self -> GT.transform(t1) (new show_t1 self self) ()) x
  in
  Printf.printf "a=%s, map a=%s\n" (show1 a) (show1 (mapt1 a));

  let b = `D (`D `C) in
  let rec mapt2 x =
    GT.fix0 (fun self -> GT.transform(t2) (new gmap_t2 self self) ()) x
  in
  let rec show2 x =
    GT.fix0 (fun self -> GT.transform(t2) (new show_t2 self self) ()) x
  in
  Printf.printf "b=%s, map b=%s\n" (show2 b) (show2 (mapt2 b));

  let c = `D (`B (`D `A)) in
  let rec mapt x =
    GT.fix0 (fun self -> GT.transform(t)  (new gmap_t self self) ()) x
  in
  let rec show x =
    GT.fix0 (fun self -> GT.transform(t)  (new show_t self self) ()) x
  in
  Printf.printf "c=%s, map c=%s\n" (show c) (show (mapt c))
