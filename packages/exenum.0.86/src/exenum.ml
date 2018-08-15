open Exenum_internals

open Parts
open Exen
open Convenience

type 'a enum = 'a Exen.t
type 'a t = 'a enum

let get_exen x = x

let product   = Exen.product
let union     = Exen.union
let from_list = Exen.from_list
let single ?name x  = from_list ?name [x]
let map       = Exen.map
let get       = Exen.get
let cardinal  = Exen.cardinal
let pay       = Exen.pay
let sub       = Exen.sub

(* Boilerplate code. *)
type ('a,'b) tuple2 = C1_2 of 'a | C2_2 of 'b

let c1_2 x = C1_2 x
let c2_2 x = C2_2 x

let proj2 = function
  | [C1_2 a ; C2_2 b] -> (a,b)
  | _ -> assert false

let pair ex1 ex2 = 
  let mex1 = map ex1 c1_2
  and mex2 = map ex2 c2_2 in
  map (product [mex1 ; mex2]) proj2

type ('a,'b,'c) tuple3 = C1_3 of 'a | C2_3 of 'b | C3_3 of 'c

let c1_3 x = C1_3 x
let c2_3 x = C2_3 x
let c3_3 x = C3_3 x

let proj3 = function
  | [C1_3 a ; C2_3 b ; C3_3 c] -> (a,b,c)
  | _ -> assert false

let triple ex1 ex2 ex3 =
  let mex1 = map ex1 c1_3
  and mex2 = map ex2 c2_3 
  and mex3 = map ex3 c3_3 in
  map (product [mex1 ; mex2 ; mex3]) proj3

let tuple2 = pair
let tuple3 = triple
let tuple4 e1 e2 e3 e4 = map (pair (triple e1 e2 e3) e4) (fun ((a,b,c), d) -> (a,b,c,d))

let tuple5 e1 e2 e3 e4 e5 = map (pair (triple e1 e2 e3) (pair e4 e5)) (fun ((a,b,c), (d,e)) -> (a,b,c,d,e))
let tuple6 e1 e2 e3 e4 e5 e6 = map (pair (triple e1 e2 e3) (triple e4 e5 e6)) (fun ((a,b,c),(d,e,f)) -> (a,b,c,d,e,f))

(******************************************************************)
(*                     BUILTINS                                   *)
(******************************************************************)

let char_part =
  { p_cardinal = boi 256 ;
    compute = (fun n -> Char.chr (iob n)) }

(* Usual chars: from 32 to 125. *)
let usual_char_part =
  { p_cardinal = boi (125 - 32 + 1) ;
    compute = (fun n -> Char.chr (32 + iob n)) }

let e_unit  = from_list ~name:"Unit" [ () ]
let e_bool  = from_list ~name:"Bool" [true ; false]
let e_char  = from_single_part (lazy "Char") char_part
let e_pchar = from_single_part (lazy "Printable-Char") usual_char_part

(* Big ints & ints *)
let e_zero = from_list ~name:"Zero" [bigzero]
let e_one = from_list ~name:"One" [bigone]

let e_bigpos =

  let big2n n = n ++ n in
  let big2np1 n = succ (big2n n) in
  
  let rec e_bigpos = lazy (union [e_one ; map (pay e_bigpos) big2n ; map (pay e_bigpos) big2np1 ]) in
  Lazy.force e_bigpos



let e_bignat = union [e_zero ; e_bigpos]

let e_bigneg = map e_bigpos minus

let e_bigint = union [e_zero ; e_bigneg ; e_bigpos]

let biginterval_part a b =
  { p_cardinal = succ (b -- a) ;
    compute = (fun n -> a ++ n) }

let e_biginterval a b = from_single_part (lazy (Printf.sprintf "[%s-%s]" (sob a) (sob b))) (biginterval_part a b)

let interval_part a b =
  { p_cardinal = boi (b - a + 1) ;
    compute = (fun n -> a + iob n) }

let e_interval a b = from_single_part (lazy (Printf.sprintf "[%d-%d]" a b)) (interval_part a b)

let bmax_int = boi max_int

let toint b = try iob (bigmod b bmax_int) with _ -> 0

let e_nat = map e_bignat toint 
let e_pos = map e_bigpos toint
let e_neg = map e_bigpos (fun b -> - (toint b))
let e_int = union [single 0 ; e_pos ; e_neg]
    
(* Strings *)
let e_string_from_echar enum_chars =
  let rec enum = lazy ( union [ map e_unit (fun () -> "") ; 
				map (pair (map enum_chars (String.make 1)) (pay enum))
				  (fun (a,b) -> a ^ b) 
			      ] )
  in
  Lazy.force enum

let e_string = e_string_from_echar e_pchar
let e_rstring charlist = e_string_from_echar (from_list ~name:"The given chars" charlist)

let e_emptylist () = from_list ~name:"Emptylist" [ [] ]

(* Enumerations of lists. *)
let e_list exen =
  let rec enum = lazy (union [ e_emptylist () ;
			       map (pair exen (pay enum))
				 (fun (a,b) -> a :: b)
			     ] )
  in
  Lazy.force enum

let e_ne_list exen = map (pair exen (e_list exen)) (fun (a,b) -> a :: b)
  
(* Enumerations of arrays. *)
let e_array exen = map (e_list exen) Array.of_list

let e_option exen = union [single None ; map exen (fun x -> Some x)]

let bigshow exen to_string n1 n2 =
  for i = 0 to n2 - 1 do
    let index = n1 +++ i in
    Printf.printf "Value #%d is %s\n" i (to_string (get exen index)) ;
  done ;
  ()

let show exen to_string n1 n2 = bigshow exen to_string (boi n1) n2

let tester exen ?from ?upto ?verbose_period ?tos ~len f =
  Tester.gen_tester (Printf.printf "%s%!") ?tos (fun a b -> ignore(a) ; b ()) () exen ?from ?upto ?verbose_period ~len f


