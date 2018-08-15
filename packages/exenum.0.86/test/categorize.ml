open Exenum_internals.Convenience

type 'b category = 
    { name : (unit -> string) ;
      size : (unit -> Z.t) ;
      id : 'b }

type ('a,'b) catfun = ('a -> 'b category)

let size cat = cat.size ()
let name cat = cat.name ()
let id   cat = cat.id

(* BOOL *)
let bool_category =
  { name = (fun () -> "Bool") ;
    size = (fun () -> boi 2) ;
    id = () }

let cat_bool _ = bool_category

(* INT *)
let rec bitsize n x =
  if x = 0 then n
  else bitsize (n+1) (x lsr 1)

(* Signed *)
let cat_int x =
  let bits = bitsize 0 (abs x) in
  { name = (fun () -> "Int-" ^ string_of_int bits) ;
    size = (fun () -> boi (1 lsl bits)) ;  (* Actually 2 * (1 lsl (bits - 1)), with a particular case for 0. *)
    id   = bits }

(* String *)
let cat_rstring chars =
  
  let charlen = List.length chars in

  fun s ->
    let len = String.length s in
    { name = (fun () -> "Rstring-" ^ string_of_int len) ;
      size = (fun () -> charlen *^ len) ;
      id   = len }

(* Pairs *)
let pair f1 f2 =
  
  fun (v1, v2) ->

    let cat1 = f1 v1
    and cat2 = f2 v2 in

    { name = (fun () -> cat1.name () ^ "*" ^ cat2.name ()) ;
      size = (fun () -> cat1.size () ** cat2.size ()) ;
      id   = (cat1.id, cat2.id) }

(* Triples *)
let triple f1 f2 f3 =
  
  fun (v1, v2, v3) ->

    let cat1 = f1 v1
    and cat2 = f2 v2 
    and cat3 = f3 v3 in

    { name = (fun () -> cat1.name () ^ "*" ^ cat2.name () ^ "*" ^ cat3.name () ) ;
      size = (fun () -> cat1.size () ** cat2.size () ** cat3.size () ) ;
      id   = (cat1.id, cat2.id, cat3.id) }

(* Lists *)
let list f =
  
  fun l ->   
    let lc  = List.map f l in
 
    { name = (fun () -> "[" ^ (sep (fun c -> c.name ()) "," lc) ^ "]") ;
      size = (fun () -> myfold lc bigone (fun acu c -> acu ** c.size ())) ;
      id   = List.map (fun c -> c.id) lc }
