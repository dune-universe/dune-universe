(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

module type BASE =
  sig
    val b : int
  end


module type TABLE =
  sig
    exception Not_found
    exception Conflict
    type 'a t
    type key
    val empty : 'a t
    val add : ?override:bool -> key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  end

module Make_table (Base : BASE)=
  struct

    exception Not_found
    exception Conflict

    type 'a t = 
        Nil
      | T of ('a option * 'a t) array

    type key = int

(*    let create () = T (Array.create Base.b (None, Nil)) *)
    let create () = T (Array.make Base.b (None, Nil)) 

    let empty =  Nil

    let add ?(override=false) n attr table = 
      let rec insert1 n table =
        match table with
          Nil  -> insert1 n (create ())
        | T ar -> let (r, i) = (n / Base.b, n mod Base.b) in
                  let (a, tb) = ar.(i) 
                  in 
                  if r = 0
                  then 
		    match a,override with
		      | None,_ -> ar.(i) <- (Some attr, tb); T ar
		      | Some _,false -> raise Conflict
		      | Some _ ,true -> ar.(i) <- (Some attr, tb); T ar
                  else (ar.(i) <- (a, insert1 r tb);T ar)
      in
      insert1 n table

    let rec find n table =
      match table with
        Nil  -> raise Not_found
      | T ar -> let (r, i) = (n / Base.b, n mod Base.b) in
                let (a, tb) = ar.(i)
                in
                if r = 0
                then match a with
                       None   -> raise Not_found
                     | Some b -> b 
                else find r tb 

    let fold f acc table =
      let rec fold_aux q acc = function
	| Nil -> acc
	| T ar ->
	    let _,new_acc =
	      Array.fold_left
		(fun (i,acc) -> function
		   | Some v,_ -> i+1,f (q*Base.b+i) v acc
		   | None,_ -> i+1,acc)
		(0,acc)
		ar in
	      snd (Array.fold_left
		(fun (i,acc) (_,t) -> i+1,fold_aux (q+1) acc t)
		(0,new_acc)
		ar) in
	fold_aux 0 acc table
	      
		  

  end
