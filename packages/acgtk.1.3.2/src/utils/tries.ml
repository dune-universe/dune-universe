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

module Tries =

  struct

(*    type 'a option = None | Some of 'a *)

    type 'a t = ST of 'a option * (char * 'a t) list
    type key=string

    exception Not_found

    exception Conflict
 
    let empty = ST (None, [])

    let explode str = 
      let rec explode_aux i ls =
        if i = -1 then ls
                  else explode_aux (i-1) ((String.get str i)::ls)
      in
      explode_aux (String.length str - 1) []

    let add ?(override=false) id attr smtb =
      let rec insert1 lts (ST (a, s)) =
        match lts with
(*           []    -> (match a with *)
(*                       None   -> ST (Some attr, s) *)
(*                     | Some b -> ST (Some attr, s)(\*raise Conflict*\)) *)
(*         | l::rm -> ST (a, insert2 l rm s) *)
            []    -> (match a,override with
			  None,_   -> ST (Some attr, s)
			| Some b,true -> ST (Some attr, s)
			| Some b,false -> raise Conflict)
          | l::rm -> ST (a, insert2 l rm s)
      and     insert2 lt lts stls =
        match stls with
            []        -> [lt, insert1 lts empty]  
          | (l,i)::rm -> if lt = l 
            then (lt, insert1 lts i)::rm
            else if lt <= l
            then (lt, insert1 lts empty)::stls
            else (l,i)::(insert2 lt lts rm)
      in 
	insert1 (explode id) smtb

    let find w smtb =
      let rec lookup1 lts (ST (a, s)) = 
        match lts with
          []    -> (match a with 
                      None   -> raise Not_found
                    | Some b -> b)
        | l::rm -> lookup2 l rm s
      and     lookup2 lt lts stls =
        match stls with 
          []        -> raise Not_found
        | (l,i)::rm -> if lt = l 
                       then lookup1 lts i
                       else if lt <= l
                            then raise Not_found
                            else lookup2 lt lts rm
      in
      lookup1 (explode w) smtb

      let content tr =
        let rec tr_to_list tr ls =
          match tr with
            ST (None, [])          -> ls
          | ST (Some a, [])        -> a::ls
          | ST (None, (_,t)::rm)   -> trls_to_list rm (tr_to_list t ls)
          | ST (Some a, (_,t)::rm) -> trls_to_list rm (tr_to_list t (a::ls))
        and trls_to_list trls ls =
          match trls with
            []         -> ls
          | (_, t)::rm ->  trls_to_list rm (tr_to_list t ls)
      in
      List.rev (tr_to_list tr [])


      let implode lst =
	let buff = Buffer.create (List.length lst) in
	let() = List.fold_right (fun c _ -> Buffer.add_char buff c) lst () in
	  Buffer.contents buff

      let fold f acc tr =
	let rec fold_aux key acc = function
	  | ST (None,trs) -> List.fold_left (fun acc (c,t) -> fold_aux (c::key) acc t) acc trs
	  | ST (Some v,trs) -> 
	      let new_acc = f (implode key) v acc in
		(List.fold_left (fun acc (c,t) -> fold_aux (c::key) acc t) new_acc trs) in
	  fold_aux [] acc tr
	

  end
