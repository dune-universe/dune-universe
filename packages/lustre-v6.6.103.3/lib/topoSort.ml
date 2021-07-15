(** Time-stamp: <modified the 17/07/2017 (at 16:55) by Erwan Jahier> *)


module type PartialOrder =
  sig
    type elt
    type store
    val have_dep : store -> elt -> bool
    val find_dep : store -> elt -> elt list
    val remove_dep:store -> elt -> store
  end

module type S =
  sig
    type elt
    type store
    exception DependencyCycle of elt * elt list
    val check_there_is_no_cycle : store -> elt list -> unit
    val f : store -> elt list -> elt list
end


module Make(PO: PartialOrder) = struct
  type elt = PO.elt
  type store = PO.store

  module Ordered = struct
    type t=elt
    let compare = compare
  end
  module Mapt = Map.Make(Ordered)

  exception DependencyCycle of elt * elt list

  type color = Grey | Black (* in process | done *)
  type color_table = color Mapt.t

  let (grey_actions : color_table -> elt list) =
    fun ct -> 
    Mapt.fold
      (fun x color acc -> if color=Grey then x::acc else acc) ct []

  let (smallest_cycle : store -> elt -> elt list -> elt list) =
    fun store x al ->
    (* al contains a cycle at x, and no other one *)
    let rec (f: elt -> elt list -> elt list) =
      fun c path ->
      let deps = PO.find_dep store c in
      let succ = List.filter (fun x -> List.mem x al) deps in
      let cycles = List.fold_left
                     (fun acc y ->
                      try if x = y then (c::path)::acc else (f y (c::path))::acc
                      with Not_found -> acc)
                     [] succ
      in
      let res,_ = (* compute the smallest amond cycles *)
        match cycles with
        | [] -> raise Not_found
        | y::l -> List.fold_left
                    (fun (l1, s1) l2 ->
                     let s2 = List.length l2 in
                     if s1<s2 then l1, s1 else l2, s2
                    )
                    (y, List.length y) l
      in
      List.rev res
    in
    f x []
             
  let rec (visit : store -> color_table -> elt -> color_table) =
  fun store color_t n ->
    if not (PO.have_dep store n) then Mapt.add n Black color_t else
      let color_t =
	     List.fold_left
	       (fun color_t nt -> 
	         try
	           match Mapt.find nt color_t with
		        | Grey ->
                 let c = smallest_cycle store n (grey_actions color_t) in
                 raise (DependencyCycle (n, c))
		          | Black -> color_t
	         with 
		          (* The node [nt] is white *)
		          Not_found -> visit store color_t nt
	       ) 
	       (Mapt.add n Grey color_t)
	       (PO.find_dep store n)
      in
	   Mapt.add n Black color_t

(* TEDLT *)
let (check_there_is_no_cycle : store -> elt list -> unit) =
  fun store l ->
  ignore (List.fold_left (fun acc x -> visit store acc x) Mapt.empty l)

let (f : store -> elt list -> elt list) =
  fun store l -> 
    let visited_init = 
      List.fold_left (fun acc x -> Mapt.add x false acc) Mapt.empty l 
    in
    let rec aux (store:store) (acc:elt list) (l:elt list)  (visited:bool Mapt.t) = 
      (* The graph contains no cycle! *)
      match l with
	     | [] -> List.rev acc
	     | x::tail -> 
	       if (try Mapt.find x visited 
              with Not_found -> 
                true (* migth occur if a dep is not the list to be sorted *))
	       then 
	         aux store acc tail visited 
          else 
              let x_succ = PO.find_dep store x in
              if x_succ = [] then
	             aux store (x::acc) tail (Mapt.add x true visited)
              else
	             aux (PO.remove_dep store x) acc (x_succ @ l)  visited
    in
    check_there_is_no_cycle store l;
    aux store [] l visited_init

end
