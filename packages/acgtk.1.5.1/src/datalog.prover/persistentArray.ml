open UtilsLib.Utils

module PersistentArray =
struct
  type 'a t = 'a data ref
  and 'a data =
  | Arr of 'a array
  | Diff of (int*'a*'a t)
  | Invalid

  exception Unaccessible
  exception Not_found
    
  exception Store_Not_found

  let init n f = ref (Arr (Array.init n f))

  let make n d = ref (Arr (Array.make n d))

  let of_list_rev l =
      let length,map=
	List.fold_left
	  (fun (l,m) e -> l+1,IntMap.add l e m)
	  (0,IntMap.empty)
	  l in
      init length (fun i -> IntMap.find i map)

(*  let rec get_v1 i t =
    match !t with
    | Arr a -> a.(i+1)
    | Diff (j,v,_) when j=i -> v
    | Diff (_,_,t') -> get_v1 i t'
    | Invalid -> raise Unaccessible
 *)
           
  let set_v1 i v t =
    match !t with
    | Arr a as n ->
      let old_v=a.(i) in
      let () = a.(i) <- v in
      let res = ref n in
      let () = t := Diff(i,old_v,res) in
      res
    | Diff _ -> ref (Diff (i,v,t))
    | Invalid  -> raise Unaccessible

  (* TODO: can it be made tail-recursive (see Filliatre &
     Conchon's paper) *)
  let rec reroot t =
    match !t with
    | Arr _ -> ()
    | Diff (i,v,t') ->
      let () = reroot t' in
      begin
	match !t' with
	| Arr a as n ->
	  let () = a.(i) <- v in
	  let () = t := n in
	  t' := Invalid
	| Diff _ -> failwith "Bug: rerooted array shoul be a Arr of a"
	| Invalid -> failwith "Bug: rerooted array shoul be a Arr of a"
      end
    | Invalid -> raise Unaccessible

  let get_aux i t =
    match !t with
    | Arr a -> a.(i)
    | Diff (i,v,t') ->
      let () = reroot t' in
      begin
	match !t' with
	| Arr a as n ->
	  let () = a.(i) <- v in
	  let () = t := n in
	  let () = t' := Invalid in
	  a.(i)
	| Diff _ -> failwith "Bug: rerooted array shoul be a Arr of a"      
	| Invalid -> failwith "Bug: rerooted array shoul be a Arr of a"      
      end
    | Invalid -> raise Unaccessible

  let get i t =
    try
      get_aux (i-1) t
    with
    | Invalid_argument arg when arg="index out of bounds" -> raise Store_Not_found


  let set_aux i v t =
    let () = reroot t in
    match !t with
    | Arr a as n ->
      let old_v=a.(i) in
      let () = a.(i) <- v in
      let res = ref n in
      let () = t := Diff(i,old_v,res) in
      res
    | Diff _ -> failwith "Bug: rerooted array shoul be a Arr of a"
    | Invalid -> failwith "Bug: rerooted array shoul be a Arr of a"

  let set i v t = set_aux (i-1) v t


  let rec print f t =
    match ! t with
    | Arr a -> 
      Array.iteri (fun i v -> Printf.printf " %i:%s\n" i (f v)) a 
    | Diff (i,v,t') -> 
      let () = Printf.printf "d%i:%s\n" i (f v) in
      print f t'
    | Invalid  -> Printf.printf "Inaccessible value\n"

  let print_and_reroot f t =
    let () = reroot t in
    match ! t with
    | Arr a -> Array.iteri (fun i v -> Printf.printf "%i:%s" i (f v)) a
    | _ -> failwith "Bug: rerooted array shoul be a Arr of a"

  let length t =
    let () = reroot t in
    match !t with
    | Arr a -> Array.length a
    | Diff _ -> failwith "Bug: rerooted array shoul be a Arr of a"      
    | Invalid -> failwith "Bug: rerooted array shoul be a Arr of a"      

  let rec copy_aux t =
    match !t with
    | Arr a ->  Array.copy a
    | Diff (i,v,a) -> 
      let res = copy_aux a in
      let () = res.(i) <- v in
      res
    | Invalid -> raise Unaccessible

  let copy t = ref (Arr (copy_aux t))



end
