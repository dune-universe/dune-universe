

let hash_k = Int64.(shift_left 1L 31)

let rec jmp_hash j n key =

  let key = Int64.(succ (mul key 2862933555777941757L)) in
  let key' = Int64.(succ (shift_right_logical key 33)) in

  let a = Int64.succ j in
  let j' = Int64.(div (mul a hash_k) key') in

  if j' >= Int64.of_int n then
    j
  else
    jmp_hash j' n key


             
let host ~hosts key =
  Int64.to_int (jmp_hash 0L hosts key)

               
let lookup t key =
  (*let key = Cstruct.BE.get_uint64 digest 0 in  *)
  let hosts = List.length t in 

  host hosts key |> (List.nth t) 



let rec range i j = if i > j then [] else i :: (range (i+1) j)
let range_rev j i = range i j |> List.rev                                                 

                                   

                                                 
let slice t succ f =

  let size = List.length t in 
  

  let last =
    (List.length t) - 1
  in  


  let max =
    if (f <= size) then succ + (f - 1)
    else succ + last
  in


  let successors =
    if max <= last then (range succ max)

    else
      let diff = max - size in 
      (range succ last) @ (range 0 diff)

  in

  List.fold_left (fun acc x ->
    try ( acc @ [ (List.nth t x) ] ) 
    with _ -> acc
  ) [] successors


                 
                            
      
    
    
  
  

let shards t key f =
  (* let key = Cstruct.BE.get_uint64 digest 0 in *)
  let hosts = List.length t in 

  let succ = host hosts key in
  slice t succ f 

