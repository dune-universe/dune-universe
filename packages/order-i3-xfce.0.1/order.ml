 
 type 'a t = (int * 'a) array

let from_array (tab: 'a array) (compare : 'a -> 'a -> int) : 'a t =
  let tab' = Array.mapi (fun i e -> (i, e)) tab in
  let sorted = Array.copy tab' in
  Array.stable_sort (fun (_, e) (_, e') -> compare e e') sorted;
  let indexes : int array = Array.make (Array.length tab) (-1) in
  Array.iteri (fun i' (i, _) -> indexes.(i)<-i') sorted;

  Array.mapi (fun i e -> (*Printf.printf "%d, " e ;*) (e, tab.(i))) indexes


let ordered tab compare move_to_left =
  let tab' = from_array tab compare in
  Lwt_list.iteri_s
 (fun cur (dest, e) -> (
    for%lwt i=0 to cur - dest - 1 do
      move_to_left e
    done
  )) (Array.to_list tab')
  