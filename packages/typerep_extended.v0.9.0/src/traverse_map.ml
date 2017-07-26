open Core_kernel

let option t ~f =
  match t with
  | Some arg ->
    let arg' = f arg in
    if phys_equal arg arg' then t else Some arg'
  | None -> t
;;

let array array ~f =
  let rec aux ~f array index size =
    if index >= size then array else
      let elt = array.(index) in
      let felt = f elt in
      if phys_equal elt felt then aux ~f array (succ index) size else begin
        let new_array = Array.copy array in
        new_array.(index) <- felt;
        for i = succ index to pred size do
          new_array.(i) <- f array.(i)
        done;
        new_array
      end
  in
  aux ~f array 0 (Array.length array)
;;
