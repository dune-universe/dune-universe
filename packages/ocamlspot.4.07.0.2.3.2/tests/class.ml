class (* class c => *) c (* <= class c *) =  (* CR: pos can be improved *)
  let (* x => *) x (* <= x *) = 1 in
  let (* y => *) y (* <= y *) = x (* ? x *) in
  object
    val x = x (* ? x *)
    val y = y (* ? y *)
    val (* vx => *) vx (* <= vx *) = 1 
    method m = vx (* ? vx *)
end 

let v = new c (* ? class c *)
;;

