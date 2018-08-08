class c = 
  let (* x => *) x (* <= x *) = 1 in
  object
    val v = x (* ? x *)
    method get_x = x (* ? x *)
  end
