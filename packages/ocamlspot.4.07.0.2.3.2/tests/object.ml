class (* c0 => *) c0 (* <= c0 *) = object
  val a = 1
  method m = a
end 


class (* c => *) c (* <= c *) ((* p => *) p (* <= p *) : int) = 
  let (* x => *) x (* <= x *) = 1 in
  let p' = p (* ? p *) in
  object (* self => *)(self)(* <= self *)
    inherit (* a => *) c0 (* <= a *)

    val mutable (* y => *) y (* <= y *) = x
    val z = x (* ? x *)
    val p'' = p (* ? p *)
    method f () = x (* ? x *)
    method g () = y (* ? y *)
    method h () = self(* ? self *)#g ()
    method i () = a (* ? a *) (* We cannot follow into c0... *)
    method get_p = p (* ? p *)
    method get_p' = p'
    initializer
      y <- 42
  end


let _ = 
  let (* o => *) o (* <= o *) : c (* ? c *) = new c (* ? c *) 42 in
  o(*? o *)#f ()

let o = 
  let (* yy => *) yy (* <= yy *) = 2 in
object 
  val (* xx => *) xx (* <= xx *) = 1 
  method get_xx = xx (* ? xx *) 
  method get_yy = yy (* ? yy *)
end
