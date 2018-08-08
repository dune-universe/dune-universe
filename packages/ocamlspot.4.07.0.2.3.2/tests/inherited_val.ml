class c = object
  val a = 1
end

class c' = object
  inherit (* a => *) c (* <= a *) (* CR jfuruse: this should point to c's a *)
  method get_a = a (* ? a *)
end
