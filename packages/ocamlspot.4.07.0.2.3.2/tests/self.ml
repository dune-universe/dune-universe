class c = object (* self => *) (self)  (* <= self *)
  val x = 1
  method m = self (* ? self *) #m + 1
end
