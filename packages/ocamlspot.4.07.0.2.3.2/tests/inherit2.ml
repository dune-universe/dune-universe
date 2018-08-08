class c x = object
  val x = x
end

class c2 = object
  inherit let x = 1 in c x as super
  method m = x
end

