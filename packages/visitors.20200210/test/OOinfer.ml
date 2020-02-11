class int_cell = object
  val mutable x = 0
  method get = x
  method incr y = x <- x + y
end
