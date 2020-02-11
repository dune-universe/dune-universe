class virtual ['self] cell (init) = object (self : 'self)
  val mutable x = init
  method get = x
  method set y = x <- self#check y
  method virtual check: _
end
