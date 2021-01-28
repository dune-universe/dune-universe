class ['self] c = object (_ : 'self)
  method identity (x : 'a) : 'a = x
end

let b : bool =
  new c # identity true
let i : int =
  new c # identity 0
