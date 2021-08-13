let identity x = x
let compose f g x = f (g x)
let swap f x y = f y x

let (<==) f g x = f (g x)
let (==>) f g x = g (f x)
