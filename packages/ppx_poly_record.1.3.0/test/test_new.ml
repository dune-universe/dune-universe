let r1 = !{ x = 1; y = 2 }
let () = assert (r1#!x = 1 && r1#!y = 2)

let f r = !{ r with x = 2; y = 3 }
let () = assert ( (f r1)#!x = 2 )

let g r = r#!x := 1
let r3 = !{ x = ref 0 }
let () = g r3 ; assert(!(r3#!x) = 1)

