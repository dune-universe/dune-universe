type t = { x : int; y : int }

let r_mono = { x = 1; y = 2 } 
let () = assert (r_mono.x = 1); assert (r_mono.y = 2)

let r1 = [%poly_record { x = 1; y = 2 } ]
let () = [%poly_record assert(r1.x = 1 && r1.y = 2)] 

let r2 = [%poly_record let x = 1 and y = 2 in { x; y } ]
let f r = [%poly_record { r with x = 2; y = 3 } ]
let () = assert ( [%poly_record (f r2).x] = 2 )

let g r = [%poly_record r.x <- 1]
let r3 = [%poly_record { x = ref 0 }]
let () = g r3 ; assert(![%poly_record r3.x] = 1)

