let (* x => *) x (* <= x *) = 1 [@x]
let (* y => *) y (* <= y *) = 1 [@@x]
[@@@x]

let z = x (* ? x *) + y (* ? y *)
