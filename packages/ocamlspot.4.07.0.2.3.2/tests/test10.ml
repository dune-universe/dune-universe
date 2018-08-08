module (* M => *) M (* <= M *) = struct
  let (* M.y => *) y (* <= M.y *) = 2
end


let (* x => *) x (* <= x *) = 1

let y = 2

include M

let z = 3

