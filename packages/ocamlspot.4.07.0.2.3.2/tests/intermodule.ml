module M = struct
  let x = 1
end

include M

include Printf

let y = x

let z = printf
