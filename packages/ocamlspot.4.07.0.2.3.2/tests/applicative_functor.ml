module Make() = struct
  let (* x => *) x (* <= x *) = 1
end

module M = Make()

module N = M

let y = N.x (* ? x *)



