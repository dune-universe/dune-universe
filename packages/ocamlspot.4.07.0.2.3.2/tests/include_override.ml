module Z1 : sig
  val zx : int
end = struct
  let (* Z1.zx => *) zx (* <= Z1.zx *) = 1
  let zy = 2
end

module Z2 = struct
  let zx = 2
  let (* Z2.zy => *) zy (* <= Z2.zy *) = 3
  include Z1
end

let _ = Z2.zx (* ? Z1.zx *) (* fixed bug : did not point to zx in Z1 *)
let _ = Z2.zy (* ? Z2.zy *)
