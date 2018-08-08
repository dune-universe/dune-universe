module L = struct
  let (* length => *) length (* <= length *) = List.length
end
include L
let _ = length (* ? length *)

