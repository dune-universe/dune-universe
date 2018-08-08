module M = struct
  type t
  let x = 1
end

include M

module N = struct
  include M
end
