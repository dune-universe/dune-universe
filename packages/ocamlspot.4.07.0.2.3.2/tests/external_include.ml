module M = struct
  external ext : int -> int = "hogehoge"
end

module N = struct
  external ext : int -> int = "hagehage"
end

include M

let ext = ext

include N

let ext = ext
