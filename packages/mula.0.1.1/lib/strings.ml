module UsualString : Match.S with type ch = char and type t = string = struct
  type ch = char
  type t = string

  let length = String.length
  let get = String.get

  let equal = Char.equal
end

include Match.Make(UsualString)
