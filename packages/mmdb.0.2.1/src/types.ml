module Path = struct
  type t = string [@@deriving show]

  let of_string = Base.Fn.id

  let to_string = Base.Fn.id
end

module Ip = struct
  type t = string [@@deriving show]

  let of_string = Base.Fn.id

  let to_string = Base.Fn.id
end
