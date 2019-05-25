module Ip = struct
  type t = string [@@deriving show]

  let of_string = Base.Fn.id

  let to_string = Base.Fn.id
end

module Language = struct
  type t = string [@@deriving show]

  let of_string = Base.Fn.id

  let to_string = Base.Fn.id
end

module Path = struct
  type t = string [@@deriving show]

  let of_string = Base.Fn.id

  let to_string = Base.Fn.id
end

module Version_number = struct
  type t = {
    major : int;
    minor : int
  }
  [@@deriving show]

  let of_major_and_minor (major, minor) = {major; minor}

  let to_major_and_minor {major; minor} = major, minor
end
