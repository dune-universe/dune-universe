let re_group_get_opt group num =
  match Re.Group.get group num with s -> Some s | exception Not_found -> None

module Let_syntax = struct
  module Cmdliner = struct
    open Cmdliner.Term

    let ( let+ ) t f = const f $ t

    let ( and+ ) a b = const (fun x y -> (x, y)) $ a $ b
  end

  module Option = struct
    let ( let+ ) x f = Option.map f x

    let ( let* ) = Option.bind

    let ( and+ ) xo yo =
      let* x = xo in
      let+ y = yo in
      (x, y)
  end

  module Result = struct
    let ( let+ ) x f = Result.map f x

    let ( let* ) = Result.bind
  end
end
