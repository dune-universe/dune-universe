module type IO = Io.IO

module type Intf = sig
  module IO : IO

  val return : 'a -> ('a, 'b) result IO.t
  val fail : 'b -> ('a, 'b) result IO.t
  val (>>=?)
    :  ('a, 'b) result IO.t
    -> ('a -> ('c, 'b) result IO.t)
    -> ('c, 'b) result IO.t
end

module Make (IO : IO) : Intf with module IO := IO = struct
  let return v = IO.return (Ok v)
  let fail err = IO.return (Error err)

  let (>>=?) a f =
    let open IO in
    a >>= fun a ->
    match a with
    | Ok a -> f a
    | Error err -> IO.return (Error err)
end

