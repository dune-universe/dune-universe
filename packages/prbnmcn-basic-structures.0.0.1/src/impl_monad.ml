type 'a cont = { fa : 'b. ('a -> 'b) -> 'b } [@@unboxed]

module Identity : Intf_monad.Monad with type 'a t = 'a and type 'a res = 'a =
struct
  type 'a t = 'a

  type 'a res = 'a

  let return x = x [@@inline]

  let bind m f = f m [@@inline]

  let map m f = f m [@@inline]

  let run x = x [@@inline]

  module Infix = struct
    let ( >>= ) = bind

    let ( >|= ) = map

    let ( let* ) = bind

    let return = return
  end
end

module Cps : Intf_monad.Monad with type 'a t = 'a cont and type 'a res = 'a =
struct
  type 'a t = 'a cont

  type 'a res = 'a

  let return x = { fa = (fun k -> k x) } [@@inline]

  let bind m f = { fa = (fun k -> m.fa (fun m -> (f m).fa k)) } [@@inline]

  let map m f = { fa = (fun k -> m.fa (fun m -> k (f m))) } [@@inline]

  let run x = x.fa (fun x -> x) [@@inline]

  module Infix = struct
    let ( >>= ) = bind

    let ( >|= ) = map

    let ( let* ) = bind

    let return = return
  end
end

module Codegen_cps
    (Repr : Intf_lang.Empty)
    (M : Intf_lang.Sequencing with type 'a m = 'a Repr.m) : sig
  type 'a cont = { fa : 'b. ('a -> 'b Repr.m) -> 'b Repr.m } [@@unboxed]

  include
    Intf_monad.Codegen_monad with type 'a m = 'a Repr.m and type 'a t = 'a cont
end = struct
  type 'a m = 'a Repr.m

  type 'a cont = { fa : 'b. ('a -> 'b Repr.m) -> 'b Repr.m } [@@unboxed]

  type 'a t = 'a cont

  let return x = { fa = (fun k -> k x) } [@@inline]

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
   fun m f -> { fa = (fun k -> m.fa (fun m -> (f m).fa k)) }
   [@@inline]

  let run x = x.fa (fun x -> x) [@@inline]

  module Infix = struct
    let ( >>= ) = bind

    let ( let* ) = bind

    let ( let*! ) : 'a m -> ('a m -> 'b t) -> 'b t =
     fun m f ->
      bind
        { fa =
            (fun k ->
              M.(
                let* x = m in
                k x))
        }
        f
     [@@inline]

    let ( >> ) (m : unit m t) f =
      let* u = m in
      let*! _u = u in
      f ()

    let ( >>! ) (m : unit m) f =
      bind { fa = (fun k -> M.(seq m k)) } f
      [@@inline]
  end

  open Infix

  let lift1 f x =
    let* x = x in
    return (f x)

  let lift2 f x y =
    let* x = x in
    let* y = y in
    return (f x y)

  let lift3 f x y z =
    let* x = x in
    let* y = y in
    let* z = z in
    return (f x y z)
end

module Codegen_identity
    (Repr : Intf_lang.Empty with type 'a m = 'a)
    (M : Intf_lang.Sequencing with type 'a m = 'a) :
  Intf_monad.Codegen_monad with type 'a m = 'a and type 'a t = 'a = struct
  type 'a m = 'a

  type 'a t = 'a

  let return x = x [@@inline]

  let bind : 'a t -> ('a -> 'b t) -> 'b t = fun m f -> f m [@@inline]

  let run x = x [@@inline]

  module Infix = struct
    let ( >>= ) = bind

    let ( >> ) (m : unit m t) f = bind m @@ fun _u -> f ()

    let ( let* ) = bind

    let ( let*! ) = bind

    let ( >>! ) (m : unit m) f = f m
  end

  open Infix

  let lift1 f x =
    let* x = x in
    return (f x)

  let lift2 f x y =
    let* x = x in
    let* y = y in
    return (f x y)

  let lift3 f x y z =
    let* x = x in
    let* y = y in
    let* z = z in
    return (f x y z)
end
