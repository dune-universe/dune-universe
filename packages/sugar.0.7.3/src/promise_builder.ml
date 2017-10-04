open S.Params

(**
  How to create a {{!Sugar.S.Promise}promise-based} result monad:

  {[
  module MyError = struct
    type t = A | B | C
  end

  module MyMonad = struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let (>>=) = Lwt.(>>=)
  end

  module MyResult = Sugar.Promise.Make (MyError) (MyMonad)
  ]}

  Notice that for most cases, you can just plug the module
  directly from a monadic library, like:

  {[
    module MyResult = Sugar.Promise.Make (MyError) (Lwt)
    module MyResult2 = Sugar.Promise.Make (MyError) (Async.Std.Deferred)
  ]}
*)

(**
  A parametric module that implements the monadic interface for values.
  The complete documentation can be found in {!Sugar.S.Promise}.
*)
module Make (UserError:Error) (UserMonad:Monad) : S.Promise
  with
    type error := UserError.t
    and type 'a monad := 'a UserMonad.t
    and type 'a value = ('a, UserError.t) Result.result
    and type 'a result = ('a, UserError.t) Result.result UserMonad.t
=
struct
  include UserError

  type 'a monad = 'a UserMonad.t
  type 'a value = ('a, UserError.t) Result.result
  type 'a result = 'a value monad

  open UserMonad
  open Result

  let return v = UserMonad.return (Ok v)
  let throw e = UserMonad.return (Error e)

  let bind r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> f v

  let bind_unless r f =
    r
    >>= function
    | Error e -> f e
    | Ok v -> return v

  let map r f =
    r
    >>= function
    | Error e -> throw e
    | Ok v -> return (f v)

  module Infix = struct
    let (>>=) = bind

    let (>>|) = map

    let (>>) x y = bind x (fun () -> Lazy.force y)

    let (>>>) x y = bind x (fun _ -> Lazy.force y)

    let (>>>=) = UserMonad.(>>=)

    let (>---------) = bind_unless

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f
  end

  let unwrap r =
    r
    >>= function
    | Ok v -> UserMonad.return v
    | Error _ -> invalid_arg "Could not unwrap value"

  let unwrap_or f r =
    r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> f e

  let expect r msg =
    r
    >>= function
    | Ok v -> UserMonad.return v
    | Error _ -> invalid_arg msg


  let (>>=) = bind
end
