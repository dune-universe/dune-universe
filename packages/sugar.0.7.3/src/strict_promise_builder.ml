open S.Params

(**
  This module is similar to {{!Sugar__Promise_builder}Sugar.Promise}.
  It lets you create a result monad on top of an arbitrary monad.

  The main difference is that the functions in this module were created to
  recognize unexpected exceptions, and require you to provide some mecanism to
  recover from that. This is done with the signatures for {{!Sugar.S.Params.Strict_error}
   strict error} and {{!Sugar.S.Params.Strict_monad} strict monad}.


  An example:
  {[
  module MyError = struct
    type t = A | B | Unexpected of exn

    let panic e = Unexpected e
  end

  module MyMonad = struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let (>>=) = Lwt.(>>=)
    let catch = Lwt.catch
  end

  module MyResult = Sugar.Strict.Promise.Make (MyError) (MyMonad)
  ]}

  Notice that the signature for the required strict monad is the same as the Lwt
  library. That means, you can just plug it in:
  {[
    module MyResult = Sugar.Strict.Promise.Make (MyError) (Lwt)
  ]}
*)

(**
  A parametric module that implements the monadic interface for values.
  The complete documentation can be found in {!Sugar.S.Promise}.
*)
module Make (UserError:Strict_error) (UserMonad:Strict_monad) : S.Strict_promise
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

  let resolve r =
    UserMonad.catch
      ( fun () -> r )
      ( fun e -> throw (UserError.panic e) )

  let bind r f =
    resolve r
    >>= function
    | Error e -> throw e
    | Ok v -> f v

  let bind_unless r f =
    resolve r
    >>= function
    | Error e -> f e
    | Ok v -> return v

  let map r f =
    resolve r
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
    resolve r
    >>= function
    | Ok v -> UserMonad.return v
    | Error _ -> invalid_arg "Could not unwrap value"

  let unwrap_or f r =
    resolve r
    >>= function
    | Ok v -> UserMonad.return v
    | Error e -> f e

  let expect r msg =
    resolve r
    >>= function
    | Ok v -> UserMonad.return v
    | Error _ -> invalid_arg msg

  let (>>=) = bind

  module NoExceptions = Promise_builder.Make (UserError) (UserMonad)
end
