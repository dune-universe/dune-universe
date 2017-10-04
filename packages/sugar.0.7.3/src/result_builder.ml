open S.Params

(**
  How to create a result monad:

  {[
  module MyError = struct
    type t = A | B | C
  end

  module MyResult = Sugar.Result.Make (MyError)
  ]}

  The generated module will have the signature of {!Sugar.S.Result}
*)


(**
  A parametric module that implements the blocking interface.

  The complete documentation can be found in {!Types.Result}.
*)
module Make (UserError:Error) : S.Result
  with type error = UserError.t =
struct
  type 'a result = ('a, UserError.t) Result.result
  type error = UserError.t

  let return v = Result.Ok v

  let throw e = Result.Error e

  let bind r f =
    match r with
      | Result.Error e -> Result.Error e
      | Result.Ok v -> f v

  let bind_unless r f =
    match r with
    | Result.Error e -> f e
    | Result.Ok v -> Result.Ok v

  let map r f =
    match r with
    | Result.Error e -> Result.Error e
    | Result.Ok v -> Result.Ok (f v)


  module Infix = struct
    let (>>=) = bind
    let (>>|) = map
    let (>>>) x y = bind x (fun _ -> Lazy.force y)
    let (>>) x y = bind x (fun () -> Lazy.force y)
    let (>---------) = bind_unless

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f
  end

  let unwrap = function
    | Result.Ok r -> r
    | Result.Error _ -> invalid_arg "Could not unwrap value from result"

  let unwrap_or f r =
    match r with
    | Result.Ok r -> r
    | Result.Error e -> f e

  let expect r msg =
    match r with
    | Result.Ok r -> r
    | Result.Error _ -> invalid_arg msg

  let (>>=) = bind

  module Monad : Monad
    with type 'a t = 'a result =
  struct
    type 'a t = 'a result

    let return = return
    let (>>=) = bind
  end

  module For(M: Monad) = struct
    include Promise_builder.Make (UserError) (M)
  end
end
