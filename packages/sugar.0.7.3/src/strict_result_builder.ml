open S.Params

(**
  How to create a {{!Sugar.S.Strict_result} strict result monad}:

  {[
  module MyError = struct
    type t = A | B | Unexpected of exn

    let panic e = Unexpected e
  end

  module MyResult = Sugar.Result.Make (MyError)
  ]}

  The generated module will have the signature of {!Sugar.S.Strict_result}
*)


(**
  A parametric module that implements the blocking interface.
*)
module Make (UserError:Strict_error) : S.Strict_result
  with type error = UserError.t =
struct
  type 'a result = ('a, UserError.t) Result.result
  type error = UserError.t

  let return v = Result.Ok v

  let throw e = Result.Error e

  let bind r f =
    match r with
      | Result.Error e -> Result.Error e
      | Result.Ok v ->
        ( try f v with
          e -> Result.Error (UserError.panic e)
        )

  let bind_unless r f =
    match r with
    | Result.Ok v -> Result.Ok v
    | Result.Error e ->
      ( try f e with
        | e -> Result.Error (UserError.panic e)
      )

  let map r f =
    match r with
    | Result.Error e -> Result.Error e
    | Result.Ok v ->
      ( try Result.Ok (f v) with
        | e -> Result.Error (UserError.panic e)
      )


  module Infix = struct
    let (>>=) = bind
    let (>>|) = map
    let (>>>) x y = bind x (fun _ -> Lazy.force y)
    let (>---------) = bind_unless

    let (<*>) f x =
      f
      >>= fun f' ->
      x
      >>= fun x' ->
      return (f' x')

    let (<$>) f x = map x f

    let (>>) x y = bind x (fun () -> Lazy.force y)
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
    with type 'a t := 'a result =
  struct
    type 'a t = 'a result

    let return = return
    let (>>=) = bind
  end

  module For (Strict_monad: Strict_monad) = struct
    include Strict_promise_builder.Make (UserError) (Strict_monad)
  end

  module NoExceptions = Result_builder.Make (UserError)
end
