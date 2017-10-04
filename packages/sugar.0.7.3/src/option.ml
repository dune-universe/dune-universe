open S.Params

(**
  An implementation of {{!Sugar.S.Result_partials}  Result } interface
  for the option type.

  This is probably the easiest way to start using Sugar, as there is no need to
  use describe errors. Still, because this module follows the
  same interface, when you need to start refactoring to more complex, monadic
  results you get to keep the same clean interface, making be transition
  straightfoward.

  Usage example:
  {[
    open Sugar.Option

    let do_something (): string result =
      if true then
        Some "you could use any option type"
      else
        throw ()

    let run (): string result =
      do_something ()
      >----------
      ( fun () ->
        return "recovered"
      )
  ]}

  In case you are wondering, the evaluation of [run ()] in the example above,
  will produce: [string option = Some "you could use any option type"].

*)

type error = unit
type 'a result = 'a option

let return v = Some v
let throw () = None

let bind r f =
  match r with
  | None -> None
  | Some v -> f v

let bind_unless r f =
  match r with
  | None -> f ()
  | Some v -> Some v

let map r f =
  match r with
  | None -> None
  | Some v -> Some (f v)

let (>>=) = bind
let (>>) x y = bind x (fun () -> y)

module Infix = struct
  let (>---------) = bind_unless
  let (>>|) = map
  let (>>) x y = bind x (fun () -> Lazy.force y)
  let (>>>) x y = bind x (fun _ -> Lazy.force y)

  let (<*>) f x =
    f
    >>= fun f' ->
    x
    >>= fun x' ->
    return (f' x')

  let (<$>) f x = map x f
end

let wrap f =
  try Some (f ()) with
  | _ -> None

let unwrap = function
  | Some r -> r
  | None -> invalid_arg "Could not unwrap value from result"

let unwrap_or r f =
  match r with
  | Some r -> r
  | None -> f ()

let expect r msg =
  match r with
  | Some r -> r
  | None -> invalid_arg msg

module Monad : Monad
   with type 'a t = 'a option =
struct
  type 'a t = 'a option
  let return = return
  let (>>=) = bind
end
