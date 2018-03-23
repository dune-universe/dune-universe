open Core
open Async

type ('ok, 'err) t = ('ok, 'err) Deferred.Result.t

include Applicative.Make2(struct
  type nonrec ('ok, 'err) t = ('ok, 'err) t
  let return = Deferred.Result.return
  let apply ft xt =
    Deferred.choose
      [ Deferred.choice ft (fun f -> `F f)
      ; Deferred.choice xt (fun x -> `X x)
      ]
    >>= function
    | `F (Error _ as err)
    | `X (Error _ as err) -> Deferred.return err
    | `F (Ok f) -> Deferred.Result.map xt ~f:(fun x -> f x)
    | `X (Ok x) -> Deferred.Result.map ft ~f:(fun f -> f x)
  let map = `Custom Deferred.Result.map
end)

let all_unit ts = Deferred.create (fun r ->
  Deferred.List.iter ~how:`Parallel ts ~f:(fun t ->
    t >>| function
    | Error _ as error -> Ivar.fill_if_empty r error
    | Ok () -> ())
  >>> fun () ->
  Ivar.fill_if_empty r (Ok ()));
