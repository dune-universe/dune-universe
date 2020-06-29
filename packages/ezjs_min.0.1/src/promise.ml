open Js

class type ['a, 'b] promise0 =
  object
    method _then : ('a -> unit) callback -> ('a, 'b) promise0 t meth

    method catch : ('b -> unit) callback -> ('a, 'b) promise0 t meth
  end

class type any = object end

type 'a promise = ('a, any t) promise0

type ('a, 'b) promise_cs =
  ((('a -> unit) -> ('b -> unit) -> unit) callback -> ('a, 'b) promise0 t)
  constr

let promise f =
  let cs : ('a, 'b) promise_cs = Unsafe.global##._Promise in
  new%js cs (wrap_callback f)

let jthen ?error (prom : ('a, 'b) promise0 t) f =
  let p = prom##_then (wrap_callback f) in
  match error with
  | None ->
      ()
  | Some error ->
      ignore (p##catch (wrap_callback error))

let jthen_opt prom = function None -> ignore prom | Some f -> jthen prom f
