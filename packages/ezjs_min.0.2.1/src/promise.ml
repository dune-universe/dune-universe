open Js

class type ['a, 'b] promise0 =
  object
    method then_ : ('a -> unit) callback -> ('a, 'b) promise0 t meth
    method catch : ('b -> unit) callback -> ('a, 'b) promise0 t meth
  end

type 'a promise = ('a, error t) promise0

type ('a, 'b) promise_cs =
  ((('a -> unit) -> ('b -> unit) -> unit) callback -> ('a, 'b) promise0 t)
  constr

let promise f =
  let cs : ('a, error t) promise_cs = Unsafe.global##._Promise in
  new%js cs (wrap_callback f)

let jthen0 ?error (prom : ('a, 'b t) promise0 t) f =
  let catch_exn exn =
    match error with
    | None -> ()
    | Some ef -> catch_exn (fun x -> ef (Unsafe.coerce x)) exn in
  let p = prom##then_ (wrap_callback (fun x -> try f x with exn -> catch_exn exn)) in
  match error with
    | None -> ()
    | Some error -> ignore (p##catch (wrap_callback error))

let jthen ?error (prom : 'a promise t) f =
  let catch_exn exn = match error with
    | None -> ()
    | Some ef -> catch_exn ef exn in
  let p = prom##then_ (wrap_callback (fun x -> try f x with exn -> catch_exn exn)) in
  match error with
    | None -> ()
    | Some error -> ignore (p##catch (wrap_callback error))

let jthen0_opt prom = function None -> ignore prom | Some f -> jthen0 prom f
let jthen_opt prom = function None -> ignore prom | Some f -> jthen prom f

let rthen prom f =
  jthen ~error:(fun e -> f @@ Error e) prom (fun x -> f (Ok x))
