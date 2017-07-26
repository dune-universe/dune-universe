module Mutex = struct
  include Mutex
  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock;
    let r = begin try f () with exn -> Mutex.unlock lock; raise exn end; in
    Mutex.unlock lock;
    r
end

type 'a t = {
  data: 'a option ref;
  m: Mutex.t;
  c: Condition.t;
}

let create_empty () =
  {
    data = ref None;
    m = Mutex.create ();
    c = Condition.create ();
  }

let create x =
  {
    data = ref (Some x);
    m = Mutex.create ();
    c = Condition.create ();
  }

let take mvar =
  let rec check () =
    match !(mvar.data) with
    | None ->
      Condition.wait mvar.c mvar.m;
      check ()
    | Some x ->
      mvar.data := None;
      Condition.signal mvar.c;
      x
    in
  Mutex.execute mvar.m (fun () -> check ())

let try_take mvar =
  Mutex.execute mvar.m (fun () ->
    match !(mvar.data) with
    | None -> None
    | Some x ->
      mvar.data := None;
      Condition.signal mvar.c;
      Some x)

let put mvar x =
  let rec check () =
    match !(mvar.data) with
    | None ->
      mvar.data := (Some x);
      Condition.signal mvar.c
    | Some _ ->
      Condition.wait mvar.c mvar.m;
      check ()
  in
  Mutex.execute mvar.m (fun () -> check ())

let try_put mvar x =
  Mutex.execute mvar.m (fun () ->
    match !(mvar.data) with
    | Some _ ->
      false
    | None ->
      mvar.data := (Some x);
      Condition.signal mvar.c;
      true)

let is_empty mvar =
  Mutex.execute mvar.m (fun () ->
    match !(mvar.data) with
    | Some _ -> false
    | None -> true)

let swap mvar x =
  let rec check () =
    match !(mvar.data) with
    | None ->
      Condition.wait mvar.c mvar.m;
      check ()
    | Some y ->
      mvar.data := (Some x);
      Condition.signal mvar.c;
      y
  in
  Mutex.execute mvar.m (fun () -> check ())

let modify mvar f =
  let rec check () =
    match !(mvar.data) with
    | None ->
      Condition.wait mvar.c mvar.m;
      check ()
    | Some x ->
      mvar.data := (Some (f x));
      Condition.signal mvar.c
  in
  Mutex.execute mvar.m (fun () -> check ())
