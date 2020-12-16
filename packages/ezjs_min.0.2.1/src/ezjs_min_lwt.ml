include Ezjs_min

module Promise = struct
  include Promise

  let ( >>= ) = Lwt.( >>= )
  let ( >|= ) = Lwt.( >|= )

  let return = Lwt.return

  let async = Lwt.async

  let return_unit = Lwt.return_unit

  let (>>=?) v f =
    v >>= function
    | Error _ as err -> Lwt.return err
    | Ok v -> f v

  let (>>|?) v f = v >>=? fun v -> Lwt.return (Ok (f v))


  (* promises *)

  let to_lwt (p : 'a promise t) =
    let waiter, notifier = Lwt.wait () in
    rthen p (Lwt.wakeup notifier);
    waiter

  let to_lwt_opt cb (p : 'a promise t) =
    to_lwt p >>= function
    | Error e -> return (Error e)
    | Ok x -> match cb with
      | None -> Lwt.return_ok None
      | Some cb -> Lwt.return_ok @@ Some (cb x)

  let to_lwt_tr tr (p : 'a promise t) =
    to_lwt p >>|? tr

  let to_lwt_exn (p : 'a promise t) =
    let waiter, notifier = Lwt.wait () in
    jthen p (Lwt.wakeup notifier);
    waiter

  let to_lwt_exn_opt cb (p : 'a promise t) =
    to_lwt_exn p >>= fun x ->
    match cb with None -> return None | Some cb -> return (Some (cb x))

  let to_lwt_exn_tr tr (p : 'a promise t) =
    to_lwt_exn p >|= tr

  (* callbacks *)

  let to_lwt_cb0 f =
    let waiter, notifier = Lwt.wait () in
    f (Lwt.wakeup notifier) ;
    waiter

  let to_lwt_cb f =
    let waiter, notifier = Lwt.wait () in
    f (wrap_callback (Lwt.wakeup notifier)) ;
    waiter

  let to_lwt_cb_tr tr f =
    let waiter, notifier = Lwt.wait () in
    f (wrap_callback (fun x -> Lwt.wakeup notifier (tr x))) ;
    waiter

  let to_lwt_cb_opt callback f =
    match callback with
    | Some callback ->
      let waiter, notifier = Lwt.wait () in
      f (def (wrap_callback (Lwt.wakeup notifier))) ;
      waiter >>= fun x -> return (Some (callback x))
    | None ->
      f undefined ; Lwt.return_none

  let promise_lwt res =
    let f resolve _reject =
      async (fun () -> res >>= fun value -> resolve value ; return_unit) in
    promise f

  let promise_lwt_res res =
    let f resolve reject =
      async (fun () -> res >>= function
        | Ok value -> resolve value ; return_unit
        | Error reason -> reject reason ; return_unit) in
    promise f
end
