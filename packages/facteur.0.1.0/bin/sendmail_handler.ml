module Lwt_scheduler = Colombe.Sigs.Make(Lwt)

let ( <.> ) f g = fun x -> f (g x)

let lwt_bind x f =
  let open Lwt.Infix in
  let open Lwt_scheduler in
  inj (prj x >>= (prj <.> f))

let lwt =
  { Colombe.Sigs.bind= lwt_bind
  ; return= (fun x -> Lwt_scheduler.inj (Lwt.return x)) }

type flow =
  { ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel }

let rdwr =
  { Colombe.Sigs.rd= (fun { ic; _ } bytes off len ->
        let res = Lwt_io.read_into ic bytes off len in
        Lwt_scheduler.inj res)
  ; wr= (fun { oc; _ } bytes off len ->
        let res = Lwt_io.write_from_exactly oc (Bytes.unsafe_of_string bytes) off len in
        Lwt_scheduler.inj res) }

let run_with_starttls ?logger ~hostname ?port ~domain ~authenticator:auth ~tls ~from ~recipients mail =
  let port = match port with Some port -> port | None -> 465 in
  let tls = Tls.Config.client ~authenticator:tls () in
  let ctx = Colombe.State.make_ctx () in

  match Sendmail_tls.make_state ?logger ~domain ~from ~recipients auth mail tls with
  | Error (`Msg err) -> Lwt.return (`Error (false, err))
  | Ok state ->
    let state = Sendmail_tls.make state in

    let open Lwt.Infix in

    Lwt_unix.gethostbyname (Domain_name.to_string hostname) >>= fun res ->
    if Array.length res.Lwt_unix.h_addr_list = 0
    then Lwt.return (`Error (false, Fmt.strf "%a can not be resolved" Domain_name.pp hostname))
    else
      let socket = Lwt_unix.socket Lwt_unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket (Lwt_unix.ADDR_INET (res.Lwt_unix.h_addr_list.(0), port)) >>= fun () ->

      let closed = ref false in
      let close () = if !closed then Lwt.return () else ( closed := true ; Lwt_unix.close socket ) in
      let ic = Lwt_io.of_fd ~close ~mode:Lwt_io.Input socket in
      let oc = Lwt_io.of_fd ~close ~mode:Lwt_io.Output socket in

      Sendmail_tls.run lwt rdwr { ic; oc; } state ctx |> Lwt_scheduler.prj >>= function
      | Ok _ ->
        if not !closed
        then ( Lwt_unix.close socket >|= fun () -> `Ok () )
        else Lwt.return (`Ok ())
      | Error err ->
        Lwt.return (`Error (false, Fmt.strf "%a" Sendmail_tls.pp_error err))

let run ?logger ~hostname ?port ~domain ~authenticator:auth ~tls ~from ~recipients mail =
  let open Lwt.Infix in
  Sendmail_lwt.run ?logger ~hostname ?port ~domain ~authenticator:tls ~from ~recipients auth mail >>= function
  | Ok _ -> Lwt.return (`Ok ())
  | Error err -> Lwt.return (`Error (false, Fmt.strf "%a" Sendmail_lwt.pp_error err))
