module Lwt_scheduler = Dkim.Sigs.Make (Lwt)

let ( <.> ) f g x = f (g x)

type 'a stream = unit -> 'a option Lwt.t

module Flow = struct
  type backend = Lwt_scheduler.t

  type flow = {
    linger : Bytes.t;
    mutable pos : int;
    stream : (string * int * int) stream;
  }

  let chunk = 0x1000 (* XXX(dinosaure): see [extract_dkim]. *)

  let of_stream stream = { linger = Bytes.create chunk; pos = 0; stream }

  let rec input ({ linger; pos; stream } as flow) tmp off len =
    let open Lwt.Infix in
    if pos > 0
    then (
      let len = min len pos in
      Bytes.blit tmp off linger 0 len ;
      if len < pos then Bytes.unsafe_blit linger len linger 0 (pos - len) ;
      (* compress *)
      flow.pos <- pos - len ;
      Lwt_scheduler.inj (Lwt.return len))
    else
      let fiber =
        stream () >>= function
        | None -> Lwt.return 0 (* end-of-input *)
        | Some (_, _, 0) ->
            Lwt_scheduler.prj (input flow tmp off len) (* redo *)
        | Some (str, str_off, str_len) ->
            let max = min str_len len in
            Bytes.blit_string str str_off tmp off max ;
            if str_len > max
            then (
              Bytes.blit_string str max linger 0 (str_len - max) ;
              flow.pos <- str_len - max) ;
            Lwt.return max in
      Lwt_scheduler.inj fiber
end

let bind x f =
  let open Lwt.Infix in
  Lwt_scheduler.inj (Lwt_scheduler.prj x >>= (Lwt_scheduler.prj <.> f))

let return x = Lwt_scheduler.inj (Lwt.return x)

let ( >>= ) x f = bind x f

let ( >>| ) x f = x >>= (return <.> f)

let ( >>? ) x f =
  x >>= function
  | Ok x -> f x
  | Error err -> Lwt_scheduler.inj (Lwt.return_error err)

let lwt = { Dkim.Sigs.bind; return }

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4V6) =
struct
  module DNS = struct
    include Dns_client_mirage.Make (R) (T) (C) (S)

    type backend = Lwt_scheduler.t

    let getaddrinfo dns `TXT domain_name =
      let open Lwt.Infix in
      getaddrinfo dns Dns.Rr_map.Txt domain_name >>= function
      | Ok (_ttl, txtset) -> Lwt.return_ok (Dns.Rr_map.Txt_set.elements txtset)
      | Error err -> Lwt.return_error err

    let getaddrinfo dns `TXT domain_name =
      Lwt_scheduler.inj (getaddrinfo dns `TXT domain_name)
  end

  let fold_left_s ~f a l =
    let rec go a = function
      | [] -> return a
      | x :: r -> f a x >>= fun a -> go a r in
    go a l

  let verify ?newline ?size ?nameserver ?timeout stream stack =
    let flow = Flow.of_stream stream in
    let dns = DNS.create ?size ?nameserver ?timeout stack in
    Dkim.extract_dkim ?newline flow lwt (module Flow) >>? fun extracted ->
    Dkim.extract_body ?newline flow lwt
      (module Flow)
      ~prelude:extracted.Dkim.prelude
    >>= fun body ->
    let f (valid, invalid) (dkim_field_name, dkim_field_value, m) =
      let fiber =
        Dkim.post_process_dkim m |> return >>? fun dkim ->
        Dkim.extract_server dns lwt (module DNS) dkim >>? fun n ->
        Dkim.post_process_server n |> return >>? fun server ->
        return (Ok (dkim, server)) in
      fiber >>= function
      | Error _ -> return (valid, invalid)
      | Ok (dkim, server) -> (
          Dkim.verify extracted.fields
            (dkim_field_name, dkim_field_value)
            dkim server body
          |> return
          >>= function
          | true -> return (dkim :: valid, invalid)
          | false -> return (valid, dkim :: invalid)) in
    fold_left_s ~f ([], []) extracted.dkim_fields >>| Rresult.R.ok

  let verify ?newline ?size ?nameserver ?timeout stream stack =
    Lwt_scheduler.prj (verify ?newline ?size ?nameserver ?timeout stream stack)
end

module Flow_with_stream = struct
  type backend = Lwt_scheduler.t

  type flow = {
    linger : Bytes.t;
    mutable pos : int;
    pusher : (string * int * int) option -> unit;
    stream : (string * int * int) stream;
  }

  let chunk = 0x1000 (* XXX(dinosaure): see [extract_dkim]. *)

  let of_stream stream =
    let stream', pusher = Lwt_stream.create () in
    ({ linger = Bytes.create chunk; pos = 0; pusher; stream }, stream')

  let rec input ({ linger; pos; pusher; stream } as flow) tmp off len =
    let open Lwt.Infix in
    if pos > 0
    then (
      let len = min len pos in
      Bytes.blit tmp off linger 0 len ;
      if len < pos then Bytes.unsafe_blit linger len linger 0 (pos - len) ;
      (* compress *)
      flow.pos <- pos - len ;
      Lwt_scheduler.inj (Lwt.return len))
    else
      let fiber =
        stream () >>= function
        | None ->
            pusher None ;
            Lwt.return 0 (* end-of-input *)
        | Some (_, _, 0) ->
            Lwt_scheduler.prj (input flow tmp off len) (* redo *)
        | Some (str, str_off, str_len) as v ->
            pusher v ;
            let max = min str_len len in
            Bytes.blit_string str str_off tmp off max ;
            if str_len > max
            then (
              Bytes.blit_string str max linger 0 (str_len - max) ;
              flow.pos <- str_len - max) ;
            Lwt.return max in
      Lwt_scheduler.inj fiber
end

let sign ~key ?(newline = Dkim.LF) stream dkim =
  let open Lwt.Infix in
  let flow, mail_stream = Flow_with_stream.of_stream stream in
  Lwt_scheduler.prj
    (Dkim.sign ~key ~newline flow lwt (module Flow_with_stream) dkim)
  >>= fun dkim ->
  let new_line = match newline with Dkim.LF -> "\n" | Dkim.CRLF -> "\r\n" in
  let stream = Prettym.to_stream ~new_line Dkim.Encoder.as_field dkim in
  let dkim_stream =
    Lwt_stream.from (fun () ->
        match stream () with
        | Some str -> Lwt.return_some (str, 0, String.length str)
        | None -> Lwt.return_none) in
  let stream = Lwt_stream.append dkim_stream mail_stream in
  Lwt.return (dkim, fun () -> Lwt_stream.get stream)
