open Core
open Async_kernel
open Async_unix

let default_delivery_unit = Byte_units.create `Megabytes 2.

module File = struct
  type t =
    { path          : string
    ; fd            : Fd.t
    ; raw_fd        : Core.Unix.File_descr.t
    ; bytes_sent    : int
    ; bytes_pending : int
    } [@@deriving fields]

  let with_file file ~f =
    Unix.stat file
    >>= fun stat ->
    Unix.with_file ~mode:[ `Rdonly ] file ~f:(fun fd ->
      let t =
        { path          = file
        ; fd
        ; raw_fd        = Fd.file_descr_exn fd
        ; bytes_sent    = 0
        ; bytes_pending = Int64.to_int_exn (Unix.Stats.size stat)
        }
      in
      f t)
  ;;

  let update t ~bytes_sent_now =
    { t with
      bytes_sent    = t.bytes_sent    + bytes_sent_now
    ; bytes_pending = t.bytes_pending - bytes_sent_now
    }
  ;;

  let sendfile =
    Or_error.ok_exn Linux_ext.sendfile
  ;;

  let sendfile t ~socket_fd ~delivery_unit =
    let delivery_unit = Byte_units.bytes delivery_unit |> Float.to_int in
    if Int.(=) 0 t.bytes_pending
    then (Ok `Fully_sent)
    else begin
      try
        let bytes_sent_now =
          sendfile
            ~pos:t.bytes_sent
            ~len:(Int.min t.bytes_pending delivery_unit)
            ~fd:t.raw_fd
            socket_fd
        in
        if bytes_sent_now < 0
        then (
          Error (Error.create_s
                   [%message "Negative return value from [sendfile]"
                               ~return_value:(bytes_sent_now : int)]))
        else (Ok (`Sent (bytes_sent_now, (update t ~bytes_sent_now ))))
      with
      | exn -> Error (Error.of_exn exn)
    end
  ;;
end

module Limiter = struct
  module Limiter = Limiter.Token_bucket

  type t = (bytes_sent:int -> unit Deferred.t)

  let create ~rate_per_sec =
    let rate = Byte_units.bytes rate_per_sec in
    let limiter =
      Limiter.create_exn
        ~burst_size:(Float.to_int rate) ~sustained_rate_per_sec:rate
        ~continue_on_error:true
        ()
    in
    fun ~bytes_sent ->
      let sent = Ivar.create () in
      Limiter.enqueue_exn limiter ~allow_immediate_run:true
        bytes_sent
        (Ivar.fill sent)
        ();
      Ivar.read sent
  ;;

  let no_pushback ~bytes_sent:_ = Deferred.unit
end


let optimization_to_achieve_the_limiter_limits deferred f =
  match Deferred.peek deferred with
  | None   -> deferred >>= f
  | Some v -> f v
;;

let (>>==) =
  optimization_to_achieve_the_limiter_limits
;;

let error_socket_fd_closed = Error.of_string "Socket fd is closed."

let failed_to_send ~file ~error =
  Error
    (Error.create_s
       [%message "Failed to fully send file"
                   ~file:(File.path file : string)
                   ~bytes_sent:(File.bytes_sent file : int)
                   ~bytes_pending:(File.bytes_pending file : int)
                   (error : Error.t)
       ])
;;

let feed_file ~file ~socket_fd ~delivery_unit ~limiter =
  if Fd.is_closed socket_fd
  then (return (failed_to_send ~file ~error:error_socket_fd_closed))
  else begin
    let raw_client_fd = Fd.file_descr_exn socket_fd in
    let rec loop file =
      match File.sendfile ~socket_fd:raw_client_fd file ~delivery_unit with
      | Error error                   -> return (failed_to_send ~file ~error)
      | Ok `Fully_sent                -> Deferred.Or_error.ok_unit
      | Ok (`Sent (bytes_sent, file)) ->
        let ready_to_send = limiter ~bytes_sent in
        let ready_to_write = Fd.ready_to socket_fd `Write in
        ready_to_send
        >>== fun () ->
        ready_to_write
        >>== function
        | `Ready              -> loop file
        | (`Closed | `Bad_fd) ->
          return (failed_to_send ~file ~error:error_socket_fd_closed)
    in
    loop file
  end
;;

let sendfile
      ?(limiter = Limiter.no_pushback)
      ?(delivery_unit = default_delivery_unit)
      ~socket_fd
      ~file
      () =
  File.with_file file ~f:(fun file ->
    feed_file ~file ~socket_fd ~delivery_unit ~limiter)
;;
