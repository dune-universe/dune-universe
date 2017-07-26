open Core
open Import

let writers = ref []

let bytes_to_write () =
  List.fold !writers ~init:0 ~f:(fun ac writer ->
    ac + Writer.bytes_to_write writer)
;;

type t =
  { writer         : Writer.t
  ; monitor        : Monitor.t
  ; mutable failed : bool
  }
[@@deriving sexp_of]

let monitor t = t.monitor

let write t s =
  if not t.failed then
    Writer.write t.writer s
;;

let write_sexp ?hum t sexp =
  if not t.failed then
    Writer.write_sexp ?hum t.writer sexp
;;

let write_bin_prot t sw_arg m =
  if not t.failed then
    Writer.write_bin_prot t.writer sw_arg m
;;

let write_substring t ss =
  if not t.failed then
    Writer.write_substring t.writer ss
;;

let write_bigsubstring t ss =
  if not t.failed then
    Writer.write_bigsubstring t.writer ss
;;

let write_bigstring t ?pos ?len bigstring =
  if not t.failed then
    Writer.write_bigstring t.writer ?pos ?len bigstring
;;

let schedule_bigstring t ss =
  if not t.failed then
    Writer.schedule_bigstring t.writer ss

let create ?(append = true) file =
  let monitor =
    Monitor.create ~info:(Info.create "Async.File_writer" file [%sexp_of: string]) ()
  in
  within' ~monitor (fun () ->
    Deferred.create
      (fun result ->
         let append_flag = if append then [`Append] else [] in
         Unix.openfile file ~mode:(append_flag @ [`Wronly; `Creat]) ~perm:0o644
         >>> fun fd ->
         let writer =
           Writer.create ~syscall:(`Periodic (sec 0.1)) fd
         in
         let t =
           { writer
           ; failed = false
           ; monitor
           }
         in
         Stream.iter (Monitor.detach_and_get_error_stream (Writer.monitor writer))
           ~f:(fun e ->
             if not t.failed then begin
               t.failed <- true;
               raise e;
             end);
         writers := writer :: !writers;
         Ivar.fill result t))
;;

let flushed t =
  if not t.failed
  then Deferred.ignore (Writer.flushed t.writer)
  else return ()
;;

let close t = flushed t >>= fun () -> Writer.close t.writer
