open Core
open Import

module Stats = Unix.Stats

module Global_throttle : sig
  val enqueue : (unit -> 'a Deferred.t) -> 'a Deferred.t
end = struct
  let global = lazy (Throttle.create ~continue_on_error:true ~max_concurrent_jobs:50)
  let enqueue job = Throttle.enqueue (Lazy.force global) job
end

module Error = struct
  type t =
    | File_replaced
    | File_shrank
    | Read_failed   of exn
    | Stat_failed   of exn
  [@@deriving sexp_of]

  let to_string_hum t =
    match t with
    | File_replaced -> "file replaced"
    | File_shrank -> "file shrank"
    | Read_failed _ -> sprintf "read failed"
    | Stat_failed _ -> sprintf "stat failed"
  ;;
end

module Warning = struct
  type t =
    | Did_not_reach_eof_for               of Time.Span.t
    | Reached_eof
    | Delayed_due_to_null_reads_for       of Time.Span.t
    | No_longer_delayed_due_to_null_reads
  [@@deriving sexp_of]

  let to_string_hum t =
    match t with
    | Did_not_reach_eof_for how_long ->
      sprintf "did not reach EOF for %s" (Time.Span.to_short_string how_long)
    | Reached_eof -> "reached EOF"
    | Delayed_due_to_null_reads_for how_long ->
      sprintf "delayed due to null reads for %s" (Time.Span.to_short_string how_long)
    | No_longer_delayed_due_to_null_reads -> sprintf "no longer delayed due to null reads"
  ;;
end

module Update = struct
  type t =
    | Data    of string
    | Warning of string * Warning.t
    | Error   of string * Error.t
  [@@deriving sexp_of]

  let to_string_hum = function
    | Data d -> sprintf "data:%s" d
    | Warning (file, w) -> sprintf "warning: %s %s" file (Warning.to_string_hum w)
    | Error (file, e) -> sprintf "error: %s %s" file (Error.to_string_hum e)
  ;;
end

module Chunker : sig
  (** A chunker is used to convert the incoming strings of data into a sequence of
      updates.  One creates a chunker, and then repeatedly [feed]s it data. *)
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t
  val create : break_on_lines:bool -> t
  val feed : t -> string -> pos:int -> len:int -> Update.t Queue.t
end = struct
  type t =
    | Simple
    | By_line of Buffer.t sexp_opaque
  [@@deriving sexp_of]

  let newline = '\n'

  let invariant t =
    try
      match t with
      | Simple -> ()
      | By_line buffer ->
        for i = 0 to Buffer.length buffer - 1 do
          assert (Buffer.nth buffer i <> newline)
        done;
    with exn -> failwiths "invariant failed" (exn, t) [%sexp_of: exn * t]
  ;;

  let create ~break_on_lines =
    if break_on_lines then By_line (Buffer.create 1) else Simple
  ;;

  let feed t buf ~pos ~len =
    let result : Update.t Queue.t = Queue.create () in
    let enqueue data = Queue.enqueue result (Data data) in
    begin match t with
    | Simple -> enqueue (String.sub buf ~pos ~len)
    | By_line buffer ->
      for i = pos to pos + len - 1 do
        let c = buf.[i] in
        if c <> newline
        then Buffer.add_char buffer c
        else begin
          let current_line =
            if Buffer.length buffer = 0
            then ""
            else begin
              let n = Buffer.length buffer in
              (* Strip off terminating '\r', if present. *)
              let length = if Buffer.nth buffer (n - 1) = '\r' then n - 1 else n in
              Buffer.sub buffer 0 length
            end
          in
          enqueue current_line;
          Buffer.clear buffer;
        end
      done
    end;
    result;
  ;;
end

let%test_unit _ =
  let open Chunker in
  let buf =
    String.concat
      (List.init 10 ~f:(fun string_length ->
         String.init string_length ~f:(fun i ->
           if i = string_length - 1
           then '\n'
           else Char.of_int_exn (i + Char.to_int '0'))))
  in
  for bytes_at_a_time = 1 to 10 do
    let result = Queue.create () in
    let t = create ~break_on_lines:true in
    let rec loop pos =
      Chunker.invariant t;
      let len = min bytes_at_a_time (String.length buf - pos) in
      if len > 0 then begin
        Queue.blit_transfer ~src:(Chunker.feed t buf ~pos ~len) ~dst:result ();
        loop (pos + bytes_at_a_time);
      end
    in
    loop 0;
    let output =
      String.concat
        (List.map (Queue.to_list result) ~f:(function
           | Data d -> d ^ "\n"
           | _ -> assert false))
    in
    assert (buf = output);
  done
;;

type t =
  { file                                  : string
  ; read_delay                            : Time.Span.t
  ; retry_null_reads                      : bool
  ; chunker                               : Chunker.t
  ; pipe_w                                : Update.t Pipe.Writer.t
  ; ignore_inode_change                   : bool
  ; eof_latency_tolerance                 : Time.Span.t
  ; null_read_tolerance                   : Time.Span.t
  ; mutable read_buf                      : string
  ; mutable file_pos                      : int64
  ; mutable file_len                      : int64
  ; mutable most_recent_file_len_increase : Time.t
  ; mutable file_inode                    : int
  (* [need_to_report_eof] is used to ensure that we report [Reached_eof] exactly once each
     time we reach EOF. *)
  ; mutable need_to_report_eof            : bool
  }
[@@deriving fields, sexp_of]

let _invariant t =
  try
    Chunker.invariant t.chunker;
    assert Int64.(t.file_len >= zero);
    assert Int64.(t.file_pos >= zero);
    (* It is possible to have [t.file_pos > t.file_len], since we read as much as
       we can into [t.read_buf] and the file may have grown in between the time we stat
       and the time we read. *)
  with exn -> failwiths "invariant failed" (exn, t) [%sexp_of: exn * t]
;;

let need_to_read t = t.file_pos < t.file_len

let am_alive t = not (Pipe.is_closed t.pipe_w)

let write t updates =
  if am_alive t
  then Pipe.transfer_in t.pipe_w ~from:updates
  else Deferred.unit
;;

let warning t w = don't_wait_for (write t (Queue.singleton (Update.Warning (t.file, w))))

let error t e =
  (* We want to be sure that [t] will be closed when this function returns, so we don't
     wait on the pipe to flush before calling [Pipe.close]. *)
  don't_wait_for (write t (Queue.singleton (Update.Error (t.file, e))));
  Pipe.close t.pipe_w;
;;

(* [stat t] stats [t.file] and possibly updates [t.file_inode], [t.file_len], and
   [t.most_recent_file_len_increase]. *)
let stat t ~initial_call =
  try_with (fun () ->
    Global_throttle.enqueue (fun () ->
      In_thread.run (fun () ->
        (* We use [Unix.with_file t.file ~f:Unix.fstat], which does [open; fstat; close],
           rather than just [stat t.file] because [File_tail] is often used over NFS and
           this leverages open-to-close cache consistency to get a fresh answer from
           [stat] regardless of the metadata caching options on the underlying mount.  The
           downside is that this adds two more system calls per for every [stat], and in
           the case where the file has grown "wastes" the open file descriptor since we
           are just going to re-open it. *)
        let module Unix = Core.Unix in
        Unix.with_file t.file ~mode:[ O_RDONLY ] ~f:Unix.fstat)))
  >>| fun result ->
  let error e = error t e; Error () in
  match result with
  | Error exn -> error (Stat_failed exn)
  | Ok stats ->
    let new_inode = stats.st_ino in
    if not initial_call && not t.ignore_inode_change && new_inode <> t.file_inode
    then error File_replaced
    else begin
      t.file_inode <- new_inode;
      let new_len = stats.st_size in
      if new_len < t.file_len
      then error File_shrank
      else begin
        if new_len > t.file_len then begin
          t.file_len <- new_len;
          t.most_recent_file_len_increase <- Time.now ();
          t.need_to_report_eof <- true;
        end;
        Result.ok_unit
      end
    end
;;

let start_eof_latency_check t =
  Clock.every t.eof_latency_tolerance ~stop:(Pipe.closed t.pipe_w) (fun () ->
    if need_to_read t then begin
      let now = Time.now () in
      let eof_delay = Time.diff now t.most_recent_file_len_increase in
      if Time.Span.(eof_delay > t.eof_latency_tolerance) then
        warning t (Did_not_reach_eof_for eof_delay);
    end)
;;

(* [read_once t] reads into [t.read_buf] some bytes from the file starting at
   [t.file_pos], and returns the number of bytes read. *)
let read_once t =
  Global_throttle.enqueue (fun () ->
    In_thread.run (fun () ->
      let module Unix = Core.Unix in
      Unix.with_file t.file ~mode:[ O_RDONLY ] ~f:(fun fd ->
        ignore (Unix.lseek fd t.file_pos ~mode:SEEK_SET);
        Unix.read fd ~buf:t.read_buf)))
;;

(* [read t] repeatedly does [read_once] until the data read doesn't contain nulls, unless
   [not t.retry_null_reads], in which case it simply reads once.  It feeds the data read
   to [t.chunker] and returns the resulting updates. *)
let read t =
  assert (need_to_read t);
  Deferred.create (fun finished ->
    let started = Time.now () in
    let have_warned_about_null_delay = ref false in
    let rec loop () =
      if am_alive t then begin
        read_once t
        >>> fun len ->
        if len = 0 then begin
          (* [Unix.read] should only return 0 if we're at EOF.  But we're only calling
             [read] because [t.file_pos < t.file_len], where the [t.file_len] was set by
             the most recent [stat].  Since we're at EOF before reading up to
             [t.file_len], the file must have shrunk. *)
          error t File_shrank
        end
        else if t.retry_null_reads && String.contains t.read_buf ~len '\000'
        then begin
          let delayed_for = Time.(diff (now ()) started) in
          if Time.Span.(>=) delayed_for t.null_read_tolerance then
            warning t (Delayed_due_to_null_reads_for delayed_for);
          have_warned_about_null_delay := true;
          after (sec 0.2)
          >>> fun () ->
          loop ()
        end else begin
          t.file_pos <- Int64.(t.file_pos + of_int len);
          if !have_warned_about_null_delay then
            warning t No_longer_delayed_due_to_null_reads;
          Ivar.fill finished (Chunker.feed t.chunker t.read_buf ~pos:0 ~len)
        end
      end
    in
    loop ())
;;

(* [read_while_needed t] repeatedly does [read t] as long as [need_to_read t]. *)
let read_while_needed t =
  Deferred.create (fun finished ->
    let rec loop () =
      if am_alive t then begin
        if need_to_read t then begin
          try_with (fun () -> read t)
          >>> function
          | Error exn -> error t (Read_failed exn)
          | Ok updates ->
            write t updates
            >>> fun () ->
            loop ()
        end else begin
          if t.need_to_report_eof then begin
            t.need_to_report_eof <- false;
            warning t Reached_eof;
          end;
          Ivar.fill finished ()
        end
      end
    in
    loop ())
;;

let start_stat_loop t =
  let rec loop () =
    if am_alive t then begin
      (* We create [stat_loop_delay] now because we want the countdown to start as soon as
         [loop] is called, and not after [read_until_eof] has finished its work. *)
      let stat_loop_delay = after t.read_delay in
      stat t ~initial_call:false
      >>> function
      | Error () -> ()
      | Ok () ->
        read_while_needed t
        >>> fun () ->
        stat_loop_delay
        >>> fun () ->
        loop ()
    end
  in
  loop ()
;;

let start_reading t ~start_at =
  stat t ~initial_call:true
  >>> function
  | Error () ->
    (* If the initial stat failed, then it already put an error in the pipe and closed
       it, so there's nothing to do. *)
    ()
  | Ok () ->
    t.file_pos <-
      (match start_at with
       | `Beginning -> 0L
       | `End -> t.file_len
       | `Pos pos -> pos);
    start_eof_latency_check t;
    start_stat_loop t;
;;

let create
      ?(read_buf_len = 32 * 1024)
      ?(read_delay = sec 0.5)
      ?(retry_null_reads = true)
      ?(break_on_lines = true)
      ?(ignore_inode_change = false)
      ?(start_at = `Beginning)
      ?(eof_latency_tolerance = sec 5.)
      ?(null_read_tolerance = sec 0.)
      file =
  let pipe_r, pipe_w = Pipe.create () in
  let t =
    { file
    ; read_delay
    ; retry_null_reads
    ; chunker                       = Chunker.create ~break_on_lines
    ; pipe_w
    ; ignore_inode_change
    ; eof_latency_tolerance
    ; null_read_tolerance
    ; read_buf                      = String.create read_buf_len
    ; file_len                      = 0L
    ; most_recent_file_len_increase = Time.epoch
    ; file_pos                      = 0L
    ; file_inode                    = 0
    ; need_to_report_eof            = true
    }
  in
  start_reading t ~start_at;
  pipe_r;
;;
