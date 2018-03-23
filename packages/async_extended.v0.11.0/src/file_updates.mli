open! Core
open! Async

module Update : sig
  type 'a t =
    | Update of 'a
    | Error of ([`Transform | `Read] * exn)
end

(* [updates filename ~transform] returns a pipe that contains one value every time
   [filename] is updated or whenever there is an error reading/transforming [filename].
   [transform] is simply passed the full contents of the filename.  Under the covers this
   code stats [filename] every [poll_interval] (default 5s) and reloads the file whenever
   the mtime has changed from the last successful load.  If the reader is closed the file
   will no longer be polled.
*)
val updates : 
     ?poll_interval:Time.Span.t 
  -> string 
  -> transform:([`Of_sexp of Sexp.t -> 'a | `Of_raw of (string -> 'a)])
  -> 'a Update.t Pipe.Reader.t
