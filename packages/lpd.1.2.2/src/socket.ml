(* buffered socket abstraction
 **********************************************************************
 * © Christophe Troestler
 *
 * This library is distributed under the terms of the GNU Lesser
 * General Public License, with the special exception on linking as
 * for the OCaml Library.
 *)
(* Since (in|out)_channel_of_descr does NOT work under win32, we need
   to implement a minimal buffered interface. *)

(* Specialize [min] to integers for performance reasons (> 150% faster). *)
let min x y = if (x:int) <= y then x else y

let buffer_len = 4096

type out_channel = {
  out_fd : Unix.file_descr;
  out_buf : Bytes.t;  (* The data to output is in out_buf.[i], 0 <= i < out1 *)
  mutable out1 : int;(* out1 <= buffer_len ; < 0 iff channel closed. *)
}

let out_channel_of_descr fd = {
  out_fd = fd;
  out_buf = Bytes.create buffer_len;  out1 = 0; }

let descr_of_out_channel outchan = outchan.out_fd

type in_channel = {
  in_fd : Unix.file_descr;
  in_buf : Bytes.t;   (* The data in the in_buf is at indexes i s.t. *)
  mutable in0 : int; (* in0 <= i < in1. *)
  mutable in1 : int; (* Invariant: 0 <= in0 ; in1 <= buffer_len
                        in1 < 0 indicates a closed channel. *)
}

let in_channel_of_descr fd = {
  in_fd = fd;
  in_buf = Bytes.create buffer_len; in0 = 0; in1 = 0 }

let descr_of_in_channel inchan = inchan.in_fd

(*
 * Output functions
 *)

(* [flush_noerr oc] assumes that the channel is not closed,
   i.e. [oc.out1 >= 0] *)
let flush_noerr oc =
  let rec write i0 len =
    let w = Unix.write oc.out_fd oc.out_buf i0 len in
    if w < len then write (i0 + w) (len - w) in
  write 0 oc.out1;
  oc.out1 <- 0

let flush oc =
  if oc.out1 >= 0 then flush_noerr oc (* Channel [oc] not closed. *)

let close_out oc =
  if oc.out1 >= 0 then begin
    (* Not already closed *)
    flush_noerr oc;
    Unix.close oc.out_fd;
    oc.out1 <- -1
  end

let rec unsafe_output oc buf pos len =
  let w = min len (buffer_len - oc.out1) in
  Bytes.blit buf pos oc.out_buf oc.out1 w;
  oc.out1 <- oc.out1 + w;
  if w < len then begin
    flush_noerr oc;
    unsafe_output oc buf (pos + w) (len - w)
  end

let output oc buf pos len =
  if pos < 0 || len < 0 || pos + len > Bytes.length buf
  then invalid_arg "Socket.output";
  if oc.out1 < 0 then raise(Sys_error "Bad file descriptor");
  unsafe_output oc buf pos len

let output_string oc s =
  if oc.out1 < 0 then raise(Sys_error "Bad file descriptor");
  unsafe_output oc (Bytes.unsafe_of_string s) 0 (String.length s)

let output_char oc c =
  if oc.out1 < 0 then raise(Sys_error "Bad file descriptor");
  if oc.out1 = buffer_len then flush_noerr oc;
  Bytes.unsafe_set oc.out_buf oc.out1 c;
  oc.out1 <- oc.out1 + 1

let fprintf oc =
  Printf.kprintf (fun s -> output_string oc s)


(*
 * Input functions
 *)

let close_in chan =
  if chan.in1 >= 0 then begin
    (* [chan] not yet closed *)
    Unix.close chan.in_fd;
    chan.in0 <- 0;
    chan.in1 <- -1
  end

(* [fill_in_buf chan] refills in_buf if needed (when empty).  After this
   [in0 < in1] or [in1 = 0], the latter indicating that the end of
   file is reached (and then in0 = 0). *)
let fill_in_buf chan =
  if chan.in0 >= chan.in1 then begin
    chan.in0 <- 0;
    try
      chan.in1 <- Unix.read chan.in_fd chan.in_buf 0 buffer_len;
    with
    | Unix.Unix_error(Unix.EAGAIN, _, _)
    | Unix.Unix_error(Unix.EWOULDBLOCK, _, _) -> raise Sys_blocked_io
  end

let unsafe_input chan buf ofs len =
  fill_in_buf chan;
  let r = min len (chan.in1 - chan.in0) in
  Bytes.blit chan.in_buf chan.in0 buf ofs r;
  chan.in0 <- chan.in0 + r;
  r

let input ic buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > Bytes.length buf
  then invalid_arg "Socket.input";
  if ic.in1 < 0 then raise(Sys_error "Bad file descriptor");
  unsafe_input ic buf ofs len

let input_char ic =
  if ic.in1 < 0 then raise(Sys_error "Bad file descriptor");
  fill_in_buf ic;
  if ic.in1 = 0 then raise End_of_file
  else
    let c = Bytes.unsafe_get ic.in_buf ic.in0 in
    ic.in0 <- ic.in0 + 1;
    c


let rec unsafe_really_input ic s ofs len =
  if len > 0 then begin
    let r = unsafe_input ic s ofs len in
    if r = 0 then raise End_of_file
    else unsafe_really_input ic s (ofs+r) (len-r)
  end

let really_input ic s ofs len =
  if ofs < 0 || len < 0 || ofs + len > Bytes.length s
  then invalid_arg "Socket.really_input";
  if ic.in1 < 0 then raise(Sys_error "Bad file descriptor");
  unsafe_really_input ic s ofs len



(* [index_in_range i0 i1 c s] returns the index [j] of the first
   occurrence of [c] in s.[i], i0 <= i < i1.  If no occurence of [c]
   is found, it will return [i1].  It is assumed that 0 <= i0 and i1
   <= String.length s. *)
let index_in_range i0 i1 c s =
  let rec examine i =
    if i < i1 then
      if Bytes.unsafe_get s i = c then i
      else examine (i+1)
    else i1 in
  examine i0


let input_till c ic buf ofs len =
  if ofs < 0 || len < 0 || ofs + len > Bytes.length buf
  then invalid_arg "Socket.input_till";
  if ic.in1 < 0 then raise(Sys_error "Bad file descriptor");
  fill_in_buf ic;
  if ic.in1 = 0 then raise End_of_file;
  let in1 = min (ic.in0 + len) ic.in1 in
  let i = index_in_range ic.in0 in1 c ic.in_buf in
  let r = i - ic.in0 in
  Bytes.blit ic.in_buf ic.in0 buf ofs r;
  ic.in0 <- i;
  r

let rec input_till_char acc c ic =
  fill_in_buf ic;
  if ic.in1 = 0 then
    if acc = Bytes.empty then raise End_of_file else acc
  else begin
    (* Buffer contains something (ic.in0 < ic.in1), seek [c]. *)
    let i = index_in_range ic.in0 ic.in1 c ic.in_buf in
    (* FIXME: use a buffer: *)
    let line = Bytes.cat acc (Bytes.sub ic.in_buf ic.in0 (i - ic.in0)) in
    ic.in0 <- i + 1; (* skip [c] *)
    if i = ic.in1 then input_till_char line c ic
    else line
  end

let input_line ic =
  if ic.in1 < 0 then raise(Sys_error "Bad file descriptor");
  Bytes.unsafe_to_string(input_till_char Bytes.empty '\n' ic)

let input_all_till c ic =
  if ic.in1 < 0 then raise(Sys_error "Bad file descriptor");
  Bytes.unsafe_to_string(input_till_char Bytes.empty c ic)


(* Define an [open_connection] alike the one in the Unix lib for
   convenience. *)
let open_connection sockaddr =
  let sock =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  try
    Unix.connect sock sockaddr;
    (in_channel_of_descr sock, out_channel_of_descr sock)
  with exn ->
    Unix.close sock; raise exn

let shutdown_connection ic =
  Unix.shutdown (descr_of_in_channel ic) Unix.SHUTDOWN_SEND


(* Polling *)

let select inl outl t =
  (* Gather input channels whose buffer is not empty *)
  let (inbuf, inempty) = List.partition (fun c -> c.in0 < c.in1) inl in
  (* Check the file descriptors *)
  let inempty_fd = List.map (fun c -> c.in_fd) inempty in
  let out_fd = List.map (fun c -> c.out_fd) outl in
  let (in_ready, out_ready, _) = Unix.select inempty_fd out_fd [] t in
  (* Gather the results *)
  (inbuf @ List.filter (fun c -> List.mem c.in_fd in_ready) inempty,
   List.filter (fun c -> List.mem c.out_fd out_ready) outl)


(* I/O object *)

(* FIXME: Sys_blocked_io / EAGAIN ??? *)
class out_channel_obj chan_init =
object
  val chan = chan_init

  method output buf pos len =
    output chan buf pos len;
    len

  method flush () = flush chan
  method close_out () = close_out chan

  method output_string = output_string chan
  method output_char = output_char chan
  method fprintf : 'a. ('a, unit, string, unit) format4 -> 'a
    = fprintf chan
end

class in_channel_obj chan_init =
object
  val chan = chan_init

  method input = input chan
  method close_in() = close_in chan

  method input_char () = input_char chan
  method really_input = really_input chan
  method input_till c = input_till c chan
  method input_line () = input_line chan
  method input_all_till c = input_all_till c chan
end
