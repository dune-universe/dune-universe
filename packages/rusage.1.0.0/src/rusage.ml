(* Copyright (c) 2019 Zach Shipko

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE. *)

type t = {
  utime : float;
      (** {b User CPU time used} (seconds). The total amount of time spent
          executing in user mode. *)
  stime : float;
      (** {b System CPU time used} (seconds). The total amount of time spent
          executing in kernel mode. *)
  maxrss : int64;
      (** {b Maximum resident set size used} (kilobytes). If [who = Children],
          this is the resident set size of the largest child, not the maximum
          resident set size of the process tree. *)
  ixrss : int64;
      (** {b Integral shared memory size}. Currently unused on Linux. *)
  idrss : int64;
      (** {b Integral unshared data size}. Currently unused on Linux. *)
  isrss : int64;
      (** {b Integral unshared stack size}. Currently unused on Linux. *)
  minflt : int64;
      (** {b Minor page faults}. The number of page faults serviced without any
          I/O activity. Here, I/O activity is avoided by "reclaiming" a page
          frame form the list of pages awaiting re-allocation. *)
  majflt : int64;
      (** {b Major page faults}. The number of page faults that required I/O
          activity. *)
  nswap : int64;  (** {b Swaps}. Currently unused on Linux. *)
  inblock : int64;
      (** {b Block input operations}. The number of times the filesystem had to
          perform input. *)
  oublock : int64;
      (** {b Block output operations}. The number of times the filesystem had to
          perform output. *)
  msgsnd : int64;  (** {b IPC messages sent}. Currently unused on Linux. *)
  msgrcv : int64;  (** {b IPC messages received}. Currently unused on Linux. *)
  nsignals : int64;  (** {b Signals received}. Currently unused on Linux.*)
  nvcsw : int64;
      (** {b Voluntary context switches}. The number of times a context switch
          resulted due to a process voluntarily giving up the processor before
          its time slice was completed (usually to await availability of a
          resource). *)
  nivcsw : int64;
      (** {b Involuntary context switches}. The number of times a context switch
          resulted due to a higher priority process becoming runnable or because
          the current process exceeded its time slice. *)
}
(** [rusage] result struct. See manpage for the [getrusage] sys-call for more
    details. *)

type who =
  | Self
      (** Return resource usage statistics for the calling process, which is the
          sum of resources used by all threads in the process. *)
  | Children
      (** Return resource usage statistics for all children of the calling
          process that have terminated and been waited for. These statistics
          will include the resources used by grandchildren, and further removed
          descendants, if all of the intervening descendants waited on their
          terminated children.*)

external get : who -> t = "unix_getrusage"
