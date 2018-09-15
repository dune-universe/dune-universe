(* File: lpd.mli

   Copyright 2004 Christophe.Troestler(at)umh.ac.be
   Web: http://math.umh.ac.be/an/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 3 or later as published by the Free Software Foundation, with
   the special exception on linking described in file LICENSE.txt.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE.txt for more details.
*)

(** LPD protocol server library (RFC 1179 compliant).

    @author Christophe Troestler
    @version 1.2.2
*)


(** {2 Types describing a print job} *)

(** Informations for a banner page. *)
type banner = {
  user_name : string; (** User name for the banner page. *)
  class_name : string; (** Class name, conventionally used to display the
                           host from which the printing job originated. *)
  job_name : string; (** Job name, conventionally used to display the
                         name of the file or files which were "printed".  *)
}

(** Information on the type of the file to be printed. *)
type file_type =
  | Text of int * int
      (** [Text(indent, number_of_columns)]: print as a text file,
          providing page breaks as necessary.  According to the RFC,
          any ASCII control chars other than '\b', '\t', '\n', '\012',
          '\r' should be discarded -- it is your task to do so as you
          know better how to deal with them (accented letters,...)
          especially when you know PDF files may be sent using that type!  *)
  | Bin     (** Binary file to be printed as is (including control chars). *)
  | PS      (** PostScript file. *)
  | DVI     (** TeX DVI file. *)
  | Troff of string * string * string * string
      (** [Troff(R font, I font, B font, S font)]: troff output file. *)
  | Ditroff (** ditroff (device independent troff) output. *)
  | CIF     (** CalTech Intermediate Form graphics language. *)
  | Plot    (** Output from the Berkeley Unix plot library. *)
  | Pr of string * int
      (** [Pr(title, number_of_columns)]: print with a heading, page
          numbers, and pagination.  The heading should include the
          date and time that printing was started, the [title], and a
          page number identifier followed by the page number. *)
  | Fortran (** Print interpreting the first column of each line as
                FORTRAN carriage control.  *)
  | Raster  (** Sun raster format file. *)

type file = {
  name: string;         (** Name of the file on the client machine.
                            [""] means standard input. *)
  size: int;            (** Size in bytes of the file. *)
  nbcopies: int;        (** Number of copies requested. *)
  storage: string;      (** Local file on the disk holding the data. *)
  of_type: file_type;   (** Type of the file. *)
}

type job = {
  number : int;
  (** Three digits job number.  A negative number indicates that the
      client did not send a correct job number. *)
  user : string;
  (** User identification of the entity requesting the printing job. *)
  host : string;
  (** Host which is to be treated as the source of the print job.
      This is not necessarily the same as the machine which connected
      this server -- the job may have been routed through several
      computers. *)
  mailto : string;
  (** If different from [""], request that a mail be sent to the given
      address when the printing operation ends (successfully or
      unsuccessfully).  There is no guarantee that the email address
      is even well formed (e.g. it may only be the login name). *)
  banner : banner option; (** Possible banner page. *)
  files : file list;
  (** List of files of this job.  The files will be removed after the
      job handling function [on_reception] return.  If you want to
      keep them longer, rename them or (better) move them to another
      directory. *)
  addr : Unix.sockaddr; (** Address of the machine which connected to
                            this server. *)
}


(** {2 Module specifying the daemon behavior} *)

type jobref =
  | User of string (** Reference all jobs of the given user *)
  | Num of int     (** Reference the job with the given number *)

(** Functions to be executed on queue events. *)
type queue_actions = {
  print : unit -> unit;
  (** Function executed when it is requested to start the printing
      process (if not already running). *)
  on_reception: job -> unit;
  (** Handler for each job received. *)
  send_queue : jobref list -> Socket.out_channel -> unit;
  (** Handler for sending a queue state in short form. *)
  send_queue_long : jobref list -> Socket.out_channel -> unit;
  (** Handler for sending a queue state in long form. *)
  remove : string -> Unix.sockaddr -> jobref list -> unit;
  (** Handler for removing jobs.  The [string] is the user name (the
      agent) requesting the removal and [Unix.sockaddr] is the address
      of the machine which connected the server (to enable security
      checks).  [lprm -], requesting to remove all the jobs that the
      user owns, gives as a list of jobs: [[User "-"]]. *)
}


module type CONFIG =
sig
  val queues : (string * queue_actions) list
    (** Give the names of the allowed queues and the actions to be
        taken for each of them.  Do not forget that it is your
        responsability to remove the jobs files on the disk (we have
        no way to know for how long you need them).  None of the queue
        names can contain a space (i.e. ' ', '\t', '\012' or
        '\013'). *)

  val authorized_host : Unix.sockaddr -> bool
    (** [authorized_host addr] determines whether a connection from
        [addr] is accepted. *)

  val log : string -> unit
    (** How to log connections and protocol.  It is the responsability
        of this function to add a final ["\n"] and to flush the
        necessary channel.  [print_endline] is an easy choice.  It is
        customary to add a timestamp to the messages being logged.
        {!Lpd.string_of_current_time} can help you with that.  *)

  val temp_dir : string
    (** Temprary directory to store the jobs sent.  If equal to [""],
        the value of the environment variable [TMPDIR] (or [TEMP] on
        win32) will be used. *)
end


(** Make a Line Printer daemon according to the configuration [C].

    The functions are splitted to offer various points where threads
    can be launched.

    @raise Failure if one if the queue names contains a space.
*)
module Make(C : CONFIG) :
sig
  val socket : ?port:int -> unit -> Unix.file_descr
    (** [socket ?port ()] creates a socket for the LPD daemon.

        @param port allows to specify on which port the socket
        listens.  By default it is the standard port for LPD i.e. 515.
    *)

  val accept : ?thread:((unit -> unit) -> unit) ->
    Unix.file_descr ->
    (Unix.sockaddr -> Socket.in_channel -> Socket.out_channel -> unit) -> 'a
    (** [accept ?thread socket f] listen on [socket] and, for each
        authorized connection (see [C.authorized_host]), runs
        [f addr inchan outchan] where [addr] is the address of the
        connecting machine and [inchan], [outchan] are buffered
        communication channels connected to the client.  [accept]
        never returns normally.

        @param thread tells how to run [f] in a separate thread.  A
        typical example is to declare it as
        [fun f -> ignore(Thread.create f ())] but of course one can
        also arrange
        the reuse a thread of a pool.  The default is not to create a
        new thread.  Do {i not} use that function to clone the process
        with [fork] or file descriptors will leak. *)

  val daemon : Unix.sockaddr -> Socket.in_channel -> Socket.out_channel -> unit
    (** [deamon addr inchan outchan] will read LPD queries on [inchan]
        and send replies on [outchan].  The particular treatement each
        query receives is determined by [C.queues].  [addr] is the
        address of the client.  This function is typically used as
        [accept (socket()) daemon].  *)
end


(** {2 Useful functions} *)

val string_of_current_time : unit -> string
  (** [string_of_current_time()] returns the current date and time. *)

val header_of_job : string -> string
  (** [header_of_job queue] returns a string suitable as a header for
      {!Lpd.string_of_job}.  If [queue = ""], the mention of the queue
      is omitted. *)

val string_of_job : int -> job -> string
  (** [string_of_job rank job] returns a one line string describing
      the [job].  A job with an empty list of files is considered to
      have been canceled.  The [rank] is a positive number giving the
      rank of the job in the queue.  A [rank] if [0] means that the
      job is in the printing stage.  [string_of_job] is a helper
      function to design a [send_queue] callback (see
      {!Lpd.queue_actions}). *)

val long_string_of_job : int -> job -> string
  (** [long_string_of_job rank job] does the same as
      {!Lpd.string_of_job} except that the description is in long
      format (multi-lines).  It is suitable to write [send_queue_long]
      callback (see {!Lpd.queue_actions}). *)

val any_host : Unix.sockaddr -> bool
  (** [any_host addr] accepts any Internet host which can be found in
      the DNS. *)

val these_hosts : ?file:string -> string list -> (Unix.sockaddr -> bool)
  (** [these_hosts ?file hosts adrr] accepts connections from Internet
      hosts or IP addresses listed in the file or in the list [hosts].

      @param file filename containing a list of hosts and IP addresses
      to authorize separated with white space, tabulations, or
      newlines.  Blanks lines are ignored.  Comments start with '#'
      and last till the end of the line. *)
