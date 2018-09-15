(* This short program waits for a connection on the printer port and
   uses the ghostscript program to forward the data to the WINDOWS
   printer.  It is especially useful when no unix driver is available.

   © 2005 Christophe Troestler

   This program is distributed under the terms of the GNU General
   Public License.
*)


(* Configure this part
 ***********************************************************************)

(* Associative list of unix printer names (the queues for LPD) to
   corresponding windows printer names. *)
let win_printers = [
  ("HP1500L", "HP Color LaserJet 1500");
  ("EPS1160", "EPSON stylus COLOR 1160");
]

(* The file [authorized_hosts_file] will contain a list of hostnames
   that are allowed to print, one per line.  A character '#' starts a
   comment that lasts till the end of the line.  Blank lines are
   ignored.  *)
let authorized_host_file = "hosts.lpd"

(* The spool directory.  It will be created on startup if it does not
   exists. *)
(* let spool_dir = "c:\\spool" *)
let spool_dir = "/tmp/spool"

(* Full path of the GSPRINT program (in the GSView package).  Beware
   that this path must be legal for DOS, so names with white spaces
   must be quoted. *)
let gsprint = "c:\\PROGRA~1\\Ghostgum\\gsview\\gsprint.exe"


(* end of configuration
 ***********************************************************************)

open Printf
let debug msg = prerr_endline ("[DEBUG] " ^ msg)
(* let debug msg = () *)

let log = print_endline
(* [log_cont msg] append to the log but without the date added by Lpd. *)
let log_cont msg = log("                    " ^ msg)


let try_remove f =
  try Sys.remove f with _ -> log_cont(sprintf "NOT DELETED: %S" f)


(* Creation of temporary files in the spool directory *)
let () =
  try
    if (Unix.stat spool_dir).Unix.st_kind <> Unix.S_DIR then (
      printf "%S must be a directory!" spool_dir;
      exit 1
    )
  with Unix.Unix_error (Unix.ENOENT, "stat", _) ->
    (* Create it *)
    Unix.mkdir spool_dir 0o600

let prng = Random.State.make_self_init()
let mode = [Open_wronly; Open_creat; Open_excl; Open_binary]
let temp_file queue =
  let rec try_name counter =
    if counter >= 1000 then failwith(sprintf "Spool dir %S full!!!" spool_dir);
    let rnd = (Random.State.bits prng) land 0xFFFFFF in
    let name = Filename.concat spool_dir (sprintf "%s%06x.dat" queue rnd) in
    try
      close_out(open_out_gen mode 0o600 name);
      name
    with Sys_error _ -> try_name (counter + 1)
  in try_name 0


(* [is_prefix pre s] returns true if [pre] is a prefix of [s]. *)
let is_prefix =
  let rec is_pre i len pre s =
    if i < len then
      (String.unsafe_get pre i = String.unsafe_get s i)
      && is_pre (i+1) len pre s
    else true in
  fun prefix s ->
    (String.length prefix <= String.length s)
    && is_pre 0 (String.length prefix) prefix s


(* Tries to print the file [file] on the windows printer [win_queue].
   Returns [true] in case of success and [false] otherwise.  *)
let gs win_queue file =
  let cmd = sprintf "%s -color -printer %S -copies %i %S" gsprint win_queue
    file.Lpd.nbcopies  file.Lpd.storage in
  debug("EXEC: " ^ cmd);
  let fh = Unix.open_process_in cmd in
  let npages = ref 0 in
  (try
     while true do
       let line = input_line fh in
       if is_prefix "PRINT" line then incr npages
     done;
     assert(false)
   with End_of_file -> ());
  log_cont(sprintf "%i pages printed of %S"
             (file.Lpd.nbcopies * !npages) file.Lpd.name);
  match Unix.close_process_in fh with
  | Unix.WEXITED _ -> ()
  | _ -> ()


(* Convert a jobref list to a function to check whether a given job is
   referenced. *)
let job_is_in_list = function
  | [] -> (fun _ -> true) (* all jobs *)
  | l -> (fun job ->
	    List.mem (Lpd.Num job.Lpd.number) l
	    || List.mem (Lpd.User job.Lpd.user) l)

(* Compare the two addresses [a1] and [a2] to see whether they
   represent the same machine.  The port is allowed to differ. *)
let same_address a1 a2 =
  match a1, a2 with
  | Unix.ADDR_UNIX s1, Unix.ADDR_UNIX s2 -> s1 = s2
  | Unix.ADDR_INET(i1,_), Unix.ADDR_INET(i2,_) -> i1 = i2
  | _ -> false

(* Queues
 ***********************************************************************)
(* The model is the following.  Each print queue will have its own
   thread in charge of sending jobs to the printer.  Each incoming
   job will be added to the right queue from which the thread
   process will periodically pick the head.  The job being printed
   will be treated specially, with a flag indicating whether a
   (valid) request to kill it has been issued.  We will handle jobs
   in the queue through references in order to replace them with
   dummy versions in case they are cancelled (the file list will be
   emptied and these files will be erased).  *)

let make_queue queue win_queue =
  let q = Queue.create()
  and access = Mutex.create()
  and not_empty = Condition.create()
    (* [not_empty] signals when a new element is inserted in the queue
       making it nonempty. *)
  and kill_active = ref false in

  (* Return the 1st element from the queue or wait to be awaken by the
     condition.  It is supposed that [access] is locked when calling
     this function. *)
  let rec queue_peek () =
    try !(Queue.peek q)
    with Queue.Empty ->
      Condition.wait not_empty access;
      queue_peek () in

  let rec print () =
    (* Print a job (but leave it in the queue so [send_queue] still
       displays it. *)
    Mutex.lock access;
    let job0 = queue_peek() in
    Mutex.unlock access;
    (try List.iter (fun f ->
                     if !kill_active then raise Exit
                     else gs win_queue f
                   ) job0.Lpd.files;
     with Exit -> ());
    kill_active := false;
    (* Remove the job from the queue and its files *)
    Mutex.lock access;
    ignore(Queue.take q);
    Mutex.unlock access;
    List.iter (fun f -> try_remove f.Lpd.storage) job0.Lpd.files;
    (* Handle the next job *)
    print () in

  let add job =
    (* Rename the file to avoid its automatic deletion by Lpd *)
    let new_files = List.map (fun f ->
				let new_storage = temp_file queue in
				Unix.rename f.Lpd.storage new_storage;
				{f with Lpd.storage = new_storage}
			     ) job.Lpd.files in
    Mutex.lock access;
    Queue.add (ref {job with Lpd.files = new_files}) q;
    Mutex.unlock access;
    Condition.signal not_empty in

  let send_queue jobs outc =
    Socket.output_string outc (Lpd.header_of_job queue);
    let is_requested = job_is_in_list jobs in
    let i = ref 0 in
    Mutex.lock access;
    Queue.iter (fun j ->
		  if is_requested !j then
		    Socket.output_string outc (Lpd.string_of_job !i !j);
		  incr i) q;
    Mutex.unlock access in

  let send_queue_long jobs outc =
    let is_requested = job_is_in_list jobs in
    let i = ref 0 in
    Mutex.lock access;
    Queue.iter (fun j ->
		  if is_requested !j then
		    Socket.output_string outc (Lpd.long_string_of_job !i !j);
		  incr i) q;
    Mutex.unlock access in

  let remove agent addr jobs =
    (* There is no such thing as "root" on win98.  Do no treat "root"
       in any special way i.e., a job can only be removed if one owns
       it. *)
    if jobs = [] then (
      (* Remove the current active job -- if allowed. *)
      Mutex.lock access;
      (try
	 let job0 = !(Queue.peek q) in
	 if job0.Lpd.user = agent && same_address job0.Lpd.addr addr then
	   kill_active := true
       with Queue.Empty -> () (* nothing to be removed *)
      );
      Mutex.unlock access;
    )
    else (
      (* Remove only specified jobs -- if allowed. *)
      let is_requested =
	if List.mem (Lpd.User "-") jobs || List.mem (Lpd.User agent) jobs
	then (fun _ -> true) (* all jobs of the user *)
	else (fun job -> List.mem (Lpd.Num job.Lpd.number) jobs) in
      let i = ref 0 in
      let rm j =
	let job = !j in
	if is_requested job && job.Lpd.user = agent
	  && same_address job.Lpd.addr addr then (
	    if !i = 0 then (
	      kill_active := true (* kill active job *)
	    )
	    else (
	      (* Remove all data files *)
	      List.iter (fun f -> try_remove f.Lpd.storage) job.Lpd.files;
	      j := {job with Lpd.files = []}
	    )
	  );
	incr i in
      Mutex.lock access;
      Queue.iter rm q;
      Mutex.unlock access;
    )
  in
  (* Launch the printing thread *)
  let _ = Thread.create print () in
  (queue, {
     Lpd.print = (fun () -> ());
     Lpd.on_reception = add;
     Lpd.send_queue = send_queue;
     Lpd.send_queue_long = send_queue_long;
     Lpd.remove = remove;
   })



(* Link the printing model to the Lpd machinery
 ***********************************************************************)
module C =
struct
  let queues = List.map (fun (q, winq) -> make_queue q winq) win_printers

  let authorized_host = Lpd.these_hosts ~file:authorized_host_file []
  let log msg = log(Lpd.string_of_current_time() ^ " " ^ msg)
  let temp_dir = ""
end

module L = Lpd.Make(C)


let () =
  let hostname = Unix.gethostname() in
  let hostip =
    try
      let addr0 = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
      Unix.string_of_inet_addr addr0
    with e -> Printexc.to_string e in
  C.log(sprintf "OCaml LPD running on %s (%s)." hostname hostip);
  L.accept (L.socket()) L.daemon
