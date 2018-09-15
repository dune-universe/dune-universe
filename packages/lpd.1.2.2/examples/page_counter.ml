(* This program keeps the number of pages each user prints.  It
   replaces the standard LPD (that you must therefore configure with
   "-s" not to listen on the TCP socket).  It acts as a simple "pass
   through" -- so spooling is attempted.

   © 2005 Christophe Troestler

   This program is distributed under the terms of the GNU General
   Public License.
*)
(* 	$Id: page_counter.ml,v 1.3 2005/07/09 12:07:47 chris_77 Exp $	 *)

(* Configure this part
 ***********************************************************************)

(* The file in which the history will be saved.  The format is CVS
   (comma separated values) with TAB separator in order to be easily
   readable by a spreadsheet. *)
let history_file = "/tmp/lpd_stat.csv"

(* The file [authorized_hosts_file] will contain a list of hostnames
   that are allowed to print, one per line.  A character '#' starts a
   comment that lasts till the end of the line.  Blank lines are
   ignored.  *)
let authorized_host_file = "/etc/hosts.lpd"

(* Unix queues and corresponding spool directories for which you
   accept remote jobs. *)
let queues = [
  ("net", "/var/spool/lpd/net/");
  ("abel", "/var/spool/lpd/abel");
]


(* end of configuration
 ***********************************************************************)

open Printf

(* Log to syslog *)
let logger msg =
  let cmd = sprintf "/usr/bin/logger -t page_counter %S" msg in
  ignore(Sys.command(cmd))

(* Disable logging *)
(* let logger _ = () *)


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



(* Number of pages of a PS or PDF document
 ***********************************************************************)

(* From the "ps2ps" script: command to normalize the PS file -- thus
   the result will contain "%%Pages:" comment. *)
let gs fname =
  sprintf "/usr/bin/gs \
    -q -sDEVICE=pswrite -sOutputFile=- -dNOPAUSE -dBATCH -dSAFER %S" fname

(* Count the number if pages in the file [fname].  Returns a negative
   number to indicate error. *)
(* FIXME: This is a bit slow. *)
let number_of_pages fname =
  let fh = Unix.open_process_in (gs fname) in
  let npages = ref(-1) in
  (try
     while true do
       let line = input_line fh in
       if is_prefix "%%Pages:" line then (
	 (* Note: "%%Pages: (atend)" also likely *)
	 try
	   let l = String.length line in
	   npages := int_of_string(String.sub line 9 (l - 9))
	 with Failure _ -> ()
       )
     done
   with End_of_file -> ());
  ignore(Unix.close_process_in fh);
  !npages


(* Printing functions
 ***********************************************************************)

(* Create [history_file] if it does not exists and put a one line
   header. *)
let () =
  if not(Sys.file_exists history_file) then (
    let flags = [Open_wronly; Open_creat; Open_append; Open_text] in
    let fh = open_out_gen flags 0o644 history_file in
    output_string fh "Date\tQueue\t\
      User\tJob #\tHost\tFile\tPages\tBytes\t#Copies\n";
    close_out fh
  )

let is_safe c =
  ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

(* Return a safe [host] string, keeping only alpha-numeric characters. *)
let sanitize host =
  let len = String.length host
  and slen = ref 0 in
  for i = 0 to len - 1 do
    if is_safe(String.unsafe_get host i) then incr slen
  done;
  if !slen = len then host
  else if !slen = 0 then "not_valid"
  else
    let s = Bytes.create !slen in
    let j = ref 0 in
    for i = 0 to len - 1 do
      let c = String.unsafe_get host i in
      if is_safe(c) then (Bytes.unsafe_set s !j c; incr j)
    done;
    Bytes.unsafe_to_string s

(* [copy oldfile newfile] moves the [oldfile] to [newfile].  It works
   accross filesystems.  It does not erase [oldfile]. *)
let copy oldfile newfile =
  let buf = Bytes.create 4096 in
  let fd0 = Unix.openfile oldfile [Unix.O_RDONLY] 0o600 in
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] in
  let fd1 = (try Unix.openfile newfile flags 0o600
             with e -> Unix.close fd0; raise e) in
  let rec copy_chunk () =
    let len = Unix.read fd0 buf 0 4096 in
    if len > 0 then (
      ignore(Unix.write fd1 buf 0 len);
      copy_chunk()
    ) in
  try
    copy_chunk();
    Unix.close fd0;
    Unix.close fd1
  with e ->
    Unix.close fd0;
    Unix.close fd1;
    raise e


(* Try to create a control file with the given job number but, if such
   file already exists, change the job number.  The control file is
   pre-filled with the general job data.

   FIXME: This could be improved by creating more names -- but it is
   already unlikely that it fails. *)
let spool_name spool job =
  let host = sanitize(job.Lpd.host) in
  let cf = ref(Filename.concat spool
		 (sprintf "cfA%03i%s" job.Lpd.number host)) in
  let n = ref(-1) in
  while !n < 1000 && Sys.file_exists(!cf) do
    incr n;
    cf := Filename.concat spool (sprintf "cfA%03i%s" !n host);
  done;
  if !n = 1000 then failwith(sprintf "%S is full!" spool);
  if !n = -1 then n := job.Lpd.number;
  (* General options in the control file *)
  let cfh = open_out_gen [Open_wronly; Open_creat; Open_text] 0o660 !cf in
  fprintf cfh "H%s\nP%s\n" job.Lpd.host job.Lpd.user;
  if job.Lpd.mailto <> "" then fprintf cfh "M%s\n" job.Lpd.mailto;
  (match job.Lpd.banner with
   | None -> ()
   | Some b -> fprintf cfh "L%s\nC%s\nJ%s\n"
       b.Lpd.user_name b.Lpd.class_name b.Lpd.job_name);
  (* Function to generate the data files *)
  let suffix = sprintf "%03i%s" !n host in
  let d = ref 'd'
  and a = ref(Char.chr(Char.code 'A' - 1)) in
  let data_file () =
    (* Compute the next value of the couple (d,a).  One generates the
       files dfA,... dfZ, dfa,... dfz, efA,... (the [suffix] always
       being the same: <job number><hostname>). *)
    if !a = 'Z' then a := 'a'
    else if !a = 'z' then (
      a := 'A';
      d := Char.chr(Char.code !d + 1);
      if !d > 'z' then failwith "Exhausted all datafile names!"
    )
    else a := Char.chr(Char.code !a + 1);
    sprintf "%cf%c%s" !d !a suffix in
  (cfh, data_file)

(* Check the number of pages of the job and then print it by creating
   the files (control and data) in the spool directory to print the
   job.  (The modification time of the control file determines the
   order in the queue.)  *)
let print queue spool job =
  if job.Lpd.number >= 0 then (
    let flags = [Open_wronly; Open_creat; Open_append; Open_text] in
    let fh = open_out_gen flags 0o644 history_file in
    let (cfh, data_file) = (try spool_name spool job
			    with e -> close_out fh; raise e) in
    let lpr f =
      let npages = number_of_pages f.Lpd.storage in
      if npages > 0 then (
	(* Logs the number of pages *)
	fprintf fh "%s\t%s\t%s\t%i\t%s\t%s\t%i\t%i\t%i\n"
	  (Lpd.string_of_current_time()) queue
	  job.Lpd.user job.Lpd.number job.Lpd.host
	  f.Lpd.name npages f.Lpd.size f.Lpd.nbcopies;
	(* Forward the job: add en entry to the control file and
	   rename appropriately the data file.  (Unix.rename cannot be
	   used for this job because it is likely that the original
	   file and the target are on different filesystems -- see man
	   rename(2) EXDEV.)  The original file is erased by the Lpd
	   library. *)
	let df = data_file() in
	fprintf cfh "f%s\nU%s\nN%s\n" df df f.Lpd.name;
	copy f.Lpd.storage (Filename.concat spool df)
      )
      else (
	(* Error -- maybe the document is not PS or PDF? *)
	logger(sprintf "%S (user %S) is invalid PS or PDF."
		 f.Lpd.name job.Lpd.user)
      ) in
    try
      List.iter lpr job.Lpd.files;
      close_out fh;
      close_out cfh
    with e ->
      close_out fh;
      close_out cfh;
      raise e
  )

let send_queue queue ~long _jobs outchan =
  let cmd = (if long then "lpq -l -P" else "lpq -P") ^ queue in
  let fh = Unix.open_process_in cmd in
  (try
     while true do
       let line = input_line fh in
       Socket.output_string outchan (line ^ "\n");
     done
   with End_of_file -> ());
  ignore(Unix.close_process_in fh)


(* Read the cfA... files [spool] directory and build an associative
   list of number -> user *)
let user_of_cf spool =
  let file = Sys.readdir spool in
  let l = ref [] in
  for i = 0 to Array.length file - 1 do
    if is_prefix "cfA" file.(i) && String.length file.(i) > 6 then (
      let num = (try int_of_string(String.sub file.(i) 3 3)
		 with Failure _ -> -1) in
      (* look for the user *)
      let fh = open_in(Filename.concat spool file.(i)) in
      (try
	 while true do
	   let line = input_line fh in
	   if is_prefix "P" line then (
	     let user = String.sub line 1 (String.length line - 1) in
	     l := (num, user) :: !l;
	     raise Exit
	   )
	 done
       with End_of_file | Exit -> ());
      close_in fh
    )
  done;
  !l

(* Since we will call lprm as root, we must handle the permissions
   here.  The security however is minimal -- given what is stored in
   control file, we cannot even verify that the removal order comes
   from the same machine which sent the job.  *)
let remove queue spool agent _addr jobs =
  if List.mem (Lpd.User agent) jobs then
    ignore(Unix.system (sprintf "/usr/bin/lprm -P%s %s" queue agent))
  else if jobs = [] then
    (* Remove the 1st job *)
    () (* not supported at the moment *)
  else
    let queued = user_of_cf spool in
    let filter nums = function
      | Lpd.User _ -> nums
      | Lpd.Num n ->
	  if List.mem (n, agent) queued
	  then (string_of_int n) ^ " " ^ nums
	  else nums  in
    let nums = List.fold_left filter "" jobs in
    if nums <> "" then
      ignore(Unix.system (sprintf "/usr/bin/lprm -P%s %s" queue nums))


let queue_actions queue spool = {
  Lpd.print = (fun () -> ());
  Lpd.on_reception = print queue spool;
  Lpd.send_queue = send_queue queue ~long:false;
  Lpd.send_queue_long = send_queue queue ~long:true;
  Lpd.remove = remove queue spool;
}



module C =
struct
  let queues = List.map (fun (q, spool) -> (q, queue_actions q spool)) queues

  let authorized_host = Lpd.these_hosts ~file:authorized_host_file []
  let log = logger
  let temp_dir = ""
end

module L = Lpd.Make(C)


let () = L.accept (L.socket()) L.daemon
