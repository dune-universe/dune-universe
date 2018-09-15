(* Library LDP prorocol (RFC1179).

   © 2004-present Christophe Troestler

   This program is distributed under the terms of the GNU Lesser
   General Public License, with the special exception on linking as
   for the OCaml Library.  See file LICENSE.txt
*)


open Printf

(* let debug msg = prerr_endline ("[DEBUG] " ^ msg) *)
let debug _msg = ()


(* Useful functions
 ***********************************************************************)

(* [split is_cut ?pos s] splits the part of string [s] of indices [>=
   pos] at (any group of) the characters spotted by the function
   [is_cut].  It is assumed that 0 <= pos < String.length s.  All
   elements of the returned list are nonempty strings.  In particular,
   [is_cut] chars at the beginning or end of [s] are ignored. *)
let split is_cut ?(pos=0) s =
  let rec seek_cut acc i0 i1 =
    if i1 >= String.length s then
      (String.sub s i0 (i1 - i0)) :: acc
    else if is_cut(String.unsafe_get s i1) then
      skip ((String.sub s i0 (i1 - i0)) :: acc) (i1 + 1)
    else
      seek_cut acc i0 (i1 + 1)
  and skip acc i =
    if i >= String.length s then acc
    else if is_cut(String.unsafe_get s i) then skip acc (i + 1)
    else seek_cut acc i (i + 1) in
  List.rev(skip [] pos)


let contains is_sought s =
  let rec search i =
    if i >= String.length s then false
    else is_sought(String.unsafe_get s i) || search (i + 1) in
  search 0

let contains_only is_valid s =
  let rec search i =
    if i >= String.length s then true
    else is_valid(String.unsafe_get s i) && search(i + 1) in
  search 0


(* [really_input_tochan sk oc len] reads [len] characters from the
   abstract socket [sk], outputing them to the channel [oc].

   @raise [End_of_file] if the end of file is reached before [len]
   characters have been read. *)
let rec really_input_tochan inchan oc len =
  let buffer = Bytes.create 4096 in
  if len > 0 then begin
    let r = Socket.input inchan buffer 0 (min 4096 len) in
    if r = 0 then raise End_of_file
    else begin
      output oc buffer 0 r;
      really_input_tochan inchan oc (len-r)
    end
  end

(* [input_tochan_till sk oc c] reads characters from the abstract
   socket [sk], outputing them to the channel [oc] until a character
   [c] is encountered ([c] read but not outputted to [oc]).

   @raise [End_of_file] if the end of file is reached (i.e., if no
   more characters can be read).  All previously read characters are
   sent to [oc]. *)
let rec input_tochan_till sk oc c =
  let buffer = Bytes.create 4096 in
  let r = Socket.input_till '\000' sk buffer 0 4096 in
  if r > 0 then begin
    output oc buffer 0 r;
    input_tochan_till sk oc c
  end
  else
    ignore(Socket.input_char sk) (* Remove [c] from the input stream. *)




(* Types to represent the jobs
 ***********************************************************************)

type file_type =
  | Text of int * int   (* f -> Text(I -> indent, W -> width) *)
  | Bin                 (* l *)
  | PS                  (* o *)
  | DVI                 (* d *)
  | Troff of string * string * string * string (* t -> troff(1, 2, 3, 4) *)
  | Ditroff             (* n *)
  | CIF                 (* c *)
  | Plot                (* g *)
  | Pr of string * int  (* p -> Pr(N|T -> title, W -> width) *)
  | Fortran             (* r *)
  | Raster              (* v *)

(* For holding the values when parsing the control file *)
type file_type_params = {
  indent : int;         (* I *)
  width : int;          (* W *)
  r_font : string;      (* 1 *)
  i_font : string;      (* 2 *)
  b_font : string;      (* 3 *)
  s_font : string;      (* 4 *)
  pr_title : string;    (* T *)
}

(* In the control file, the parameters are set BEFORE the printing
   command is issued (with the exception of 'U' and 'N').  For 'T', we
   assume that, if no new title is issued, it "falls through" (i.e. it
   serves for several jobs).  FIXME: the RFC is vague on this. *)
let file_type_of_char ftype = function
  | 'c' -> CIF
  | 'd' -> DVI
  | 'f' -> Text(ftype.indent, ftype.width) (* 'I', 'W' set before 'f' *)
  | 'g' -> Plot
  | 'l' -> Bin
  | 'n' -> Ditroff
  | 'o' -> PS
  | 'p' -> Pr(ftype.pr_title, ftype.width) (* 'T' set before 'p' *)
  | 'r' -> Fortran
  | 't' -> Troff(ftype.r_font, ftype.i_font, ftype.b_font, ftype.s_font)
  | 'v' -> Raster
  | _ -> failwith "file_type_of_char"

let default_type_params = {
  indent = 0;
  width = 132; (* from RFC *)
  r_font = "Times Roman";
  i_font = "Times Italic";
  b_font = "Times Bold";
  s_font = "Special Mathematical Font";
  pr_title = "";
}

type file = {
  name: string;       (* N *)
  size: int;
  nbcopies: int;
  storage: string;
  of_type: file_type;
}

(* Since all fields are immutable, we do not need a function to
   construct it. *)
let dummy_file = {
  name = ""; (* i.e. assume stdin *)
  size = 0;
  nbcopies = 1; (* at least 1 copy *)
  storage = "";
  of_type = Bin; (* safe default in case it is not supplied *)
}

type banner = {
  user_name : string;  (* L *)
  class_name : string; (* C *)
  job_name : string;   (* J *)
}

type job = {
  number : int;  (* from the control file name *)
  user : string; (* P, mandatory *)
  host : string; (* H, mandatory *)
  mailto : string; (* M, optional *)
  banner : banner option; (* L, to know whether requested *)
  files : file list; (* list of data files given in the control file. *)
  addr : Unix.sockaddr;
}


let update_banner_user job s =
  let banner = match job.banner with
    | None -> { user_name = s;  class_name = "";  job_name = "" }
    | Some b -> { b with user_name = s }  in
  { job with banner = Some banner }

let update_banner_class job s =
  let banner = match job.banner with
    | None -> { user_name = "";  class_name = s;  job_name = "" }
    | Some b -> { b with class_name = s }  in
  { job with banner = Some banner }

let update_banner_job job s =
  let banner = match job.banner with
    | None -> { user_name = "";  class_name = "";  job_name = s }
    | Some b -> { b with job_name = s }  in
  { job with banner = Some banner }



(* Specifying the daemon behavior
 ***********************************************************************)

type jobref =
  | User of string
  | Num of int

let jobref_of_list l =
  List.map (fun s -> try Num(int_of_string s) with Failure _ -> User s) l


type queue_actions = {
  print : unit -> unit;
  on_reception: job -> unit;
  send_queue : jobref list -> Socket.out_channel -> unit;
  send_queue_long : jobref list -> Socket.out_channel -> unit;
  remove : string -> Unix.sockaddr -> jobref list -> unit;
}


module type CONFIG =
sig
  val queues : (string * queue_actions) list
  val authorized_host : Unix.sockaddr -> bool
  val log : string -> unit
  val temp_dir : string
end


(* Make a Lpd module
 ***********************************************************************)

exception Invalid of string
  (* Exception raised when the RFC1179 standard is not respected or
     for internal errors.  The string is an error message.  *)

(* If the socket connection is closed on the client end, the SIGPIPE
   signal will be triggered, aborting the program.  We want to see the
   unix error, so disable the signal (if it exists for the OS). *)
let () =
  try Sys.set_signal Sys.sigpipe Sys.Signal_ignore
  with Invalid_argument _ -> ()

(* RFC1179 space: ASCII space, horizontal tab, vertical tab, and form feed *)
let is_space c =
  c = ' ' || c = '\t' || c = '\013' || c = '\012'

(* Default port *)
let pr_port =
  try (Unix.getservbyname "printer" "tcp").Unix.s_port
  with Not_found -> 515 (* RFC *)

let no_thread (f : unit -> unit) = f()
  (* Execute [f] in the current thread. *)

let default_temp_dir =
  match Sys.os_type with
  | "Unix" | "Cygwin" ->
      (try Sys.getenv "TMPDIR" with Not_found -> "/tmp")
  | "Win32" ->
      (try Sys.getenv "TEMP" with Not_found -> ".")
  | _ -> assert false


let remove_noerror fname = try Sys.remove fname with _ -> ()

(* [get_command_line sk] retrieves a line from socket [sk] and returns
   [(cmd, args)] where [cmd] is a character coding the LPD command and
   [args] is a list of arguments to this command.  The behavior
   differs a bit from the RFC.  Indeed, the RFC does not exclude queue
   names starting with spaces (but not ending because 5.3).  Here we
   split at spaces, thus possible spaces at the beginning of the queue
   name will be trimmed.

   @raise End_of_file if no command is found.
   @raise Invalid if the command does not possess the right structure. *)
let get_command_line sk =
  let line = Socket.input_line sk in
  if String.length line < 1 then raise (Invalid "Empty command!")
  else (String.unsafe_get line 0,
        split is_space ~pos:1 line)

(* [send_acknowledgment outchan b] sends a positive acknowledgement
   to the client if [b] is true and a negative one otherwise.  Remark:
   the acknowledgement must NOT be ended by a linefeed. *)
let send_acknowledgment outchan b =
  Socket.output_char outchan (if b then '\000' else '\001');
  Socket.flush outchan

(* [check_pos_num_and_ack sk n] returns the positive integer in the
   string [n] and sends a positive acknowledgment.  If any error
   occurs, it sends a negative acknowledgment and raises [Invalid].
*)
let check_pos_num_and_ack outchan count =
  let nbytes =
    try int_of_string count
    with Failure _ ->
      send_acknowledgment outchan false;
      raise (Invalid "File to receive: invalid length!") in
  if nbytes < 0 then begin
    send_acknowledgment outchan false;
    raise (Invalid "File to receive: length < 0!")
  end;
  send_acknowledgment outchan true;
  nbytes



module M = Map.Make(String)


module Make(C : CONFIG) =
struct
  (* Logging *)
  let log = C.log

  (* This code will refuse to create queues that contain spaces. *)
  let () =
    let check q =
      if contains is_space q then failwith(
        sprintf "Lpd.Make: queues, unlike %S, cannot contain spaces" q) in
    List.iter (fun (q,_) -> check q) C.queues

  (* Generic server function
   ***********************************************************************)

  let socket ?(port=pr_port) () =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.bind sock (Unix.ADDR_INET(Unix.inet_addr_any, port));
    Unix.listen sock 5;
    sock

  let accept ?(thread=no_thread) sock server_fun =
    while true do
      let (s, client) = Unix.accept sock in
      (* Check authorization for connecting. *)
      let client_addr =
        match client with
        | Unix.ADDR_UNIX s -> "unix socket " ^ s
        | Unix.ADDR_INET(addr, port) ->
            Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port in
      if C.authorized_host client then begin
        log(sprintf "ALLOW connection from " ^ client_addr);
        thread(fun () ->
                 let outc = Socket.out_channel_of_descr s in
                 try
                   (* Process the connection *)
                   server_fun client (Socket.in_channel_of_descr s) outc;
                   Socket.close_out outc;
                   (* Unix.close s; *) (* done by close_out *)
                   log("Closed connection from " ^ client_addr)
                 with e ->
                   Socket.close_out outc;
                   log("Connection from " ^ client_addr
                       ^ " closed by exception " ^ (Printexc.to_string e))
              )
      end
      else begin
        Unix.close s;
        log("DENY connection from " ^ client_addr)
      end
    done;
    assert(false)


  (* Creates temprary filenames in the indicated dir
   ***********************************************************************)

  let temp_dir = if C.temp_dir = "" then default_temp_dir else C.temp_dir

  let prng = Random.State.make_self_init()

  (* This is a version of Filename.open_temp_file tailored to our
     needs: it respects the [temp_dir], opens in binary mode (we want
     to save the files as they are sent -- this is very important for
     e.g. PDF files) and raises [Invalid] if anything goes wrong. *)
  let mode = [Open_wronly; Open_creat; Open_excl; Open_binary]
  let open_temp_file () =
    let rec try_name counter =
      if counter >= 1000 then
        raise(Invalid(sprintf "Lpd.open_temp_file: %S nonexistent or full"
                        temp_dir));
      let rnd = (Random.State.bits prng) land 0xFFFFFF in
      let name = Filename.concat temp_dir (sprintf "lpd%06x.dat" rnd) in
      try  (name, open_out_gen mode 0o600 name)
      with Sys_error _ -> try_name (counter + 1)
    in try_name 0


  (* Receive job subcommands
   ***********************************************************************)
  (* Several "lpr" commands can be grouped together and send during a
     unique connection.  Therefore, one can have several control (each
     of them corresponding to a job) and data files.  Since we do not
     know in which order they are, we will save them all and then
     process the control files.  Care is be taken to "print" the files
     in the order they are listed in the control file(s). *)

  let receive_control inchan outchan ctrl cfname nbytes =
    (* Read till we encounter '\000' and then send an acknowledgment. *)
    let newctrl = Socket.input_all_till '\000' inchan in
    debug(sprintf "CONTROL FILE: %S" newctrl);
    let nread = String.length newctrl in
    if nbytes = nread then (
      ctrl := (cfname, newctrl) :: !ctrl;
      send_acknowledgment outchan true
    )
    else (
      log(sprintf "  Control file size incorrect: expected=%i read=%i"
            nbytes nread);
      send_acknowledgment outchan false
    )

  (* [receive_data inchan outchan files dfname nbytes] gets [nbytes]
     from [inchan] (or till '\000' or [End_of_file] if [nbytes = 0]).
     If everything works fine, the new datafile is added to the file
     list [files] associated to its original name [dfname]. *)
  let receive_data inchan outchan files dfname nbytes =
    let (datafile, fh) = open_temp_file() in
    debug(sprintf "Created temporary file %S" datafile);
    try
      if nbytes > 0 then begin
        (* nbytes > 0, copy that many bytes to [fh]. *)
        really_input_tochan inchan fh nbytes;
        (* Get a single '\000' as acknowledgement of the end of file. *)
        let ack = Socket.input_char inchan in
        send_acknowledgment outchan (ack = '\000');
      end
      else begin
        (* nbytes = 0, thus '\000' (?) or [End_of_file] means that the
           file is complete.  FIXME: RFC1179 is vague on this point. *)
        (try input_tochan_till inchan fh '\000'
         with End_of_file -> ());
        send_acknowledgment outchan true
      end;
      close_out fh;
      if M.mem dfname !files then (
        (* Identifier [dfname] already received.  We needed to read the
           file -- so can go on to see what is next -- but we discard it. *)
        log(sprintf "Datafile %S already received.  Ignoring." dfname);
        remove_noerror datafile
      )
      else
        (* Ok, add the file to previously saved ones. *)
        files := M.add dfname datafile !files
    with exn ->
      close_out fh;
      remove_noerror datafile; (* [datafile] content is incorrect anyway. *)
      send_acknowledgment outchan false;
      raise exn


  (* [receive_job_loop inchan outchan ctrl files number] saves all
     control and data files send by the client on [inchan], updating the
     references [ctrl], [files] and [number] as necessary. *)
  let rec receive_job_loop inchan outchan ctrl files =
    match get_command_line inchan with
    | ('\001', []) ->
        (* Abort job. *)
        ctrl := []; (* all control files removed *)
        send_acknowledgment outchan true

    | ('\002', [count; name]) ->
        (* Receive control file (cfA000hostname). *)
        let nbytes = check_pos_num_and_ack outchan count in
        log (sprintf " Receiving control file %S (%i bytes)." name nbytes);
        receive_control inchan outchan ctrl name nbytes;
        receive_job_loop inchan outchan ctrl files

    | ('\003', [count; name]) ->
        (* Receive data file (dfA000hostname). *)
        let nbytes = check_pos_num_and_ack outchan count in
        log (sprintf " Receiving data file %S (%i bytes)." name nbytes);
        (* Do not save the data to [name] for security reasons. *)
        receive_data inchan outchan files name nbytes;
        receive_job_loop inchan outchan ctrl files

    | (c, args) ->
        let args = String.concat ", " args in
        log (sprintf " INCORRECT receive job subcommand %C [%S]." c args);
        receive_job_loop inchan outchan ctrl files (* try again *)



  (* [receive_job] gets the data and control files (till EOF is
     reached), process the control files and then call the handler [f]
     on the resulting job.  *)
  let receive_job client_addr inchan outchan f =
    let ctrl_files = ref [] (* list (cfname, ctrl file as string) *)
    and files = ref M.empty (* dfname -> storage_file *) in
    begin
      try receive_job_loop inchan outchan ctrl_files files
      with
      | End_of_file -> () (* Connection closed on the client side
                             while (hopefully) waiting for a command. *)
      | Invalid msg -> log(msg)
      | e -> log("receive_job_loop raised " ^ (Printexc.to_string e))
    end;
    (* Put the control files in the order they were sent. *)
    ctrl_files := List.rev !ctrl_files;

    (* -- Try to process what we got -- *)
    (* [add_file dfname file job] makes some sanity checks on [file]
       and, if it passes, add to to the [job.files].  Return the new
       job.  *)
    let add_file dfname file job =
      if dfname = "" then job (* for the 1st dummy file *)
      else
        try
          let storage_file = M.find dfname !files in
          let file = {file with
                        size = (Unix.stat storage_file).Unix.st_size;
                        storage = storage_file } in
          {job with files = file :: job.files}
        with Not_found ->
          log(sprintf "Datafile %S requested by control file but not sent!"
                dfname);
          job
    in
    (* [parse_ctrl cur_dfname cur_file job ctrl_lines] update the
       [job.files] and other [job] characteristics according to the
       info in the control file [ctrl_lines].  See [file_type_of_char]
       for what we assume about the order of parameters and printing
       commands.  *)
    let rec parse_ctrl cur_dfname cur_file ftype job = function
      | [] ->
          (* no more lines to process *)
          let job = add_file cur_dfname cur_file job in
          { job with files = List.rev job.files }

      | line :: tl when String.length line > 0 ->
          let arg = String.sub line 1 (String.length line - 1) in
          begin match String.unsafe_get line 0 with
          | 'C' ->
              let job = update_banner_class job arg in
              parse_ctrl cur_dfname cur_file ftype job tl
          | 'H' -> (* compulsory *)
              parse_ctrl cur_dfname cur_file ftype {job with host = arg} tl
          | 'I' ->
              let ftype =
                try {ftype with indent = int_of_string arg}
                with Failure _ -> ftype in
              parse_ctrl cur_dfname cur_file ftype job tl
          | 'J' ->
              let job = update_banner_job job arg in
              parse_ctrl cur_dfname cur_file ftype job tl
          | 'L' ->
              let job = update_banner_user job arg in
              parse_ctrl cur_dfname cur_file ftype job tl
          | 'M' ->
              parse_ctrl cur_dfname cur_file ftype {job with mailto = arg} tl
          | 'N' -> (* source name of the file *)
              (* The filename may contain spaces at the beginning or
                 at the end.  However, it may happen that "stdin" is
                 represented by a white space -- while we prefer "".  *)
              let fname = if contains_only is_space arg then "" else arg in
              parse_ctrl cur_dfname {cur_file with name=fname} ftype job tl
          | 'P' -> (* compulsory *)
              parse_ctrl cur_dfname cur_file ftype {job with user = arg} tl
          | 'S' -> (* ignore *)
              parse_ctrl cur_dfname cur_file ftype job tl
          | 'T' ->
              parse_ctrl cur_dfname cur_file {ftype with pr_title=arg} job tl
          | 'U' ->
              (* ignore -- it is the user of this lib responsability *)
              parse_ctrl cur_dfname cur_file ftype job tl
          | 'W' ->
              let ftype =
                try
                  let width = int_of_string arg in
                  if width >= 0 then {ftype with width=width} else ftype
                with Failure _ -> ftype in
              parse_ctrl cur_dfname cur_file ftype job tl
          | '1' ->
              parse_ctrl cur_dfname cur_file {ftype with r_font=arg} job tl
          | '2' ->
              parse_ctrl cur_dfname cur_file {ftype with i_font=arg} job tl
          | '3' ->
              parse_ctrl cur_dfname cur_file {ftype with b_font=arg} job tl
          | '4' ->
              parse_ctrl cur_dfname cur_file {ftype with s_font=arg} job tl

          (* -- Printing commands -- *)
          | c ->
              (* REMARK: we do not filter the control chars for 'f' *)
              let new_type, unknown_cmd =
                try file_type_of_char ftype c, false
                with Failure _ -> Bin, true in
              if unknown_cmd then parse_ctrl cur_dfname cur_file ftype job tl

              (* Each time one receives a printing command one must
                 decide whether it concerns the current file (in which
                 case, one more copy is requested) or whether it is a
                 new file (in which case, the previous one is added to
                 the job). *)
              else if arg = cur_dfname && new_type = cur_file.of_type then
                let file = {cur_file with
                              nbcopies = cur_file.nbcopies + 1} in
                parse_ctrl cur_dfname file ftype job tl
              else
                let file = match cur_file.of_type with
                  | Pr(t,w) ->
                      (* If the title of a 'p' file is empty, update
                         it to the source name of the file -- RFC §7.25. *)
                      if t = ""
                      then {cur_file with of_type=Pr(cur_file.name, w)}
                      else cur_file
                  | _ -> cur_file (* nothing to update *) in
                let job = add_file cur_dfname file job in
                let new_file = {dummy_file with of_type=new_type} in
                parse_ctrl arg new_file ftype job tl
          end
      | _ :: tl ->
          parse_ctrl cur_dfname cur_file ftype job tl
    in
    (* Create a job for each control file and call [f] on it. *)
    let job_of_ctrl (cfname, ctrl) =
      (* Extract the remote job number if possible *)
      let num =
        if String.length cfname < 6 then -1
        else (try int_of_string(String.sub cfname 3 3) with _ -> -1) in
      let init_job = {
        number = num;
        user = "";  host = "";  mailto = "";  banner = None;  files = [];
        addr = client_addr;
      } in
      let ctrl_lines = split (fun c -> c = '\n') ctrl in
      let job =
        parse_ctrl "" dummy_file default_type_params init_job ctrl_lines in
      try f job
      with e ->
        log("Queue action on_reception raised " ^ (Printexc.to_string e))
    in
    List.iter job_of_ctrl !ctrl_files;
    (* Remove all the files -- the ones to be kept have been moved by
       the callback [f].  (One must remember that nothing prevent to
       send data files that are not referenced in any control file...)
    *)
    M.iter (fun _ storage_file -> remove_noerror storage_file) !files


  let action queue =
    try List.assoc queue C.queues
    with Not_found -> raise(Invalid(queue ^ ": unknown queue"))

  let action_wrapper act f =
    try f ()
    with
    | Invalid msg -> log(msg)
    | e -> log("Queue action " ^ act ^ " raised " ^ (Printexc.to_string e))

  (* Daemon
   ***********************************************************************)
  (* [deamon client_addr inchan outchan] communicates with the client
     connected to the channels according to the LPD protocol.  *)
  let daemon client_addr inchan outchan =
    try
      match get_command_line inchan with
      | ('\001', [queue]) ->
          (* Print any waiting jobs *)
          log ("Print any waiting job for " ^ queue);
          action_wrapper "print" (action queue).print

      | ('\002', [queue]) ->
          (* Receive a printer job *)
          log ("Receiving job for queue " ^ queue);
          let f, queue_exists =
            try (List.assoc queue C.queues).on_reception, true
            with Not_found -> (fun _ -> ()), false in
          if queue_exists then (
            send_acknowledgment outchan true;
            receive_job client_addr inchan outchan f;
          )
          else (
            log(queue ^ ": unknown queue");
            send_acknowledgment outchan false (* and close connection *)
          )

      | ('\003', queue :: list) ->
          (* Send queue state (short) *)
          log ("Send state of the queue " ^ queue);
          action_wrapper "send_queue"
            (fun () -> (action queue).send_queue (jobref_of_list list) outchan)

      | ('\004', queue :: list) ->
          (* Send queue state (long) *)
          log ("Send long state of the queue " ^ queue);
          action_wrapper "send_queue_long"
            (fun () ->
               (action queue).send_queue_long (jobref_of_list list) outchan)

      | ('\005', queue :: agent :: list) ->
          (* Remove jobs *)
          log(sprintf "Remove job for queue %S and agent %S" queue agent);
          action_wrapper "remove"
            (fun () ->
               (action queue).remove agent client_addr (jobref_of_list list))

      | _ -> raise (Invalid "Invalid command!")

    with End_of_file ->
      (* This may be raised by [get_command_line].  This must be
         considered a mistake because one knows how much data to
         read so should never read more. *)
      log("Premature end of input stream!");
end



(*
 *                 H E L P E R   F U N C T I O N S
 *
 ************************************************************************)


let string_of_current_time () =
  let t = Unix.localtime(Unix.time()) in
  sprintf "%4i-%02i-%02i %2i:%02i:%02i"
    (1900 + t.Unix.tm_year) (1 + t.Unix.tm_mon) t.Unix.tm_mday
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec


(* Authorization functions *)

(* Accept all connections provided one can find the address in DNS and
   the source port conform the RFC. *)
let any_host client =
  match client with
  | Unix.ADDR_UNIX _ -> false
  | Unix.ADDR_INET (addr, _port) ->
      try
        let _ = (Unix.gethostbyaddr addr).Unix.h_name in
        (* 721 <= port && port <= 731 *)
        true
      with Not_found -> false

let authorized_host_of_file fname l =
  try
    let fh = open_in fname in
    let hosts = ref l in
    begin try
      while true do
        let l = input_line fh in
        let l = (try String.sub l 0 (String.index l '#')
                 with Not_found -> l) in
        hosts := List.rev_append (split is_space l) !hosts
      done;
    with End_of_file -> ()
    end;
    close_in fh;
    !hosts
  with _ -> []

let these_hosts ?file hosts =
  let hosts = match file with
    | None -> hosts
    | Some fname -> authorized_host_of_file fname hosts in
  fun client ->
    match client with
    | Unix.ADDR_UNIX _ -> false
    | Unix.ADDR_INET (addr, _port) ->
        try
          let client_addr = Unix.string_of_inet_addr addr  in
          (* If the IP is listed, we do not need to try to resolve it. *)
          List.mem client_addr hosts
          || (let client_name = (Unix.gethostbyaddr addr).Unix.h_name in
              List.mem client_name hosts)
        with Not_found -> false



let header_of_job =
  let header = sprintf "%-6s %-9s %4s  %-37s %s\n"
    "Rank" "Host" "Job" "Files" "Total Size" in
  fun queue ->
    if queue = "" then header
    else sprintf "Jobs for %s:\n%s" queue header

let string_of_rank = function
  | 0 -> "print"
  | 1 -> "1st"
  | 2 -> "2nd"
  | 3 -> "3rd"
  | r -> string_of_int r ^ "th"

let string_of_job rank job =
  let host =
    if String.length job.host <= 9 then job.host
    else (String.sub job.host 0 6) ^ "..." in
  let files_nbytes = match job.files with
    | [] -> "(Canceled)"
    | [file] ->
        let l = String.length file.name in
        sprintf "%-37s %i bytes"
          (if l = 0 then "(standard input)"
           else if l <= 37 then file.name
           else "..." ^ (String.sub file.name (l - 34) 34))
          (file.nbcopies * file.size)
    | _ ->
        let files =
          String.concat ", " (List.map (fun f -> f.name) job.files) in
        let files =
          if String.length files <= 37 then files else
            let name f = Filename.basename f.name in
            let files = String.concat ", " (List.map name job.files) in
            let l = String.length files in
            if l <= 37 then files
            else "..." ^ (String.sub files (l - 34) 34) in
        sprintf "%-37s %i bytes"
          files
          (List.fold_left (fun a f -> a + f.nbcopies * f.size) 0 job.files)
  in
  let rank = string_of_rank rank in
  sprintf "%-6s %-9s %4i  %s\n" rank host job.number files_nbytes


let day d =
  [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" |].(d)
let month m =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep";
     "Oct"; "Nov"; "Dec" |].(m)

let string_of_file file =
  let copies =
    if file.nbcopies = 1 then ""
    else string_of_int file.nbcopies ^ " copies of " in
  let l = String.length file.name in
  let avail = 33 - String.length copies in
  let fname =
    copies
    ^ (if l = 0 then "(standard input)"
       else if l <= avail then file.name
       else let w = avail - 3 in "..." ^ (String.sub file.name (l - w) w))
  and date =
    let t = Unix.localtime (Unix.stat file.storage).Unix.st_mtime in
    sprintf "%s %s %2i %2i:%02i:%02i %i"
      (day t.Unix.tm_wday) (month t.Unix.tm_mon) t.Unix.tm_mday
      t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec (1900 + t.Unix.tm_year) in
  sprintf "    %-33s %8i bytes -- %s\n" fname file.size date

let long_string_of_job rank job =
  let files =
    if job.files = [] then "    (Canceled)\n"
    else String.concat "" (List.map string_of_file job.files) in
  sprintf "%s: %s\t\t\t\t[job %03i%s]\n%s" job.user (string_of_rank rank)
    job.number job.host files
