open Base
open Unix

let with_dir path f =
  let dh = opendir path in
  Exn.protect f dh ~finally:(fun _ -> closedir dh)

(* run [f] on files in [path] *)
let fold_dir path init f =
  with_dir path & fun dh ->
    let rec loop st =
      try
        let st' = f st (readdir dh) in
        loop st'
      with
      | End_of_file -> st
    in
    loop init


module Inodes = Set.Make(struct
  type t = int * int
  let compare : t -> t -> int = compare
end)

module Find = struct

  exception Prune

  class type path = object
    method base : string
    method depth : int
    method dev_inode : (int * int, [`Exn of exn]) Vresult.t
    method dir : string
    method is_dir : bool
    method is_ldir : bool
    method is_reg : bool
    method kind : (Unix.file_kind, [`Exn of exn]) Vresult.t
    method lkind : (Unix.file_kind, [`Exn of exn]) Vresult.t
    method path : string
    method stat : (Unix.stats, [`Exn of exn]) Vresult.t
    method lstat : (Unix.stats, [`Exn of exn]) Vresult.t
  end

  class path_ ~dir ~base ~depth =
    let path = match Filename.concat dir base with
      | "./." -> "."
      | s -> s
    in
    object (self)
      method dir = dir
      method base = base
      method path = path
      method depth : int = depth
      method stat : (_,[`Exn of exn]) Vresult.t = try Ok (stat path) with e -> Error (`Exn e)
      method lstat : (_,[`Exn of exn]) Vresult.t = try Ok (lstat path) with e -> Error (`Exn e)
      method kind : (_,[`Exn of exn]) Vresult.t = match self#stat with
        | Error e -> Error e
        | Ok stat -> Ok stat.st_kind
      method lkind : (_,[`Exn of exn]) Vresult.t = match self#lstat with
        | Error e -> Error e
        | Ok stat -> Ok stat.st_kind
      method is_dir = self#kind = Ok S_DIR
      method is_ldir = self#lkind = Ok S_DIR
      method is_reg = self#kind = Ok S_REG
      method dev_inode : (_,[`Exn of exn]) Vresult.t = match self#stat with
      | Ok stat -> Ok (stat.st_dev, stat.st_ino)
      | Error e -> Error e
    end

  let prune () = raise Prune
  
  let find ?(follow_symlink=false) ~f fnames =
  
    (* visited cache *)
    let visited = ref Inodes.empty in
    let if_not_visited_then path ~f = match path#dev_inode with
      | Error _ -> ()
      | Ok inode ->
          if Inodes.mem inode !visited then ()
          else begin
            visited := Inodes.add inode !visited;
            f path
          end
    in
  
    let rec find_dir pth =
      try 
        f pth;
        let subdirs =
          fold_dir pth#path [] & fun dirs -> function
            | "." | ".." -> dirs
            | name -> 
        	let pth = new path_ ~depth:(pth#depth + 1) ~dir:pth#path ~base:name in
        	if try if follow_symlink then pth#is_dir else pth#is_ldir with _ -> false then pth::dirs
        	else begin find_non_dir pth; dirs end
        in
        List.iter (if_not_visited_then ~f:find_dir) subdirs
      with
      | Prune -> ()
  
    and find_non_dir path = try f path with Prune -> ()
      (* Even if path is a dangling symlink, f path is called *)
      (* CR jfuruse: if the initial argument contains non existent files,
         they reach here. *)
    in
  
    List.iter (fun fname ->
      let path = 
        new path_ ~depth: 0 ~dir:(Filename.dirname fname) ~base:(Filename.basename fname)
      in
      if path#is_dir then find_dir path
      else find_non_dir path) fnames

  let fold ?(follow_symlink=false) fnames init f =
  
    let visited = ref Inodes.empty in

    let if_not_visited_then path st f = match path#dev_inode with
      | Error _ -> `Continue, st (* Ignore the error *)
      | Ok inode ->
          if Inodes.mem inode !visited then `Continue, st
          else begin
            visited := Inodes.add inode !visited;
            f st path
          end
    in

    let split_non_dirs_and_dirs pths =
      flip List.partition pths & fun pth ->
        not & 
          try 
            if follow_symlink then pth#is_dir else pth#is_ldir 
          with _ -> false
    in

    let get_dir pth =
      fold_dir pth#path [] & fun pths -> function
        | "." | ".." -> pths
        | name -> 
            let pth = new path_ ~depth:(pth#depth + 1) ~dir:pth#path ~base:name in
            pth :: pths
    in

    let rec loop pths st =
      let nondirs, dirs = split_non_dirs_and_dirs pths in
      let rec loop g st = function
        | [] -> `Continue, st
        | x::xs ->
            match g st x with
            | `Continue, st -> loop g st xs
            | (`Exit, _ as res) -> res
      in
      match loop find_non_dir st nondirs with
      | `Continue, st -> loop find_dir st dirs
      | (`Exit, _ as res) -> res
      
    and find_non_dir st pth = match if_not_visited_then pth st f with
      | (`Continue | `Prune), st -> `Continue, st (* /bin/find -prune is only meaningful aginst directories *)
      | (`Exit, _ as res) -> res

    and find_dir st pth = match if_not_visited_then pth st f with
      | (`Exit, _ as res) -> res
      | `Prune, st -> `Continue, st
      | `Continue, st -> loop (get_dir pth) st
    in

    let pths = flip List.map fnames & fun fname -> 
      new path_ ~depth: 0 ~dir:(Filename.dirname fname) ~base:(Filename.basename fname)
    in
    
    snd & loop pths init
    
  let files ?follow_symlink dirs =
    with_ref_ [] (fun xs -> 
      find ?follow_symlink ~f:(fun p -> xs := p :: !xs) dirs)
end

let try_set_close_on_exec fd =
  try set_close_on_exec fd; true with Invalid_argument _ -> false

    
let open_proc_full cmdargs input output error toclose =
  let cmd = match cmdargs with
    | x :: _ -> x
    | _ -> invalid_arg "Xunix.gen_open_proc_full"
  in
  let cmdargs = Array.of_list cmdargs in
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match fork() with
    0 ->
      dup2 input stdin; close input;
      dup2 output stdout; close output;
      dup2 error stderr; close error;
      if not cloexec then List.iter close toclose;
      begin try execvp cmd cmdargs with _ -> exit 127
      end (* never return *)
  | id -> id


let open_process_full cmdargs =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let (err_read, err_write) = pipe() in
  let pid = open_proc_full cmdargs
    out_read in_write err_write [in_read; out_write; err_read]
  in
  close out_read;
  close in_write;
  close err_write;
  pid, (in_read, out_write, err_read)

    
let open_shell_process_full cmd = open_process_full [ "/bin/sh"; "-c"; cmd ]

let rec waitpid_non_intr pid =
  try 
    waitpid [] pid 
  with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

module CommandDeprecated = struct

  type 'a result = Unix.process_status * 'a

  let fail ?name = 
    let name = match name with None -> "" | Some n -> n ^ ": " in
    function
      | WEXITED n  , _ -> Exn.failwithf "%sprocess exited with id %d" name n
      | WSIGNALED n, _ -> Exn.failwithf "%sprocess killed by signal %d" name n
      | WSTOPPED n , _ -> Exn.failwithf "%sprocess stopped by signal %d" name n

  let should_exit_with n = function
    | (WEXITED m, res) when n = m -> Ok res
    | r -> Error r

  let must_exit_with ?name n = function
    | (WEXITED m, res) when n = m -> res
    | r -> fail ?name r

  let from_exit ?name = function
    | WEXITED m, r -> m, r
    | e -> fail ?name e

  let buf_flush_limit = 100000
  
  let command_aux readers stat =
    let read_buflen = 4096 in
    let read_buf = Bytes.create read_buflen in
  
    let try_read_lines fd buf : (string list * bool (* eof *)) =
      let read_bytes = 
        try Some (read fd read_buf 0 read_buflen) with
        | Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> None
      in
      match read_bytes with
      | None -> [], false
      | Some 0 -> (* eof *)
          let s = Buffer.contents buf in
          (if s = "" then [] else [s]), true
      | Some len ->
          let buffer_old_len = Buffer.length buf in
          Buffer.add_subbytes buf read_buf 0 len;
  
          let pos_in_buffer pos = buffer_old_len + pos in
          
          let rec get_lines st from_in_buffer pos =  
            match
              if pos >= len then None
              else Xbytes.index_from_to read_buf pos (len-1) '\n'
            with
            | None ->
                let rem =
                  Buffer.sub buf
                    from_in_buffer
                    (Buffer.length buf - from_in_buffer)
                in
                Buffer.clear buf;
                if String.length rem > buf_flush_limit then rem :: st
                else begin
                  Buffer.add_string buf rem; st
                end
            | Some pos ->
                let next_from_in_buffer = pos_in_buffer pos + 1 in
                let line =
                  Buffer.sub buf
                    from_in_buffer
                    (next_from_in_buffer - from_in_buffer)
                in
                get_lines (line :: st) next_from_in_buffer (pos + 1)
          in
          List.rev (get_lines [] 0 0), false
    in
  
    let rec loop readers stat =
      if readers = [] then stat (* no more reader and no need to loop *)
      else begin
        let fds = List.map (fun (fd, _, _) -> fd) readers in 
        let readables, _, _ = select fds [] [](*?*) (-1.0)(*?*) in
        let readers', stat = 
          List.fold_right (fun (fd, buf, fs as reader) (st, stat) ->
            if not (List.mem fd readables) then
              (reader :: st, stat)
            else begin
              let rec loop stat =
                let lines, is_eof = try_read_lines fd buf in
                if lines <> [] then begin
                  let stat = 
                    List.fold_left (fun stat line ->
                      List.fold_left (fun stat f -> f stat (`Read line)) stat fs) stat lines
                  in
                  if not is_eof then loop stat else is_eof, stat
                end else is_eof, stat 
              in
              match loop stat with
              | true (*eof*), stat ->
  	        (* reached eof. remove the reader *)
  	        let stat = List.fold_left (fun stat f -> f stat `EOF) stat fs in
                  close fd; 
  	        st, stat
              | false, stat -> reader :: st, stat
            end) readers ([], stat)
        in
        loop readers' stat
      end
    in
    loop readers stat


  let command_wrapper (pid, (out, in_, err)) ~init:stat ~f =
    try
      close in_;
      set_nonblock out;
      set_nonblock err;
      
      let buf_out = Buffer.create buf_flush_limit in
      let buf_err = Buffer.create buf_flush_limit in
  
      let stat = command_aux
        [out, buf_out, [fun stat s -> f stat (`Out, s)];
         err, buf_err, [fun stat s -> f stat (`Err, s)]] stat 
      in
      snd & waitpid_non_intr pid, 
      stat
    with
    | e ->
        (* kill really ? *)
        kill pid 9;
        ignore (waitpid_non_intr pid);
        raise e
  
  type 'st t = init:'st -> f:('st -> [`Out | `Err] * [ `Read of string | `EOF ] -> 'st) -> Unix.process_status * 'st 
  
  let execvp cmd = command_wrapper (open_process_full cmd)
  let shell cmd = command_wrapper (open_shell_process_full cmd)
  
  let fold com = com
  let iter (com : _ t) ~f = com ~init:() ~f:(fun () i -> f i)

  let print ?prefix com = 
    let with_prefix s = 
      let s = Xstring.chop_eols s in
      match prefix with
      | None -> s
      | Some p -> p ^ ": " ^ s
    in
    iter com ~f:(function
    | `Err, `Read s -> prerr_endline & with_prefix s
    | `Out, `Read s -> print_endline & with_prefix s
    | _ -> ())
  
  let ignore_output com = iter com ~f:(fun _ -> ())
  
  let get_stdout com = 
    let pst, rev =
      com ~init:[] ~f:(fun rev -> function
        | `Err, `Read s -> prerr_endline & Xstring.chop_eols s; rev
        | `Out, `Read s -> s :: rev
        | _ -> rev)
    in
    pst, List.rev rev

  let get_all com = 
    let pst, rev =
      com ~init:[] ~f:(fun rev -> function
        | _, `Read s -> s :: rev
        | _ -> rev)
    in
    pst, List.rev rev
end

let gen_timed get minus f v = 
  let t1 = get () in
  let res = f v  in
  let t2 = get () in
  res, minus t2 t1

let timed f v = gen_timed Unix.gettimeofday (-.) f v

module Process_times = struct
  type t = process_times
  let (-) pt1 pt2 = {
    tms_utime = pt1.tms_utime  -. pt2.tms_utime;
    tms_stime = pt1.tms_stime  -. pt2.tms_stime;
    tms_cutime = pt1.tms_utime -. pt2.tms_cutime;
    tms_cstime = pt1.tms_utime -. pt2.tms_cstime;
  }
  let timed f v = gen_timed Unix.times (-) f v
end

let rec mkdir ?(perm=0o700) ?(recursive=false) s =
  match File.Test._d' s with
  | Error ENOENT -> 
      begin match 
          if recursive then begin
            match s with
            | "." | "/" -> Ok ()
            | _ ->
                begin match mkdir ~perm ~recursive (Filename.dirname s) with
                | Ok () | Error (_, `Already_exists _) -> Ok ()
                | err -> err
                end
          end else Ok ()
        with
        | Ok () ->
            begin try
              Unix.mkdir s perm; (* CR jfuruse: use umask? *)
	     Ok ()
            with
            | Unix_error (e,_,_) -> Error (s, `Unix e)
            end
        | err -> err
      end
  | Ok (`TRUE st) -> Error (s, `Already_exists st) (* CR jfuruse: perm check ? *)
  | Ok (`FALSE st) -> Error (s, `Not_a_directory st)
  | Error e -> Error (s, `Unix e)


let mkdtemp template =
  match Xstring.is_postfix' "XXXXXX" template with
  | None -> 
      Exn.invalid_argf "Unix.mkdtemp must take an argument whose postfix is \"XXXXXX\""
  | Some prefix ->
      let rec find () =
        let d = !% "%s%06d" prefix & Random.int 1000000 in
        if Sys.file_exists d then find ()
        else d
      in
      let d = find () in
      Unix.mkdir d 0o700;
      d

let with_dtemp template f =
  let d = mkdtemp template in
  Exn.protect f d ~finally:(fun _ ->
    if ksprintf Sys.command "/bin/rm -rf %s" d <> 0 then
      Exn.failwithf "Unix.with_dtemp: cleaning tempdir %s failed" d)
  
let with_chdir ?(at_failure=(fun exn -> raise exn)) dir f =
  let cwd = Unix.getcwd () in
  match Exn.catch Unix.chdir dir with
  | Error (`Exn exn) -> at_failure exn
  | Ok () ->
      Exn.protect f () ~finally:(fun () -> Unix.chdir cwd)
    
let timed_message mes f v =
  prerr_endline (mes ^ "...");
  let res, secs = timed (Exn.catch f) v in
  match res with
  | Ok v -> 
      !!% "%s: done (%.1f secs)@." mes secs;
      v
  | Error (`Exn e) ->
      !!% "%s: raised an exception (%.1f secs)@." mes secs;
      raise e
      
module Stdlib = struct
  let timed_message = timed_message
end
