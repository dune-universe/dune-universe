
module CLI = Minicli.CLI
module Fn = Filename
module Log = Dolog.Log

open Printf

let rec read_one_block
    (buff: Buffer.t)
    (block_end: string -> bool)
    (input: in_channel): string =
  try
    let line = input_line input in
    if block_end line && Buffer.length buff > 0 then
      (* the previous block ends just before this line *)
      let res = Buffer.contents buff in
      Buffer.clear buff;
      Buffer.add_string buff line;
      Buffer.add_char buff '\n'; (* add back line terminator *)
      res
    else
      begin (* add to block under construction *)
        Buffer.add_string buff line;
        Buffer.add_char buff '\n'; (* add back line terminator *)
        read_one_block buff block_end input
      end
  with End_of_file ->
    if Buffer.length buff > 0 then
      let res = Buffer.contents buff in (* last block read *)
      Buffer.reset buff;
      res
    else
      raise End_of_file (* no more blocks *)

let read_buff = ref (Bytes.create 0)
let read_fd = ref (Unix.descr_of_in_channel stdin)

let read_some work_dir input_ext buff count csize input demux () =
  let read = ref 0 in
  let job_dir = sprintf "%s/%d" work_dir !count in
  Unix.mkdir job_dir 0o700;
  let tmp_fn = sprintf "%s/in%s" job_dir input_ext in
  Utls.with_out_file tmp_fn (fun out ->
      match demux with
      | Demux.Bytes n ->
        begin
          try
            if !count = 0 then
              begin
                (* only allocate buffer and extract fd once *)
                read_buff := Bytes.create n;
                read_fd := Unix.descr_of_in_channel input
              end;
            for _ = 1 to csize do
              let was_read = Utls.really_read !read_fd !read_buff n in
              incr read;
              output out !read_buff 0 was_read
            done
          with End_of_file -> ()
        end
      | Demux.Line ->
        begin
          try
            for _ = 1 to csize do
              let line = input_line input in
              incr read;
              fprintf out "%s\n" line
            done
          with End_of_file -> ()
        end
      | Demux.Line_sep _
      | Demux.Reg _ ->
        let stop_cond =
          match demux with
          | Line_sep sep -> (fun line -> String.equal sep line)
          | Reg reg -> (fun line -> Str.string_match reg line 0)
          | _ -> assert(false) in
        try
          for _ = 1 to csize do
            let block = read_one_block buff stop_cond input in
            incr read;
            fprintf out "%s" block
          done
        with End_of_file -> ()
    );
  if !read = 0 then
    begin
      Sys.remove tmp_fn;
      raise Parany.End_of_input
    end;
  incr count;
  tmp_fn

(* compute number of chunks in input file *)
let nb_chunks demux fn =
  match demux with
  | Demux.Bytes n -> Utls.ceil ((float (Utls.file_size fn)) /. (float n))
  | Demux.Line -> Utls.file_nb_lines fn
  | Demux.Line_sep l -> Utls.file_count_matching_lines l fn
  | Demux.Reg r ->
    let count = ref 0 in
    Utls.iter_on_lines_of_file fn (fun line ->
        if Str.string_match r line 0 then
          incr count
      );
    !count

let input_fn_tag = Str.regexp "%IN"
let output_fn_tag = Str.regexp "%OUT"

let process_some output_ext cmd tmp_in_fn =
  let job_dir = Fn.dirname tmp_in_fn in
  Unix.chdir job_dir;
  assert(Utls.regexp_in_string input_fn_tag cmd);
  let cmd' = Str.replace_first input_fn_tag tmp_in_fn cmd in
  let tmp_out_fn = sprintf "%s/out%s" job_dir output_ext in
  assert(Utls.regexp_in_string output_fn_tag cmd');
  let cmd'' = Str.replace_first output_fn_tag tmp_out_fn cmd' in
  let cmd''' = sprintf "%s; rm -f %s" cmd'' tmp_in_fn in
  Utls.run_command !Flags.debug cmd''';
  tmp_out_fn

let gather_some total_items start_t mux_count mux_mode tmp_out_fn =
  begin
    match mux_mode with
    | Mux.Null -> () (* FBR: tmp_out_fn is not removed? *)
    | Mux.Cat_into dst_fn ->
      begin
        let cmd =
          if !mux_count = 0 then
            sprintf "mv %s %s" tmp_out_fn dst_fn
          else
            sprintf "cat %s >> %s; rm -f %s" tmp_out_fn dst_fn tmp_out_fn in
        Utls.run_command !Flags.debug cmd;
        incr mux_count
      end
  end;
  (* user feedback *)
  let elapsed_t = Unix.gettimeofday () -. start_t in
  if total_items = 0 then
    printf "done: %d freq: %.1f\r%!"
      !mux_count ((float !mux_count) /. elapsed_t)
  else
  if !mux_count <> total_items then
    let completed_fraction = (float !mux_count) /. (float total_items) in
    let estimated_total = elapsed_t /. completed_fraction in
    let estimated_remaining = estimated_total -. elapsed_t in
    let eta_hms = Utls.hms_of_seconds estimated_remaining in
    let eta_str = Utls.string_of_hms eta_hms in
    printf "done: %.2f%% ETA: %s\r%!"
      (100. *. completed_fraction) eta_str
  else
    (* "\027[2K": ANSI escape code to clear current line *)
    printf "\027[2Kdone: 100%%\n%!"

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let start_t = Unix.gettimeofday () in
  let argc, args = CLI.init () in
  let show_help = CLI.get_set_bool ["-h";"--help"] args in
  if argc = 1 || show_help then
    (eprintf "usage:\n\
              %s ...\n  \
              {-i|--input} <file>: where to read from\n  \
              {-o|--output} <file>: where to write to (default=stdout)\n  \
              [{-n|--nprocs} <int>]: max jobs in parallel \
              (default=all cores)\n  \
              [{-c|--chunks} <int>]: how many chunks per job (default=1)\n  \
              [{-d|--demux} {l|b:<int>|r:<regexp>|s:<string>}]: \
              how to cut input \n  \
              file into chunks (line/bytes/regexp/sep_line; default=line)\n  \
              {-w|--work} <string>: command to execute on each chunk\n  \
              %%IN and %%OUT are special tokens\n  \
              [{-m|--mux} {c|n}]: how to mux job results in output file\n  \
              (cat/null; default=cat)\n  \
              [{-ie|--input-ext} <string>]: append file extension to work \
              input files\n  \
              [{-oe|--output-ext} <string>]: append file extension to work \
              output files\n  \
              [{-p|--preserve}]: mux results while preserving input order\n"
       Sys.argv.(0);
     exit 1);
  Flags.debug := CLI.get_set_bool ["-v";"--verbose"] args;
  let input_fn = CLI.get_string ["-i";"--input"] args in
  let in_chan = open_in input_fn in
  let input_ext = CLI.get_string_def ["-ie";"--input-ext"] args "" in
  let output_ext = CLI.get_string_def ["-oe";"--output-ext"] args "" in
  let out_fn =
    (* must be converted to an absolute filename,
       since each job is chdir to a separate directory *)
    let fn = CLI.get_string_def ["-o";"--output"] args "/dev/stdout" in
    Utls.absolute_filename fn in
  let cmd = CLI.get_string ["-w";"--work"] args in
  let nprocs = match CLI.get_int_opt ["-n";"--nprocs"] args with
    | None -> Utls.get_nprocs ()
    | Some n -> n in
  let csize = match CLI.get_int_opt ["-c";"--chunks"] args with
    | None -> 1
    | Some n -> n in
  let demux =
    let demux_str = CLI.get_string_def ["-d";"--demux"] args "l" in
    Demux.of_string demux_str in
  let mux =
    let mux_str = CLI.get_string_def ["-m";"--mux"] args "c" in
    Mux.of_string out_fn mux_str in
  let preserve = CLI.get_set_bool ["-p";"--preserve"] args in
  CLI.finalize ();
  Log.info "computing input file #chunks...";
  let total_items = nb_chunks demux input_fn in
  Log.info "%d" total_items;
  let nb_chunks = Utls.ceil ((float total_items) /. (float csize)) in
  let work_dir = Utls.get_command_output !Flags.debug "mktemp -d -t pardi_XXXX" in
  Log.info "work_dir: %s" work_dir;
  (* because read_some takes care of the number of chunks per job
     !!! PARANY MUST USE A CSIZE OF ONE !!! *)
  Parany.run ~preserve ~csize:1 nprocs
    ~demux:(read_some work_dir input_ext
              (Buffer.create 1024) (ref 0) csize in_chan demux)
    ~work:(process_some output_ext cmd)
    ~mux:(gather_some nb_chunks start_t (ref 0) mux);
  if not !Flags.debug then
    Utls.run_command !Flags.debug (sprintf "rm -rf %s" work_dir);
  close_in in_chan

let () = main ()
