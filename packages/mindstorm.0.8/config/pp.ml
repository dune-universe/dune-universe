(* Generate interfaces for standard IO and Lwt. *)

#load "str.cma";;
#load "unix.cma";;

let read_all fname =
  (* Avoid Bytes for backward compatibility. *)
  let fh = open_in fname in
  let len = in_channel_length fh in
  let b = Buffer.create len in
  Buffer.add_channel b fh len;
  Buffer.contents b

let write fname txt =
  (try Unix.chmod fname 0o666; Unix.unlink fname with _ -> ());
  let fh = open_out fname in
  output_string fh txt;
  close_out fh;
  (try Unix.chmod fname 0o466
   with Unix.Unix_error(e, _, _) ->
     prerr_endline("Warning: chmod " ^ fname ^ ": " ^ Unix.error_message e))

let substitute fname_in ~dir fname_out tr =
  if Sys.file_exists fname_in && Sys.file_exists dir then (
    let txt = read_all fname_in in
    let txt = List.fold_left (fun t (re, s) ->
                  Str.global_replace (Str.regexp re) s t) txt tr in
    write (Filename.concat dir fname_out) txt
  )

let () =
  let pp = Filename.concat "src" "mindstorm__NXT.mli.pp" in
  substitute pp ~dir:"src" "mindstorm__NXT.mli"
    [" +LWT_t", "";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", "\\2";
     "MINDSTORM\\.NXT", "Mindstorm.NXT";
    ];
  substitute pp ~dir:"lwt" "mindstorm_lwt__NXT.mli"
    [" +LWT_t", " Lwt.t";
     " +IF_LWT(\\([^(),]*\\),\\([^(),]*\\))", " \\1";
     "MINDSTORM\\.NXT", "Mindstorm_lwt.NXT";
    ]
