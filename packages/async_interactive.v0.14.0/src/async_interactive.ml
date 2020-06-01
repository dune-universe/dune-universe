open Core
open Async
open! Int.Replace_polymorphic_compare

let interactive = ref Core.Unix.(isatty stdin && isatty stdout)

let print_string_internal string =
  if not !interactive
  then return ()
  else (
    print_string string;
    Writer.flushed (force Writer.stdout))
;;

module Job : sig
  val maybe_print_newline : unit -> unit
  val run : f:(unit -> 'a Deferred.t) -> ('r, unit, string, 'a Deferred.t) format4 -> 'r
end = struct
  let needs_a_newline = ref false
  let num_jobs_running = ref 0
  let num_jobs_pending_done_msg = ref 0

  let maybe_print_newline () =
    if !needs_a_newline
    then (
      needs_a_newline := false;
      if !interactive then print_string "\n")
  ;;

  let start str =
    incr num_jobs_running;
    maybe_print_newline ();
    needs_a_newline := true;
    print_string_internal (str ^ " ... ")
  ;;

  let finish () =
    num_jobs_running := max 0 (pred !num_jobs_running);
    if !num_jobs_running > 0
    then (
      incr num_jobs_pending_done_msg;
      Deferred.unit)
    else (
      let plural = !num_jobs_pending_done_msg > 1 in
      num_jobs_pending_done_msg := 0;
      if plural
      then (
        maybe_print_newline ();
        print_string_internal "all done.\n")
      else (
        needs_a_newline := false;
        print_string_internal "done.\n"))
  ;;

  let run ~f fmt =
    let k str =
      start str >>= fun job -> Monitor.protect f ~finally:(fun () -> finish job)
    in
    ksprintf k fmt
  ;;
end

let print_string string =
  if not !interactive
  then return ()
  else (
    Job.maybe_print_newline ();
    print_string string;
    Writer.flushed (force Writer.stdout))
;;

let print_endline string = print_string (sprintf "%s\n" string)
let printf fmt = ksprintf print_string fmt

let prints message a sexp_of_a =
  print_endline ((message, a) |> [%sexp_of: string * a] |> Sexp.to_string_hum)
;;

let print_s sexp = print_endline (sexp |> Sexp.to_string_hum)
let read_line () = Reader.read_line (force Reader.stdin)

let read_char () =
  read_line ()
  >>| function
  | `Eof -> failwith "Received EOF.  Exiting..."
  | `Ok str ->
    let len = String.length str in
    if len = 1
    then Ok (Some (Char.lowercase str.[0]))
    else if len = 0
    then Ok None
    else Error "Expected only one character."
;;

module Choice = struct
  type 'a t = char * 'a * string

  let create char a string = Char.lowercase char, a, string
  let default (char, a, string) = Char.uppercase char, a, string
end

let choose_dispatch (type a) ~(dispatch : (char * a) list)
  : char option -> (a, unit) Result.t Deferred.t
  = function
    | None ->
      (match List.filter dispatch ~f:(fun (c, _) -> Char.is_uppercase c) with
       | _ :: _ :: _ as l ->
         raise_s
           [%sexp
             "[Async_interactive.choose_dispatch] supplied multiple defaults"
           , (List.map l ~f:fst : char list)]
       | [ (_, a) ] -> return (Ok a)
       | [] -> printf "Invalid empty reply.\n" >>| fun () -> Error ())
    | Some ch ->
      let filter (reply, _) = Char.equal (Char.lowercase reply) (Char.lowercase ch) in
      (match List.find dispatch ~f:filter with
       | Some (_, a) -> return (Ok a)
       | None -> printf "Invalid reply [%c]\n" ch >>| fun () -> Error ())
;;

let ask_dispatch_gen ~f question =
  let rec loop () =
    printf "%s: " question
    >>= fun () ->
    read_line ()
    >>= function
    | `Eof -> failwith "Received EOF.  Exiting..."
    | `Ok line ->
      (match f line with
       | Ok res -> return res
       | Error msg -> printf "%s\n" msg >>= fun () -> loop ())
  in
  loop ()
;;

let ask_dispatch (type a) ?(show_options = true) question (dispatch : (char * a) list) =
  let prompt =
    if not show_options
    then question
    else (
      let cs = List.map dispatch ~f:(fun (c, _) -> Char.to_string c) in
      sprintf "%s [%s]" question (String.concat ~sep:"/" cs))
  in
  let rec loop () =
    printf "%s: " prompt
    >>= fun () ->
    read_char ()
    >>= function
    | Error s -> printf "Error: %s\n" s >>= fun () -> loop ()
    | Ok char ->
      choose_dispatch ~dispatch char
      >>= (function
        | Error () -> loop ()
        | Ok res -> return res)
  in
  loop ()
;;

let ask_dispatch_with_help ?show_options ?(show_help = false) question dispatch =
  let cr_dispatch = List.map dispatch ~f:(fun (char, value, _help) -> char, `Ok value) in
  let cr_dispatch = cr_dispatch @ [ '?', `Help ] in
  let print_help () =
    List.iter dispatch ~f:(fun (char, _value, help) ->
      don't_wait_for (printf "%c : %s\n" char help));
    don't_wait_for (printf "? : Print this help\n")
  in
  if show_help then print_help ();
  let rec loop () =
    ask_dispatch question cr_dispatch ?show_options
    >>= function
    | `Ok value -> return value
    | `Help ->
      print_help ();
      loop ()
  in
  loop ()
;;

let ask_yn ?default question =
  let y, n =
    match default with
    | None -> 'y', 'n'
    | Some true -> 'Y', 'n'
    | Some false -> 'y', 'N'
  in
  ask_dispatch question [ y, true; n, false ]
;;

let print ?red msg =
  (* One may be tempted to use Console.printf `Red ... but that's a non-async
     printf. We don't use Console.Ansi.string_with_attr because
     it's not in the base projection. *)
  if Option.is_some red then printf "[1;31m%s[0m\n" msg else printf "%s\n" msg
;;

let arithmetic_challenge_exn ?red () =
  Random.self_init ();
  let a = 1 + Random.int 20 in
  let b = 1 + Random.int 10 in
  let c = 1 + Random.int b in
  let d = (a + b) mod c in
  print ?red (sprintf "What is (%d + %d) mod %d?" a b c)
  >>= fun () ->
  read_line ()
  >>| function
  | `Eof -> failwith "Received Eof while waiting for arithmetic challenge"
  | `Ok line ->
    if d <> Int.of_string line then failwith "Incorrect answer for arithmetic challenge"
;;

let ask_ynf ?default question = Printf.ksprintf (ask_yn ?default) question

let get_pager ?pager () =
  match pager with
  | Some pager -> pager
  | None ->
    (match Sys.getenv "PAGER" with
     (* Make sure -R is passed to 'less' *)
     | Some p when String.is_prefix p ~prefix:"less" -> p ^ " -R"
     | Some p -> p
     | None -> "less -R")
;;

let run_with_pager ?pager ~cmd ~stdin () =
  let pager = get_pager ?pager () in
  let full_cmd = sprintf "%s | %s" cmd pager in
  Monitor.protect
    (fun () ->
       let pid =
         Spawn.spawn
           ~prog:"/bin/sh"
           ~argv:[ "sh"; "-c"; full_cmd ]
           ?stdin:
             (match stdin with
              | Some fd -> Some (Fd.file_descr_exn fd)
              | None -> None)
           ()
       in
       let%bind () =
         match stdin with
         | None -> return ()
         | Some fd -> Fd.close fd
       in
       match%map Unix.waitpid (Pid.of_int pid) with
       | Ok () -> ()
       (* 141 is how bash reports that its child (the pager) died of SIGPIPE.
          This can happen if the program is run non-interactively and its output is
          only partially consumed. We saw this in tests where we do things like
          [cmd ... | grep -q foo]. *)
       | Error (`Exit_non_zero 141) -> ()
       | _ as status ->
         raise_s [%message "command failed" full_cmd (status : Unix.Exit_or_signal.t)])
    ~finally:(fun () -> Deferred.unit)
;;

let show_file ?pager ?msg ~file () =
  let q = Filename.quote in
  let cmd =
    match msg with
    | None -> sprintf "cat %s" (q file)
    | Some msg -> sprintf "{ echo %s; cat %s; }" (q msg) (q file)
  in
  run_with_pager ?pager ~cmd ~stdin:None ()
;;

let show_string_with_pager ?pager contents =
  let%bind filename =
    In_thread.run (fun () ->
      Core.Filename.temp_file "async_interactive_show_string_with_pager" "")
  in
  Monitor.protect
    (fun () ->
       let%bind () = Writer.save filename ~contents in
       show_file ?pager ~file:filename ())
    ~finally:(fun () -> Unix.unlink filename)
;;

let all_wait_errors_unit fs =
  let%bind results =
    Deferred.all (List.map fs ~f:(Monitor.try_with ~extract_exn:true))
  in
  List.map results ~f:Or_error.of_exn_result
  |> Or_error.combine_errors_unit
  |> ok_exn
  |> return
;;

let with_writer_to_pager ?pager () ~f =
  let info = Info.of_string "Async_interactive.with_writer_to_pager" in
  let%bind `Reader pipe_r, `Writer pipe_w = Unix.pipe info in
  let writer =
    (* Setting these two flags has the same effect as
       [Writer.behave_nicely_in_pipeline], apart from it does not initiate shutdown
       when [pager] quits. *)
    Writer.create pipe_w ~raise_when_consumer_leaves:false ~buffer_age_limit:`Unlimited
  in
  (* [all_wait_errors_unit] ensures that we don't proceed with the error handling before
     we're done with the process, but also we don't leave the [f] running in the
     background if the user exits the pager early. *)
  all_wait_errors_unit
    [ (fun () ->
        Monitor.protect (fun () -> f writer) ~finally:(fun () -> Writer.close writer))
    ; (fun () -> run_with_pager ?pager ~cmd:"cat" ~stdin:(Some pipe_r) ())
    ]
;;
