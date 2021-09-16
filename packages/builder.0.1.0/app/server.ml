open Lwt.Infix

let read fd =
  Lwt.catch (fun () ->
      let rec r b ?(off = 0) l =
        if l = 0 then
          Lwt.return (Ok ())
        else
          Lwt_unix.read fd b off l >>= fun read ->
          if read = 0 then
            Lwt.return (Error (`Msg "end of file"))
          else
            r b ~off:(read + off) (l - read)
      in
      let open Lwt_result.Infix in
      let bl = Bytes.create 8 in
      r bl 8 >>= fun () ->
      let l = Cstruct.BE.get_uint64 (Cstruct.of_bytes bl) 0 in
      let l_int = Int64.to_int l in (* TODO *)
      let b = Bytes.create l_int in
      r b l_int >|= fun () ->
      Cstruct.of_bytes b)
    (fun e ->
       Logs.err (fun m -> m "Error while reading: %s" (Printexc.to_string e));
       Lwt.return (Error (`Msg "error in read")))

let read_cmd fd =
  let open Lwt_result.Infix in
  read fd >>= fun data ->
  Lwt.return (Builder.Asn.cmd_of_cs data)

let write fd data =
  Lwt.catch (fun () ->
      let rec w b ?(off = 0) l =
        if l = 0 then
          Lwt.return_unit
        else
          Lwt_unix.write fd b off l >>= fun written ->
          w b ~off:(written + off) (l - written)
      in
      let csl = Cstruct.create 8 in
      Cstruct.BE.set_uint64 csl 0 (Int64.of_int (Cstruct.length data));
      w (Cstruct.to_bytes csl) 8 >>= fun () ->
      w (Cstruct.to_bytes data) (Cstruct.length data) >|= fun () ->
      Ok ())
    (fun e ->
       Logs.err (fun m -> m "Error while writing: %s" (Printexc.to_string e));
       Lwt.return (Error (`Msg "unix error in write")))

let write_cmd fd cmd =
  let data = Builder.Asn.cmd_to_cs cmd in
  write fd data

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX s -> Fmt.pf ppf "unix://%s" s
  | Lwt_unix.ADDR_INET (ip, port) ->
    Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr ip) port

module UM = Map.Make(Uuidm)

module S = Binary_heap.Make (struct
    type t = Builder.schedule_item
    let compare { Builder.next = n1 ; _ } { Builder.next = n2 ; _ } =
      Ptime.compare n1 n2
  end)

let dummy =
  let job = Builder.(Script_job { name = "dummy" ; script = "#nothing" }) in
  Builder.{ next = Ptime.epoch ; period = Daily ; job }

type t = {
  mutable queue : Builder.job Queue.t ;
  mutable schedule : S.t ;
  mutable running : (Ptime.t * Builder.script_job * string Lwt_condition.t * (int64 * string) list) UM.t ;
  waiter : unit Lwt_condition.t ;
  dir : Fpath.t ;
  upload : string option ;
}

let p_to_span p =
  let one_hour = 60 * 60 in
  let s = match p with
    | Builder.Hourly -> one_hour
    | Builder.Daily -> 24 * one_hour
    | Builder.Weekly -> 7 * 24 * one_hour
  in
  Ptime.Span.of_int_s s

let add_to_queue t job =
  if Queue.fold (fun acc i ->
      if acc then not (Builder.job_equal i job) else acc)
      true t.queue
  then begin
    Queue.add job t.queue;
    Lwt_condition.broadcast t.waiter ();
  end

let schedule_job t now period job =
  add_to_queue t job;
  match Ptime.add_span now (p_to_span period) with
  | None -> Logs.err (fun m -> m "ptime add span failed when scheduling job")
  | Some next -> S.add t.schedule Builder.{ next ; period ; job }

let schedule t =
  let now = Ptime_clock.now () in
  let rec s_next modified =
    match S.minimum t.schedule with
    | exception Binary_heap.Empty -> modified
    | Builder.{ next ; period ; job } when Ptime.is_later ~than:next now ->
      S.remove t.schedule;
      schedule_job t now period job;
      s_next true
    | _ -> modified
  in
  s_next false

let dump, restore =
  let open Rresult.R.Infix in
  let file = "state" in
  (fun t ->
     let state =
       let jobs = Queue.fold (fun acc j -> Builder.Job j :: acc) [] t.queue in
       S.fold (fun s acc -> (Builder.Schedule s) :: acc) t.schedule
         (List.rev jobs)
     in
     let data = Builder.Asn.state_to_cs state in
     let bak = Fpath.(t.dir / file + "tmp") in
     Bos.OS.File.write bak (Cstruct.to_string data) >>= fun () ->
     Bos.OS.U.(error_to_msg (rename bak Fpath.(t.dir / file)))),
  (fun upload dir ->
     Bos.OS.Dir.create dir >>= fun _ ->
     let to_read = Fpath.(dir / file) in
     let queue = Queue.create ()
     and schedule = S.create ~dummy 13
     and waiter = Lwt_condition.create ()
     in
     Bos.OS.File.exists to_read >>= function
     | false ->
       Logs.warn (fun m -> m "state file does not exist, using empty");
       Ok { queue ; schedule ; running = UM.empty ; waiter ; dir ; upload }
     | true ->
       Bos.OS.File.read to_read >>= fun data ->
       Builder.Asn.state_of_cs (Cstruct.of_string data) >>= fun items ->
       let queue, schedule =
         List.fold_left (fun (queue, schedule) -> function
             | Builder.Job j -> Queue.add j queue; (queue, schedule)
             | Builder.Schedule s -> S.add schedule s; (queue, schedule))
           (queue, schedule) items
       in
       Ok { queue ; schedule ; running = UM.empty ; waiter ; dir ; upload })

let uuid_gen = Uuidm.v4_gen (Random.State.make_self_init ())

let save_to_disk dir (((job : Builder.script_job), uuid, _, _, _, _, _) as full) =
  let open Rresult.R.Infix in
  let out_dir = Fpath.(dir / job.Builder.name / Uuidm.to_string uuid) in
  Logs.info (fun m -> m "saving result to %a" Fpath.pp out_dir);
  Bos.OS.Dir.create out_dir >>= fun _ ->
  let full_cs = Builder.Asn.exec_to_cs full in
  Bos.OS.File.write Fpath.(out_dir / "full") (Cstruct.to_string full_cs)

let upload url dir full =
  let body = Cstruct.to_string (Builder.Asn.exec_to_cs full) in
  Http_lwt_client.one_request ~meth:`POST ~body url >|= function
  | Ok (resp, body) ->
    if Http_lwt_client.Status.is_successful resp.Http_lwt_client.status then begin
      Logs.info (fun m -> m "successful upload (HTTP %s)"
                    (Http_lwt_client.Status.to_string resp.Http_lwt_client.status));
      Ok ()
    end else begin
      Logs.err (fun m -> m "upload failed (HTTP %s, body: %s), saving to %a"
                   (Http_lwt_client.Status.to_string resp.Http_lwt_client.status)
                   (match body with None -> "" | Some x -> x) Fpath.pp dir);
      save_to_disk dir full
    end
  | Error `Msg e ->
    Logs.err (fun m -> m "upload failed %s, saving to %a" e Fpath.pp dir);
    save_to_disk dir full

let job_finished state uuid res data =
  let r = UM.find_opt uuid state.running in
  state.running <- UM.remove uuid state.running;
  match r with
  | None ->
    Logs.err (fun m -> m "no job found for uuid %a" Uuidm.pp uuid);
    Lwt.return_unit
  | Some (started, job, cond, out) ->
    let now = Ptime_clock.now () in
    let res_str = Fmt.to_to_string Builder.pp_execution_result res in
    Lwt_condition.broadcast cond res_str;
    let full =
      let out = List.rev_map (fun (d, d') -> Int64.to_int d, d') out in
      let out = List.rev out in
      job, uuid, out, started, now, res, data
    in
    (match state.upload with
     | None -> Lwt.return (save_to_disk state.dir full)
     | Some url -> upload url state.dir full) >|= function
    | Ok () -> ()
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error saving %s (%a) to disk: %s"
                   job.Builder.name Uuidm.pp uuid msg)

let read_template () =
  match
    Bos.OS.File.read (Fpath.v "/etc/builder/orb-build.template"),
    Bos.OS.File.read (Fpath.v "/usr/local/etc/builder/orb-build.template")
  with
  | Ok data, _ -> Ok data
  | _, Ok data -> Ok data
  | Error _ as e, _ ->
    Logs.err (fun m -> m "couldn't read template in /etc/builder or /usr/local/etc/builder");
    e

let job_to_script_job = function
  | Builder.Script_job j -> Ok j
  | Builder.Orb_build_job { name ; opam_package } ->
    match read_template () with
    | Ok template ->
      let script =
        Astring.String.(concat ~sep:opam_package (cuts ~sep:"%%OPAM_PACKAGE%%" template))
      in
      Ok { name ; script }
    | Error _ as e -> e

let reschedule_job t name f =
  let s = S.create ~dummy 13 in
  (* first we look through the schedule to find <name> *)
  match S.fold (fun ({ Builder.job ; period ; next } as si) j ->
      match j, String.equal (Builder.job_name job) name with
      | None, true -> Some (job, period, next)
      | Some _, true ->
        (* violates our policy: there ain't multiple jobs with same name *)
        assert false
      | _, false -> S.add s si; j)
      t.schedule None
  with
  | None ->
    Logs.err (fun m -> m "couldn't find job %s" name);
  | Some (job, period, next) ->
    t.schedule <- s;
    let job, period, next = f job period next in
    schedule_job t next period job;
    ignore (dump t);
    Logs.app (fun m -> m "queued job %s" name)

let handle t fd addr =
  (* -- client connection:
     (1) read client hello (or client hello 2)
     (2) send server hello
     -- now there are different paths:
     (3) read request job
     (4) send job
     (5) await job done
     -- or scheduling a job
     (3) read schedule
     -- or unscheduling a job
     (3) read unschedule
     -- or info
     (3) read info
     (4) send info_reply
     -- or observing
     (3) read observe
     (4) send outputs and job done
  *)
  let open Lwt_result.Infix in
  Logs.app (fun m -> m "client connection from %a" pp_sockaddr addr);
  let maybe_schedule_job p j =
    if S.fold (fun { Builder.job ; _ } acc ->
      if acc then not (Builder.job_equal job j) else acc)
        t.schedule true
      then
        let now = Ptime_clock.now () in
        schedule_job t now p j;
        ignore (dump t);
        Lwt.return (Ok ())
      else begin
        Logs.err (fun m -> m "job with same name already in schedule");
        Lwt.return (Error (`Msg "job name already used"))
      end
  in
  read_cmd fd >>= (function
      | Builder.Client_hello n when n = Builder.cmds ->
        write_cmd fd Builder.(Server_hello cmds)
      | Builder.Client_hello2 (t, n) when
          (t = `Client && n = Builder.client_cmds) ||
          (t = `Worker && n = Builder.worker_cmds) ->
        write_cmd fd Builder.Server_hello2
      | cmd ->
        Logs.err (fun m -> m "expected client hello, got %a"
                     Builder.pp_cmd cmd);
        Lwt_result.lift (Error (`Msg "bad communication"))) >>= fun () ->
  read_cmd fd >>= function
  | Builder.Job_requested ->
    Logs.app (fun m -> m "job requested");
    let rec find_job () =
      match Queue.take_opt t.queue with
      | None -> Lwt.bind (Lwt_condition.wait t.waiter) find_job
      | Some job -> Lwt.return job
    in
    Lwt.bind (find_job ()) (fun job ->
        let put_back_on_err uuid f =
          Lwt_result.bind_lwt_err
            f
            (fun (`Msg err) ->
               Logs.warn (fun m -> m "communication failure %s with %a, job %a put back"
                             err Builder.pp_job job pp_sockaddr addr);
               t.running <- UM.remove uuid t.running;
               add_to_queue t job;
               ignore (dump t);
               Lwt.return (`Msg err))
        in
        ignore (dump t);
        let uuid = uuid_gen () in
        Lwt_result.lift (job_to_script_job job) >>= fun script_job ->
        put_back_on_err uuid (write_cmd fd (Builder.Job_schedule (uuid, script_job))) >>= fun () ->
        Logs.app (fun m -> m "job %a scheduled %a for %a"
                     Uuidm.pp uuid Builder.pp_job job pp_sockaddr addr);
        t.running <- UM.add uuid (Ptime_clock.now (), script_job, Lwt_condition.create (), []) t.running;
        (* await output *)
        let rec read_or_timeout () =
          let timeout () =
            (* an hour should be enough for a job *)
            let open Lwt.Infix in
            let timeout = Duration.(to_f (of_hour 1)) in
            Lwt_unix.sleep timeout >>= fun () ->
            Logs.warn (fun m -> m "%a timeout after %f seconds" Uuidm.pp uuid timeout);
            job_finished t uuid (Builder.Msg "timeout") [] >|= fun () ->
            add_to_queue t job;
            ignore (dump t);
            Ok ()
          in
          let read_and_process_data () =
            put_back_on_err uuid (read_cmd fd) >>= function
            | Builder.Output (uuid, data) ->
              Logs.debug (fun m -> m "job %a output %S" Uuidm.pp uuid data);
              (match UM.find_opt uuid t.running with
               | None ->
                 Logs.err (fun m -> m "unknown %a, discarding %S"
                              Uuidm.pp uuid data)
               | Some (created, job, cond, out) ->
                 Lwt_condition.broadcast cond data;
                 let ts =
                   let delta = Ptime.diff (Ptime_clock.now ()) created in
                   Duration.of_f (Ptime.Span.to_float_s delta)
                 in
                 let value = created, job, cond, (ts, data) :: out in
                 t.running <- UM.add uuid value t.running);
              read_or_timeout ()
            | Builder.Job_finished (uuid, r, data) ->
              Logs.app (fun m -> m "job %a finished with %a" Uuidm.pp uuid
                           Builder.pp_execution_result r);
              Lwt_result.ok (job_finished t uuid r data)
            | cmd ->
              Logs.err (fun m -> m "expected output or job finished, got %a"
                           Builder.pp_cmd cmd);
              read_or_timeout ()
          in
          Lwt.pick [ timeout () ; read_and_process_data () ]
        in
        read_or_timeout ())
  | Builder.Schedule (p, j) ->
    Logs.app (fun m -> m "%a schedule %a" Builder.pp_period p
                 Builder.pp_script_job j);
    maybe_schedule_job p (Builder.Script_job j)
  | Builder.Schedule_orb_build (p, j) ->
    Logs.app (fun m -> m "%a schedule orb build %a" Builder.pp_period p
                 Builder.pp_orb_build_job j);
    maybe_schedule_job p (Builder.Orb_build_job j)
  | Builder.Unschedule name ->
    Logs.app (fun m -> m "unschedule %s" name);
    let schedule =
      let s = S.create ~dummy 13 in
      S.iter (fun ({ Builder.job ; _ } as si) ->
          if not (String.equal (Builder.job_name job) name) then
            S.add s si
          else ()) t.schedule;
      s
    and queue =
      let q = Queue.create () in
      Queue.iter (fun job ->
          if not (String.equal (Builder.job_name job) name) then
            Queue.add job q
          else
            ())
        t.queue;
      q
    in
    t.schedule <- schedule;
    t.queue <- queue;
    ignore (dump t);
    Lwt.return (Ok ())
  | Builder.Execute name ->
    begin
      Logs.app (fun m -> m "execute %s" name);
      reschedule_job t name (fun job period _next ->
          (job, period, (Ptime_clock.now ())));
      Lwt.return (Ok ())
    end
  | Builder.Reschedule (name, next, period) ->
    begin
      Logs.app (fun m -> m "reschedule %s: %a" name (Ptime.pp_rfc3339 ()) next);
      reschedule_job t name (fun job orig_period _orig_next ->
          (job, Option.value ~default:orig_period period, next));
      Lwt.return (Ok ())
    end
  | Builder.Info ->
    Logs.app (fun m -> m "info");
    let reply =
      let schedule = S.fold (fun s acc -> s :: acc) t.schedule []
      and queue = List.rev (Queue.fold (fun acc j -> j :: acc) [] t.queue)
      and running =
        UM.fold (fun uuid (started, job, _, _) acc ->
            (started, uuid, job) :: acc)
          t.running []
      in
      Builder.{ schedule ; queue ; running }
    in
    write_cmd fd (Builder.Info_reply reply)
  | Builder.Observe id ->
    (* two cases: still running or already done *)
    begin match UM.find_opt id t.running with
      | Some (_, _, cond, out) ->
        let open Lwt.Infix in
        Lwt_list.iter_s (fun (_, l) ->
            write_cmd fd (Builder.Output (id, l)) >|= ignore)
          (List.rev out) >>= fun () ->
        let rec more () =
          Lwt_condition.wait cond >>= fun data ->
          write_cmd fd (Builder.Output (id, data)) >>= function
          | Ok () -> more ()
          | Error _ -> Lwt.return (Ok ())
        in
        more ()
      | None ->
        (* TODO figure which job name this may be *)
        (* maybe more useful to get latest result of <job-name>? *)
        Logs.err (fun m -> m "not implemented: job not executing");
        Lwt.return (Ok ())
    end
  | cmd ->
    Logs.err (fun m -> m "unexpected %a" Builder.pp_cmd cmd);
    Lwt_result.lift (Error (`Msg "bad communication"))

let jump () ip port dir url =
  Lwt_main.run
    (Sys.(set_signal sigpipe Signal_ignore);
     let d = Fpath.v dir in
     Lwt_result.lift (restore url d) >>= fun state ->
     (match state with
      | Ok s -> Lwt.return s | Error `Msg m -> Lwt.fail_with m) >>= fun state ->
     let modified = schedule state in
     ignore (if modified then dump state else Ok ());
     let s = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
     Lwt_unix.(setsockopt s SO_REUSEADDR true);
     Lwt_unix.(bind s (ADDR_INET (Unix.inet_addr_of_string ip, port))) >>= fun () ->
     Lwt_unix.listen s 10;
     let _ev =
       Lwt_engine.on_timer 60. true (fun _ev ->
           let modified = schedule state in
           ignore (if modified then dump state else Ok ()))
     in
     Lwt.catch (fun () ->
         let rec loop () =
           Lwt_unix.accept s >>= fun (fd, addr) ->
           Lwt.async (fun () ->
               handle state fd addr >>= fun _ ->
               Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit));
           loop ()
         in
         loop ())
       (fun e ->
          Lwt.return (Logs.err (fun m -> m "exception %s, shutting down"
                                   (Printexc.to_string e)))));
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let port =
  let doc = "TCP listen port" in
  Arg.(value & opt int 1234 & info [ "port" ] ~doc)

let ip =
  let doc = "Listen IP" in
  Arg.(value & opt string "127.0.0.1" & info [ "ip" ] ~doc)

let dir =
  let doc = "Directory for persistent data (defaults to /var/db/builder)" in
  Arg.(value & opt dir "/var/db/builder" & info [ "dir" ] ~doc)

let upload =
  let doc = "Upload artifacts to URL (instead of local storage)" in
  Arg.(value & opt (some string) None & info [ "upload" ] ~doc)

let cmd =
  Term.(term_result (const jump $ setup_log $ ip $ port $ dir $ upload)),
  Term.info "builder-server" ~version:Builder.version

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
