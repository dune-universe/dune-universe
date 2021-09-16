let src = Logs.Src.create "builder" ~doc:"Builder"
module Log = (val Logs.src_log src : Logs.LOG)

open Rresult.R.Infix

type data = (Fpath.t * string) list

let pp_data ppf xs =
  Fmt.(list ~sep:(unit "@.")
         (pair ~sep:(unit ": ") Fpath.pp int))
    ppf
    (List.map (fun (f, s) -> f, String.length s) xs)

type script_job = {
  name : string ;
  script : string ;
}

let pp_script_job ppf { name ; _ } = Fmt.pf ppf "name %s" name

type orb_build_job = {
  name : string ;
  opam_package : string ;
}

let pp_orb_build_job ppf { name ; opam_package } =
  Fmt.pf ppf "name %s, opam %s" name opam_package

type execution_result =
  | Exited of int
  | Signalled of int
  | Stopped of int
  | Msg of string

let pp_execution_result ppf = function
  | Exited i -> Fmt.pf ppf "exited %d" i
  | Signalled i -> Fmt.pf ppf "signalled %d" i
  | Stopped i -> Fmt.pf ppf "stopped %d" i
  | Msg m -> Fmt.pf ppf "execution aborted: %s" m

type period = Hourly | Daily | Weekly

let pp_period ppf = function
  | Hourly -> Fmt.string ppf "hourly"
  | Daily -> Fmt.string ppf "daily"
  | Weekly -> Fmt.string ppf "weekly"

type job =
  | Script_job of script_job
  | Orb_build_job of orb_build_job

let pp_job ppf = function
  | Script_job j -> pp_script_job ppf j
  | Orb_build_job j -> pp_orb_build_job ppf j

 let job_name = function
   | Script_job { name ; _ } -> name
   | Orb_build_job { name ; _ } -> name

let job_equal a b =
   String.equal (job_name a) (job_name b)

type schedule_item = {
  next : Ptime.t ;
  period : period ;
  job : job ;
}

let pp_schedule_item ppf { next ; period ; job } =
  Fmt.pf ppf "%a next %a, scheduled %a" pp_job job
    (Ptime.pp_rfc3339 ()) next
    pp_period period

type info = {
  schedule : schedule_item list ;
  queue : job list ;
  running : (Ptime.t * Uuidm.t * script_job) list ;
}

let triple ~sep pc pb pa ppf (va, vb, vc)=
  Fmt.pair ~sep pc (Fmt.pair ~sep pb pa) ppf
    (vc, (vb, va))

let pp_info ppf { schedule ; queue ; running } =
  let pp_time = Ptime.pp_rfc3339 () in
  Fmt.pf ppf "schedule:@.%a@.queue: %a@.running:@.%a@."
    Fmt.(list ~sep:(unit ";@.") pp_schedule_item) schedule
    Fmt.(list ~sep:(unit ";@ ") pp_job) queue
    Fmt.(list ~sep:(unit ";@.")
           (triple ~sep:(unit ",@,") pp_script_job Uuidm.pp pp_time)) running

type cmd =
  | Client_hello of int
  | Server_hello of int
  | Job_requested (* worker *)
  | Job_schedule of Uuidm.t * script_job (* worker *)
  | Job_finished of Uuidm.t * execution_result * data (* worker *)
  | Output of Uuidm.t * string (* worker and client *)
  | Schedule of period * script_job (* client *)
  | Unschedule of string (* client *)
  | Info (* client *)
  | Info_reply of info (* client *)
  | Observe of Uuidm.t (* client *)
  | Execute of string (* client *)
  | Schedule_orb_build of period * orb_build_job (* client *)
  | Reschedule of string * Ptime.t * period option (* client *)
  | Client_hello2 of [ `Client | `Worker ] * int
  | Server_hello2

let cmds = 14 (* not used anymore, don't update *)
let client_cmds = 9
let worker_cmds = 4

let version =
  Fmt.strf "version v0.1.0 protocol version %d (client %d worker %d)"
    cmds client_cmds worker_cmds

let pp_cmd ppf = function
  | Client_hello max -> Fmt.pf ppf "client hello (max %d)" max
  | Server_hello max -> Fmt.pf ppf "server hello (max %d)" max
  | Job_requested -> Fmt.string ppf "job request"
  | Job_schedule (uuid, job) ->
    Fmt.pf ppf "[%a] job schedule %a" Uuidm.pp uuid pp_script_job job
  | Job_finished (uuid, result, data) ->
    Fmt.pf ppf "[%a] job finished with %a: %a" Uuidm.pp uuid
      pp_execution_result result pp_data data
  | Output (uuid, data) -> Fmt.pf ppf "[%a] %S" Uuidm.pp uuid data
  | Schedule (period, job) ->
    Fmt.pf ppf "schedule at %a: %a" pp_period period pp_script_job job
  | Unschedule job_name -> Fmt.pf ppf "unschedule %s" job_name
  | Info -> Fmt.string ppf "info"
  | Info_reply info -> Fmt.pf ppf "info: %a" pp_info info
  | Observe id -> Fmt.pf ppf "observe %a" Uuidm.pp id
  | Execute name -> Fmt.pf ppf "execute %s" name
  | Schedule_orb_build (period, orb_job) ->
    Fmt.pf ppf "schedule orb build at %a: %a" pp_period period pp_orb_build_job orb_job
  | Reschedule (name, next, None) ->
    Fmt.pf ppf "reschedule %s: %a" name (Ptime.pp_rfc3339 ()) next
  | Reschedule (name, next, Some period) ->
    Fmt.pf ppf "reschedule %s at %a: %a" name pp_period period (Ptime.pp_rfc3339 ()) next
  | Client_hello2 (t, num) ->
    Fmt.pf ppf "client hello2 %s %d"
      (match t with `Client -> "client" | `Worker -> "worker") num
  | Server_hello2 -> Fmt.string ppf "server hello2"

type state_item =
  | Job of job
  | Schedule of schedule_item

let pp_state_item ppf = function
  | Job j -> Fmt.pf ppf "job %a" pp_job j
  | Schedule s -> Fmt.pf ppf "schedule %a" pp_schedule_item s

type state = state_item list

let pp_state = Fmt.(list ~sep:(unit ";@ ") pp_state_item)

module Asn = struct
  let guard p err = if p then Ok () else Error err

  let decode_strict codec cs =
    match Asn.decode codec cs with
    | Ok (a, cs) ->
      guard (Cstruct.length cs = 0) (`Msg "trailing bytes") >>= fun () ->
      Ok a
    | Error (`Parse msg) -> Error (`Msg msg)

  let projections_of asn =
    let c = Asn.codec Asn.der asn in
    (decode_strict c, Asn.encode c)

  let data =
    let f (path, value) =
      match Fpath.of_string path with
      | Ok p -> p, value
      | Error `Msg msg -> Asn.S.error (`Parse msg)
    and g (path, value) =
      Fpath.to_string path, value
    in
    Asn.S.(sequence_of
             (map f g
                (sequence2
                   (required ~label:"path" utf8_string)
                   (required ~label:"data" utf8_string))))

  let script_job =
    let f (name, script, _files) =
      { name ; script }
    and g { name ; script } =
      name, script, []
    in
    Asn.S.(map f g (sequence3
                      (required ~label:"name" utf8_string)
                      (required ~label:"script" utf8_string)
                      (required ~label:"files" data)))

  let orb_build_job =
    let f (name, opam_package) =
      { name ; opam_package }
    and g { name ; opam_package } =
      name, opam_package
    in
    Asn.S.(map f g (sequence2
                      (required ~label:"name" utf8_string)
                      (required ~label:"opam_package" utf8_string)))

  let job =
    let f = function
     | `C1 j -> Script_job j
     | `C2 j -> Orb_build_job j
   and g = function
     | Script_job j -> `C1 j
     | Orb_build_job j -> `C2 j
   in
   Asn.S.(map f g
     (choice2
       (explicit 0 script_job)
       (explicit 1 orb_build_job)))

  let period =
    let f = function
      | `C1 () -> Hourly
      | `C2 () -> Daily
      | `C3 () -> Weekly
    and g = function
      | Hourly -> `C1 ()
      | Daily -> `C2 ()
      | Weekly -> `C3 ()
    in
    Asn.S.(map f g
             (choice3 (explicit 0 null) (explicit 1 null) (explicit 2 null)))

  let old_schedule =
    let f (next, period, job) = {next; period; job = Script_job job}
    and g _ = assert false
    in
    Asn.S.(map f g (sequence3
                      (required ~label:"next" utc_time)
                      (required ~label:"period" period)
                      (required ~label:"job" script_job)))

  let schedule =
    let f (next, period, job) = {next; period; job}
    and g {next; period; job} = (next, period, job)
    in
    Asn.S.(map f g (sequence3
                      (required ~label:"next" utc_time)
                      (required ~label:"period" period)
                      (required ~label:"job" job)))

  let state_item =
    let f = function
      | `C1 j -> Job (Script_job j)
      | `C2 s -> Schedule s
      | `C3 j -> Job j
      | `C4 e -> Schedule e
    and g = function
      | Job j -> `C3 j
      | Schedule s -> `C4 s
    in
    Asn.S.(map f g (choice4
                      (explicit 0 script_job)
                      (explicit 1 old_schedule)
                      (explicit 2 job)
                      (explicit 3 schedule)))

  let state_of_cs, state_to_cs = projections_of (Asn.S.sequence_of state_item)

  let uuid =
    let f s =
      match Uuidm.of_bytes s with
      | None -> Asn.S.error (`Parse "couldn't decode UUID")
      | Some s -> s
    and g uuid = Uuidm.to_bytes uuid
    in
    Asn.S.(map f g utf8_string)

  let res =
    let f = function
      | `C1 i -> Exited i
      | `C2 i -> Signalled i
      | `C3 i -> Stopped i
      | `C4 s -> Msg s
    and g = function
      | Exited i -> `C1 i
      | Signalled i -> `C2 i
      | Stopped i -> `C3 i
      | Msg s -> `C4 s
    in
    Asn.S.(map f g
             (choice4
                (explicit 0 int)
                (explicit 1 int)
                (explicit 2 int)
                (explicit 3 utf8_string)))

  let exec =
    let f = function
      | `C1 (job, uuid, out, (created, finished), res, data) ->
        job, uuid, out, created, finished, res, data
      | `C2 () -> assert false
    and g (job, uuid, out, created, finished, res, data) =
      `C1 (job, uuid, out, (created, finished), res, data)
    in
    Asn.S.(map f g
             (choice2
                (explicit 0
                   (sequence6
                      (required ~label:"job" script_job)
                      (required ~label:"uuid" uuid)
                      (required ~label:"console"
                         (sequence_of (sequence2
                                         (required ~label:"delta" int)
                                         (required ~label:"data" utf8_string))))
                      (required ~label:"timestamps"
                         (sequence2
                            (required ~label:"started" utc_time)
                            (required ~label:"finished" utc_time)))
                      (required ~label:"result" res)
                      (required ~label:"output" data)))
                (explicit 1 null)))

  let exec_of_cs, exec_to_cs = projections_of exec

  let client_or_worker =
    let f = function
      | `C1 () -> `Client
      | `C2 () -> `Worker
    and g = function
      | `Client -> `C1 ()
      | `Worker -> `C2 ()
    in
    Asn.S.(map f g @@ choice2
             (explicit 0 null)
             (explicit 1 null))

  let cmd =
    let f = function
      | `C1 `C1 max -> Client_hello max
      | `C1 `C2 max -> Server_hello max
      | `C1 `C3 (uuid, job) -> Job_schedule (uuid, job)
      | `C1 `C4 (uuid, res, data) -> Job_finished (uuid, res, data)
      | `C1 `C5 (uuid, out) -> Output (uuid, out)
      | `C1 `C6 (period, job) -> Schedule (period, job)
      | `C2 `C1 () -> Info
      | `C2 `C2 (schedule, queue, running) ->
        Info_reply { schedule ; queue ; running }
      | `C2 `C3 () -> Job_requested
      | `C2 `C4 jn -> Unschedule jn
      | `C2 `C5 id -> Observe id
      | `C2 `C6 jn -> Execute jn
      | `C3 `C1 (period, orb_job) -> Schedule_orb_build (period, orb_job)
      | `C3 `C2 (name, next, period) -> Reschedule (name, next, period)
      | `C3 `C3 (t, n) -> Client_hello2 (t, n)
      | `C3 `C4 () -> Server_hello2
    and g = function
      | Client_hello max -> `C1 (`C1 max)
      | Server_hello max -> `C1 (`C2 max)
      | Job_schedule (uuid, job) -> `C1 (`C3 (uuid, job))
      | Job_finished (uuid, res, data) -> `C1 (`C4 (uuid, res, data))
      | Output (uuid, out) -> `C1 (`C5 (uuid, out))
      | Schedule (period, job) -> `C1 (`C6 (period, job))
      | Info -> `C2 (`C1 ())
      | Info_reply { schedule ; queue ; running } ->
        `C2 (`C2 (schedule, queue, running))
      | Job_requested -> `C2 (`C3 ())
      | Unschedule jn -> `C2 (`C4 jn)
      | Observe id -> `C2 (`C5 id)
      | Execute jn -> `C2 (`C6 jn)
      | Schedule_orb_build (period, orb_job) -> `C3 (`C1 (period, orb_job))
      | Reschedule (name, next, period) -> `C3 (`C2 (name, next, period))
      | Client_hello2 (t, n) -> `C3 (`C3 (t, n))
      | Server_hello2 -> `C3 (`C4 ())
    in
    Asn.S.(map f g
             (choice3
                (choice6
                   (explicit 0 int)
                   (explicit 1 int)
                   (explicit 2 (sequence2
                                  (required ~label:"uuid" uuid)
                                  (required ~label:"job" script_job)))
                   (explicit 3 (sequence3
                                  (required ~label:"uuid" uuid)
                                  (required ~label:"result" res)
                                  (required ~label:"data" data)))
                   (explicit 4 (sequence2
                                  (required ~label:"uuid" uuid)
                                  (required ~label:"output" utf8_string)))
                   (explicit 5 (sequence2
                                  (required ~label:"period" period)
                                  (required ~label:"job" script_job))))
                (choice6
                   (explicit 6 null)
                   (explicit 7 (sequence3
                                  (required ~label:"schedule" (sequence_of schedule))
                                  (required ~label:"queue" (sequence_of job))
                                  (required ~label:"running"
                                     (sequence_of
                                        (sequence3
                                           (required ~label:"started" utc_time)
                                           (required ~label:"uuid" uuid)
                                           (required ~label:"job" script_job))))))
                   (explicit 8 null)
                   (explicit 9 utf8_string)
                   (explicit 10 uuid)
                   (explicit 11 utf8_string))
                (choice4
                   (explicit 12 (sequence2
                     (required ~label:"period" period)
                     (required ~label:"orb_build_job" orb_build_job)))
                   (explicit 13 (sequence3
                     (required ~label:"name" utf8_string)
                     (required ~label:"next" utc_time)
                     (optional ~label:"period" period)))
                   (explicit 14 (sequence2
                     (required ~label:"typ" client_or_worker)
                     (required ~label:"version" int)))
                   (explicit 15 null)
                )))

  let cmd_of_cs, cmd_to_cs = projections_of cmd
end

let rec ign_intr f v =
  try f v with Unix.Unix_error (Unix.EINTR, _, _) -> ign_intr f v

let read fd =
  try
    let rec r b ?(off = 0) l =
      if l = 0 then
        Ok ()
      else
        let read = ign_intr (Unix.read fd b off) l in
        if read = 0 then
          Error (`Msg "end of file")
        else
          r b ~off:(read + off) (l - read)
    in
    let bl = Bytes.create 8 in
    r bl 8 >>= fun () ->
    let l = Cstruct.BE.get_uint64 (Cstruct.of_bytes bl) 0 in
    let l_int = Int64.to_int l in (* TODO *)
    let b = Bytes.create l_int in
    r b l_int >>= fun () ->
    Ok (Cstruct.of_bytes b)
  with
    Unix.Unix_error (err, f, _) ->
    Log.err (fun m -> m "Unix error in %s: %s" f (Unix.error_message err));
    Error (`Msg "unix error in read")

let read_cmd fd =
  read fd >>= fun data ->
  Asn.cmd_of_cs data

let write fd data =
  try
    let rec w b ?(off = 0) l =
      if l = 0 then
        ()
      else
        let written = ign_intr (Unix.write fd b off) l in
        w b ~off:(written + off) (l - written)
    in
    let csl = Cstruct.create 8 in
    Cstruct.BE.set_uint64 csl 0 (Int64.of_int (Cstruct.length data));
    w (Cstruct.to_bytes csl) 8;
    w (Cstruct.to_bytes data) (Cstruct.length data);
    Ok ()
  with
    Unix.Unix_error (err, f, _) ->
    Log.err (fun m -> m "Unix error in %s: %s" f (Unix.error_message err));
    Error (`Msg "unix error in write")

let write_cmd fd cmd =
  let data = Asn.cmd_to_cs cmd in
  write fd data
