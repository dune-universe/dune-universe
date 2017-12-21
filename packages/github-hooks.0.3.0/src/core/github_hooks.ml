(*  Copyright (c) 2013-2016 David Sheets <sheets@alum.mit.edu>   *)
(*  Copyright (c) 2016 Thomas Gazagnaire <thomas@gazagnaire.org> *)

open Cohttp
open Github_t
open Lwt.Infix

module Repo = struct
  type t = string * string

  module Set = Set.Make(struct
      type nonrec t = t
      let compare (user,repo) (user',repo') =
        match String.compare user user' with
        | 0 -> String.compare repo repo'
        | x -> x
    end)
end

type tls_config =
  [ `Crt_file_path of string ] *
  [ `Key_file_path of string ] *
  [ `Password of bool -> string | `No_password ] *
  [ `Port of int ]

module type CONFIGURATION = sig
  module Log : Logs.LOG
  val secret_prefix : string
  val tls_config : (int -> tls_config) option
end

module type TIME = sig
  type t
  val min : t
  val now : unit -> t
end

module type HOOKS = sig
  type t
  type token
  val create : token -> Uri.t -> t
  val run : t -> unit Lwt.t
  val repos : t -> Repo.Set.t
  val watch : t -> ?events:Github_t.event_type list -> Repo.t -> unit Lwt.t
  val events : t -> (Repo.t * Github_t.event_hook_constr) list
  val clear : t -> unit
  val wait : t -> unit Lwt.t
end

module type SERVER = sig
  include Cohttp_lwt.S.Server
  type mode = private [> `TCP of [`Port of int] | `TLS of tls_config]
  val create: mode -> t -> unit Lwt.t
end

module Make
    (Time: TIME)
    (Github: Github_s.Github)
    (Server: SERVER)
    (Conf: CONFIGURATION) =
struct
  module Log = Conf.Log

  type token = Github.Token.t

  module HTTP = struct
    type body = Cohttp_lwt.Body.t

    type response = Response.t * body

    type 'a handler = Server.conn -> Request.t -> body -> 'a Lwt.t

    type service = {
      name   : string;
      routes : Re.t;
      handler: response option handler;
    }

    type t = {
      port    : int;
      mutable services: service list;
      mutable dispatch: service handler;
    }

    let service_not_found s = {
      name    = "Default404";
      routes  = Re.any;
      handler = Lwt.(fun _id req _body ->
        let routes = List.map (fun s -> s.name) s in
        let body =
          Fmt.strf "404: Resource '%s' not found\nExisting services:\n%a"
            (Uri.path (Request.uri req))
            Fmt.(list ~sep:(unit "\n") string) routes
        in
        Server.respond_string ~status:`Not_found ~body ()
        >|= fun x -> Some x
      );
    }

    let make_dispatch services =
      let routes = List.map (fun s -> Re.compile s.routes, s) services in
      fun _id req _body ->
        Log.debug (fun l ->
          l "dispatch %a" Fmt.(Dump.list string)
            (List.map (fun (_, x) -> x.name) routes));
        let rec dispatch = function
          | []              -> service_not_found services
          | (rt, s) :: rest ->
            let path = Uri.path (Request.uri req) in
            let m = Re.execp rt path in
            if m then s else dispatch rest
        in
        Lwt.return (dispatch routes)

    let create port = { port; services=[]; dispatch = make_dispatch [] }

    let add_service t service =
      t.services <- service :: t.services;
      t.dispatch <- make_dispatch t.services

    let listen server =
      let port = server.port in
      let callback t conn_id req body =
        t.dispatch conn_id req body >>= fun service ->
        let pathquery = Uri.path_and_query (Request.uri req) in
        Log.debug (fun l ->
          l "%s for %s dispatched to %s"
            (Code.string_of_method (Request.meth req)) pathquery service.name);
        service.handler conn_id req body >>= function
        | None ->
          Log.err (fun l -> l "%s refused to service %s" service.name pathquery);
          (* FIXME: should be a better error *)
          Lwt.fail_with "listen"
        | Some resp -> Lwt.return resp
      in
      let conn_closed (_, conn_id) =
        Log.debug (fun l -> l "conn %s closed" (Connection.to_string conn_id))
      in
      let config = Server.make ~callback:(callback server) ~conn_closed () in
      match Conf.tls_config with
      | None         -> Server.create (`TCP (`Port port)) config
      | Some tls_fun -> Server.create (`TLS (tls_fun port)) config

    let service name ~routes ~handler = { name; routes; handler }

  end

  module Webhook = struct

    type status = Indicated | Pending | Timeout | Unauthorized | Connected

    type t = {
      id          : int64;
      url         : Uri.t;
      secret      : Cstruct.t;
      user        : string;
      repo        : string;
      update_event: unit Lwt_condition.t;
      token       : Github.Token.t;
      handler     : HTTP.response option HTTP.handler;
      mutable status    : status;
      mutable last_event: Time.t;
    }

    let () = Random.self_init ()

    let hmac ~secret message =
      Nocrypto.Hash.SHA1.hmac ~key:secret (Cstruct.of_string message)
      |> fun x -> Hex.of_cstruct x

    let verification_failure =
      let body = "403: Forbidden (Request verification failure)" in
      Server.respond_string ~status:`Forbidden ~body ()
      >|= fun x -> Some x

    let verify_event ~secret req body =
      match Header.get (Request.headers req) "x-hub-signature" with
      | None      -> Lwt.return_none
      | Some sign ->
        let prefix = "sha1=" in
        let prefix_len = String.length prefix in
        if String.sub sign 0 prefix_len = prefix
        then
          let hex = String.(sub sign prefix_len (length sign - prefix_len)) in
          Cohttp_lwt.Body.to_string body >|= fun body ->
          let `Hex hmac = hmac ~secret body in
          if String.compare hex hmac = 0 then Some body else None
        else Lwt.return_none

    let new_secret prefix =
      let `Hex s = Hex.of_cstruct (Nocrypto.Rng.generate 20) in
      Cstruct.of_string (prefix ^ ":" ^ s)

    let default_events = [
      `Create; `Delete; `Push; (* ref updates *)
      `Status;                 (* status updates *)
      `PullRequest;            (* PR updates *)
    ]

    let new_hook ?(events=default_events) url secret =
      let insecure_ssl =
        match Conf.tls_config with None -> false | Some _ -> true
      in
      let new_hook_config = `Web {
        web_hook_config_url          = Uri.to_string url;
        web_hook_config_content_type = Some "json";
        web_hook_config_insecure_ssl = insecure_ssl;
        web_hook_config_secret       = Some (Cstruct.to_string secret);
      }
      in {
        new_hook_active = true;
        new_hook_events = events;
        new_hook_config;
      }

    let web_hook_config h = match h.hook_config with
      | `Web w -> Some w
      | `Unknown (cons, _) ->
        Log.debug (fun l -> l "ignoring hook config for %s" cons);
        None

    let handler t id req body =
      verify_event ~secret:t.secret req body >>= function
      | None ->
        t.status <- Unauthorized;
        Log.err (fun l ->
          l "FAILURE: Hook verification of %s for %s/%s"
            (Uri.to_string t.url) t.user t.repo);
        Lwt_condition.broadcast t.update_event ();
        verification_failure
      | Some body ->
        t.last_event <- Time.now ();
        t.status <- Connected;
        Lwt_condition.broadcast t.update_event ();
        Lwt.async (fun () ->
          t.handler id req (`String body) >|= function
          | None   -> ()
          | Some _ -> ());
        Server.respond_string ~status:`No_content ~body:"" ()
        >|= fun x -> Some x

    let of_hook ~token ((user, repo), handler) hook secret =
      match web_hook_config hook  with
      | None   -> assert false
      | Some w -> {
          id           = hook.hook_id;
          url          = Uri.of_string w.web_hook_config_url;
          status       = Indicated;
          update_event = Lwt_condition.create ();
          last_event   = Time.min;
          handler; token; secret; user; repo;
        }

    let test ~token t =
      let open Github.Monad in
      let f =
        Github.Repo.Hook.test ~token ~user:t.user ~repo:t.repo
          ~id:t.id ()
        |> map ignore
      in
      run f

    let register registry t = Hashtbl.replace registry (Uri.path t.url) t

    let check_connectivity t timeout_s =
      let timeout () =
        Lwt_unix.sleep timeout_s >>= fun () ->
        t.status <- Timeout;
        Lwt_condition.broadcast t.update_event ();
        Lwt.return ()
      in
      let rec wait () =
        Log.debug (fun l -> l "wait %Ld" t.id);
        Lwt_condition.wait t.update_event >>= fun () ->
        Log.debug (fun l -> l "after-wait %Ld" t.id);
        if t.status = Connected then Lwt.return_unit
        else wait ()
      in
      Lwt.pick [ timeout (); wait () ]

    let connect ~token ?events registry url ((user, repo), _ as r) =
      let points_to_us h =
        match web_hook_config h with
        | None   -> false
        | Some w -> w.web_hook_config_url = Uri.to_string url
      in
      let create =
        let open Github.Monad in
        Github.Repo.Hook.for_repo ~token ~user ~repo ()
        |> Github.Stream.to_list
        >>= fun hooks ->
        List.fold_left (fun m h ->
          m >>= fun () ->
          if not (points_to_us h) then return () else
            let id = h.hook_id in
            Log.info (fun l -> l "Github.Hook.delete %s/%s/%Ld" user repo id);
            Github.Repo.Hook.delete ~token ~user ~repo ~id ()
            |> map Github.Response.value
        ) (return ()) hooks
        >>= fun () ->
        let secret = new_secret Conf.secret_prefix in
        let hook = new_hook ?events url secret in
        Log.info (fun l ->
          l "Github.Hook.create %s/%s (%s)" user repo @@ Uri.to_string url);
        Github.Repo.Hook.create ~token ~user ~repo ~hook () >>~ fun hook ->
        of_hook ~token r hook secret
        |> return
      in
      Github.Monad.run create >>= fun t ->
      t.status <- Pending;
      register registry t;
      Lwt.join [
        check_connectivity t 10.;
        test ~token t;
      ] >|= fun () ->
      t

  end

  type s = {
    uri     : Uri.t;
    registry: (string, Webhook.t) Hashtbl.t;
    token   : Github.Token.t;
    mutable repos: Repo.Set.t
  }

  type t = {
    s: s;
    mutable events: ((string * string) * Github_t.event_hook_constr) list;
    http: HTTP.t;
    cond: unit Lwt_condition.t;
  }

  let empty token uri =
    let registry = Hashtbl.create 10 in
    let repos = Repo.Set.empty in
    { uri; registry; token; repos }

  let github_error_str (user,repo) =
    Fmt.strf "GitHub connection for %s/%s failed:" user repo

  let event_type req = Header.get (Request.headers req) "x-github-event"

  let pp_event ppf : Github_t.event_hook_constr -> _ = function
    | `Push _       -> Fmt.string ppf "push"
    | `Status _      -> Fmt.string ppf "status"
    | `Delete _      -> Fmt.string ppf "delete"
    | `Create _      -> Fmt.string ppf "create"
    | `PullRequest _ -> Fmt.string ppf "pull-request"
    | `Member _      -> Fmt.string ppf "member"
    | `Issues _      -> Fmt.string ppf "issues"
    | `Follow        -> Fmt.string ppf "follow"
    | `Watch _       -> Fmt.string ppf "watch"
    | `Release _     -> Fmt.string ppf "release"
    | `PullRequestReviewComment _ ->
      Fmt.string ppf "pull-request-review-comment"
    | `Fork _        -> Fmt.string ppf "fork"
    | `Public        -> Fmt.string ppf "public"
    | `ForkApply     -> Fmt.string ppf "fork-apply"
    | `Download      -> Fmt.string ppf "download"
    | `Gist          -> Fmt.string ppf "gist"
    | `CommitComment _ -> Fmt.string ppf "commit-comment"
    | `IssueComment _ -> Fmt.string ppf "issue-comment"
    | `Gollum _      -> Fmt.string ppf "gollum"
    | `Repository _  -> Fmt.string ppf "repository"
    | `Unknown (cons, _) -> Fmt.pf ppf "unknown:%s" cons

  let notification_handler t (user, repo) _id req body =
    Cohttp_lwt.Body.to_string body >>= fun payload ->
    let e = match event_type req with
      | None        -> None
      | Some constr -> Some (Github.Repo.Hook.parse_event ~constr ~payload ())
    in
    match e with
    | Some e ->
      Log.info (fun l ->
        l "received webhook event for %s/%s: %a" user repo pp_event e);
      t.events <- ((user, repo), e) :: t.events;
      Lwt_condition.signal t.cond ();
      let body = Fmt.strf "Got event for %s/%s\n" user repo in
      Server.respond_string ~status:`OK ~body ()
      >|= fun x -> Some x
    | None ->
      Log.info (fun l -> l "received unknown webhook event");
      Lwt.return_none

  let service registry uri service_fn =
    let root = Uri.path uri in
    let routes = Re.str root in
    let handler conn_id req body =
      let uri = Request.uri req in
      try
        let endpoint = Hashtbl.find registry (Uri.path uri) in
        Webhook.handler endpoint conn_id req body
      with Not_found ->
        Lwt.return_none
    in
    service_fn ~routes ~handler

  let (++) x y = Uri.resolve "" x (Uri.of_string y)

  let watch t ?events (user,repo) =
    if Repo.Set.mem (user,repo) t.s.repos then (
      Log.debug (fun l -> l "Already watching %s"
                    (String.concat " " (List.map (fun (user, repo) ->
                       Printf.sprintf "%s/%s" user repo
                     ) (Repo.Set.elements t.s.repos)))
                );
      Lwt.return_unit
    ) else
      let uri = t.s.uri ++ Printf.sprintf "%s/%s" user repo in
      Log.info (fun l ->
        l "Connecting GitHub to callback %s\n%!" (Uri.to_string uri));
      let service =
        let msg = Fmt.strf "GitHub listener for %s/%s" user repo in
        service t.s.registry uri (HTTP.service msg)
      in
      let err = github_error_str (user,repo) in
      HTTP.add_service t.http service;
      Webhook.connect ~token:t.s.token ?events t.s.registry uri
        ((user, repo), notification_handler t (user, repo))
      >|= fun endpoint -> match endpoint.Webhook.status with
      | Webhook.Indicated    -> Log.err (fun l -> l "%s wedged prerequest" err)
      | Webhook.Pending      -> Log.err (fun l -> l "%s wedged pending" err)
      | Webhook.Timeout      -> Log.err (fun l -> l "%s handshake timeout" err)
      | Webhook.Unauthorized -> Log.err (fun l -> l "%s authorization failed" err)
      | Webhook.Connected    ->
        Log.info (fun l -> l "%s/%s connected" user repo);
        t.s.repos <- Repo.Set.add (user, repo) t.s.repos

  let create token uri =
    let port = match Uri.port uri with None -> 80 | Some p -> p in
    let http = HTTP.create port in
    let s = empty token uri in
    let cond = Lwt_condition.create () in
    { s; http; events = []; cond }

  let repos t = t.s.repos
  let run t = HTTP.listen t.http
  let events t = List.rev t.events
  let clear t = t.events <- []
  let wait t = Lwt_condition.wait t.cond

end
